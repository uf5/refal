module Language.Refal.Syntax (desugarProgram) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, singleton)
import Data.Maybe (fromJust)
import Language.Refal.Syntax.Types qualified as S
import Language.Refal.Types qualified as T

data DesugarEnv = DesugarEnv
  { viewField :: [T.Var],
    nameFn :: Int -> String
  }

data DesugarState = DesugarState
  { auxFunctions :: [(String, T.RFunction)],
    programField :: [String]
  }

type Desugar a = ReaderT DesugarEnv (StateT DesugarState Identity) a

runDesugar :: Desugar a -> DesugarEnv -> DesugarState -> (a, DesugarState)
runDesugar m env st = runIdentity (runStateT (runReaderT m env) st)

withViewField :: [T.Var] -> Desugar a -> Desugar a
withViewField v = local (\s -> s {viewField = v <> viewField s})

withNamePrefix :: String -> Desugar a -> Desugar a
withNamePrefix p = local (\s -> s {nameFn = ((p <> "-") <>) . nameFn s})

patternPrefix :: Desugar [T.PatternExpression]
patternPrefix = asks (map (T.PSt . singleton . T.PVar) . viewField)

resultPrefix :: Desugar [T.ResultExpression]
resultPrefix = asks (map (T.RSt . singleton . T.RVar) . viewField)

pushNewAuxFunction :: T.RFunction -> Desugar String
pushNewAuxFunction f = do
  n <- mkName
  modify $ \s -> s {auxFunctions = (n, f) : auxFunctions s, programField = n : programField s}
  pure n
  where
    mkName = do
      DesugarEnv {nameFn = nf} <- ask
      DesugarState {programField = pf} <- get
      pure $ fromJust $ find (`notElem` pf) (map nf [1 ..])

desugarProgram :: S.Program -> T.Program
desugarProgram (S.Program prog) =
  let (fs', DesugarState {auxFunctions = aux}) =
        runDesugar
          (mapM (\(n, f) -> withNamePrefix n $ desugarFunction f) prog)
          initialEnv
          initialState
      prog' = zip ns fs' <> aux
   in T.Program prog'
  where
    (ns, _) = unzip prog
    initialEnv =
      DesugarEnv
        { viewField = [],
          nameFn = ("aux-" <>) . show
        }
    initialState =
      DesugarState
        { auxFunctions = [],
          programField = ns
        }

desugarFunction :: S.Function -> Desugar T.RFunction
desugarFunction (S.Function ss) = do
  ss' <- desugarSentences ss
  pure $ T.RFunction ss'

desugarSentences :: [S.Sentence] -> Desugar [T.Sentence]
desugarSentences [] = pure []
desugarSentences ((S.Sentence p r) : rest) = do
  let p' = desugarPE p
  let r' = desugarRE r
  pPrefix <- patternPrefix
  rest' <- desugarSentences rest
  pure (T.Sentence (pPrefix <> p') r' : rest')
desugarSentences ((S.ClauseSentence p x f) : rest) = do
  let p' = desugarPE p
  let x' = desugarRE x
  rest' <- withNamePrefix "rest" (desugarSentences rest) >>= pushNewAuxFunction . T.RFunction
  (f', rPrefix) <-
    withNamePrefix
      "clause"
      ( withViewField patternVars $ do
          (T.RFunction sents) <- desugarFunction f
          rPrefix <- resultPrefix
          vfVars <- asks viewField
          let argsVar = fromJust $ find (`notElem` vfVars) (map (T.EType . T.EVar . ("args" <>) . show) [1 :: Int ..])
          let restSentence = T.Sentence [T.PVar argsVar] [T.RCall rest' [T.RVar argsVar]]
          pure (map (\(T.Sentence pp rr) -> T.Sentence pp rr) sents <> [restSentence], rPrefix)
      )
  fName <- pushNewAuxFunction (T.RFunction f')
  let fCall = T.RCall fName (rPrefix <> x')
  pure [T.Sentence p' [fCall]]
  where
    patternVars = filterVars p
    filterVars [] = []
    filterVars ((S.PVar v) : xs) = v : filterVars xs
    filterVars (_ : xs) = filterVars xs

desugarPE :: [S.PatternExpression] -> [T.PatternExpression]
desugarPE = concatMap desugarPE'
  where
    desugarPE' (S.PSym s) =
      case s of
        S.SymBasic x -> [T.PSym x]
        S.SymDQString x -> [T.PSt (map (T.PSym . T.Char) x)]
        S.SymSQString x -> map (T.PSym . T.Char) x
    desugarPE' (S.PSt x) = [T.PSt (desugarPE x)]
    desugarPE' (S.PVar x) = [T.PVar x]

desugarRE :: [S.ResultExpression] -> [T.ResultExpression]
desugarRE = concatMap desugarRE'
  where
    desugarRE' (S.RSym s) =
      case s of
        S.SymBasic x -> [T.RSym x]
        S.SymDQString x -> [T.RSt (map (T.RSym . T.Char) x)]
        S.SymSQString x -> map (T.RSym . T.Char) x
    desugarRE' (S.RSt x) = [T.RSt (desugarRE x)]
    desugarRE' (S.RVar x) = [T.RVar x]
    desugarRE' (S.RCall name args) = [T.RCall name (desugarRE args)]
