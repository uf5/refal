module Language.Refal.Syntax (desugarProgram) where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, singleton)
import Data.Maybe (fromJust)
import qualified Language.Refal.BasisTypes as T
import qualified Language.Refal.ExtendedTypes as S

data DesugarEnv = DesugarEnv
  { viewField :: [T.Var],
    programField :: [String],
    nameFn :: Int -> String
  }

newtype DesugarState = DesugarState
  { auxFunctions :: [(String, T.RFunction)]
  }

type Desugar a = ReaderT DesugarEnv (StateT DesugarState Identity) a

runDesugar :: Desugar a -> DesugarEnv -> DesugarState -> (a, DesugarState)
runDesugar m env st = runIdentity (runStateT (runReaderT m env) st)

withViewField :: [T.Var] -> Desugar a -> Desugar a
withViewField v = local (\s -> s {viewField = v <> viewField s})

withNamePrefix :: String -> Desugar a -> Desugar a
withNamePrefix p = local (\s -> s {nameFn = ((p <> "-") <>) . nameFn s})

sentencePatternPrefix :: Desugar [T.PatternExpression]
sentencePatternPrefix = asks (map (T.PSt . singleton . T.PVar) . viewField)

callPrefix :: Desugar [T.ResultExpression]
callPrefix = asks (map (T.RSt . singleton . T.RVar) . viewField)

pushNewAuxFunction :: T.RFunction -> Desugar String
pushNewAuxFunction f = do
  n <- mkProgramFieldName
  modify $ \s -> s {auxFunctions = (n, f) : auxFunctions s}
  pure n

pushNewAuxFunctionReturnCallMaker :: T.RFunction -> Desugar ([T.ResultExpression] -> T.ResultExpression)
pushNewAuxFunctionReturnCallMaker f = do
  n <- pushNewAuxFunction f
  prefix <- callPrefix
  pure (T.RCall n . (prefix <>))

mkProgramFieldName :: Desugar String
mkProgramFieldName = do
  DesugarEnv {programField = pf, nameFn = nf} <- ask
  DesugarState {auxFunctions = af} <- get
  pure $ fromJust $ find (`notElem` (pf <> map fst af)) (map nf [1 ..])

mkFreeVar :: (String -> T.Var) -> Desugar T.Var
mkFreeVar constructor = do
  DesugarEnv {viewField = vf} <- ask
  pure $ fromJust $ find (`notElem` vf) (map (constructor . ("var-aux-" <>) . show) [1 :: Int ..])

mkFreeEVar :: Desugar T.Var
mkFreeEVar = mkFreeVar (T.EType . T.EVar)

desugarProgram :: S.Program -> T.Program
desugarProgram (S.Program prog) =
  let (prog', DesugarState {auxFunctions = aux}) =
        runDesugar
          (mapM desugarNameFunctionPair prog)
          initialEnv
          initialState
   in T.Program (prog' <> aux)
  where
    desugarNameFunctionPair (n, f) = withNamePrefix n $ do
      f' <- desugarFunction f
      pure (n, f')
    (functionNames, _) = unzip prog
    initialEnv =
      DesugarEnv
        { viewField = [],
          programField = functionNames,
          nameFn = ("aux-" <>) . show
        }
    initialState =
      DesugarState
        { auxFunctions = []
        }

desugarFunction :: S.Function -> Desugar T.RFunction
desugarFunction (S.Function sents) = do
  sents' <- desugarSentences sents
  pure $ T.RFunction sents'

desugarSentences :: [S.Sentence] -> Desugar [T.Sentence]
desugarSentences [] = pure []
desugarSentences ((S.Sentence p r) : rest) = do
  let p' = desugarPE p
  let r' = desugarRE r
  pPrefix <- sentencePatternPrefix
  let sentence = T.Sentence (pPrefix <> p') r'
  (sentence :) <$> desugarSentences rest
desugarSentences ((S.ClauseSentence p x f) : rest) = do
  pprefix <- sentencePatternPrefix
  let p' = desugarPE p
  let x' = desugarRE x
  -- Rest
  restCallMaker <- withNamePrefix "rest" $ do
    sents <- desugarSentences rest
    pushNewAuxFunctionReturnCallMaker (T.RFunction sents)
  -- Clause
  let patternVars = filterVars p
  clauseCallMaker <- withNamePrefix "clause" $ withViewField patternVars $ do
    (T.RFunction sents) <- desugarFunction f
    restArgsVar <- mkFreeEVar
    let restCallSentence = T.Sentence (pprefix <> [T.PVar restArgsVar]) [restCallMaker [T.RVar restArgsVar]]
    let clauseFunction = T.RFunction (sents <> [restCallSentence])
    pushNewAuxFunctionReturnCallMaker clauseFunction
  let clauseCallSentence = T.Sentence (pprefix <> p') [clauseCallMaker x']
  restArgsVar <- mkFreeEVar
  let restCallSentence = T.Sentence (pprefix <> [T.PVar restArgsVar]) [restCallMaker [T.RVar restArgsVar]]
  pure [clauseCallSentence, restCallSentence]
  where
    filterVars [] = []
    filterVars ((S.PVar v) : xs) = v : filterVars xs
    filterVars ((S.PSt elts) : xs) = filterVars elts <> filterVars xs
    filterVars (_ : xs) = filterVars xs

desugarPE :: [S.PatternExpression] -> [T.PatternExpression]
desugarPE = concatMap desugarPE'
  where
    desugarPE' (S.PSym s) =
      case s of
        S.SymBasic x -> [T.PSym x]
        S.SymQString x -> map (T.PSym . T.Char) x
    desugarPE' (S.PSt x) = [T.PSt (desugarPE x)]
    desugarPE' (S.PVar x) = [T.PVar x]

desugarRE :: [S.ResultExpression] -> [T.ResultExpression]
desugarRE = concatMap desugarRE'
  where
    desugarRE' (S.RSym s) =
      case s of
        S.SymBasic x -> [T.RSym x]
        S.SymQString x -> map (T.RSym . T.Char) x
    desugarRE' (S.RSt x) = [T.RSt (desugarRE x)]
    desugarRE' (S.RVar x) = [T.RVar x]
    desugarRE' (S.RCall name args) = [T.RCall name (desugarRE args)]
