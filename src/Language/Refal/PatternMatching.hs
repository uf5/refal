module Language.Refal.PatternMatching (matchPattern, Substitutions (..)) where

import Control.Applicative.Combinators
import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import Language.Refal.BasisTypes

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern ps os = matchPattern' ps os `execStateT` mempty

data Substitutions = Substitutions
  { sType :: [(SVar, Symbol)],
    tType :: [(TVar, ObjectExpression)],
    eType :: [(EVar, [ObjectExpression])]
  }
  deriving (Show, Eq)

instance Semigroup Substitutions where
  Substitutions {sType = s1, tType = t1, eType = e1}
    <> Substitutions {sType = s2, tType = t2, eType = e2} =
      Substitutions {sType = s1 <> s2, tType = t1 <> t2, eType = e1 <> e2}

instance Monoid Substitutions where
  mempty = Substitutions {sType = mempty, tType = mempty, eType = mempty}

-- push substitution
pushSType :: SVar -> Symbol -> Substitutions -> Substitutions
pushSType v s ss@(Substitutions {sType = st}) = ss {sType = (v, s) : st}

pushTType :: TVar -> ObjectExpression -> Substitutions -> Substitutions
pushTType v t ss@(Substitutions {tType = tt}) = ss {tType = (v, t) : tt}

pushEType :: EVar -> [ObjectExpression] -> Substitutions -> Substitutions
pushEType v t ss@(Substitutions {eType = et}) = ss {eType = (v, t) : et}

type Matcher a = StateT Substitutions Maybe a

matchPattern' :: [PatternExpression] -> [ObjectExpression] -> Matcher ()
matchPattern' [] [] = pure ()
matchPattern' ((PSym p) : ps) ((OSym o) : os)
  | p == o = matchPattern' ps os
  | otherwise = lift Nothing
matchPattern' ((PSt p) : ps) ((OSt o) : os) = matchPattern' p o *> matchPattern' ps os
matchPattern' ((PVar (SType p)) : ps) ((OSym o) : os) =
  ( gets (lookup p . sType)
      >>= maybe
        (modify $ pushSType p o)
        ((`unless` lift Nothing) . (== o))
  )
    *> matchPattern' ps os
matchPattern' ((PVar (TType p)) : ps) (o : os) =
  ( gets (lookup p . tType)
      >>= maybe
        (modify $ pushTType p o)
        ((`unless` lift Nothing) . (== o))
  )
    *> matchPattern' ps os
matchPattern' ((PVar (EType p)) : ps) os =
  gets (lookup p . eType)
    >>= maybe
      (choice $ map (withEVar . (`splitAt` os)) [0 .. length os])
      (lift . (`List.stripPrefix` os) >=> matchPattern' ps)
  where
    withEVar (elts, rest) = modify (pushEType p elts) *> matchPattern' ps rest
matchPattern' _ _ = lift Nothing
