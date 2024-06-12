module Language.Refal.PatternMatching.Types (
  Substitutions (..),
  pushSType,
  pushTType,
  pushEType,
)
where

import Language.Refal.BasisTypes

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
