module Language.Refal.PatternMatching.Implementations.State (matchPattern) where

import Control.Applicative.Combinators
import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import Language.Refal.BasisTypes
import Language.Refal.PatternMatching.Types

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern ps os = matchPattern' ps os `execStateT` mempty

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
