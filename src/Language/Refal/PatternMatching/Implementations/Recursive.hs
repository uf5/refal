module Language.Refal.PatternMatching.Implementations.Recursive (matchPattern) where

import Control.Applicative.Combinators
import Language.Refal.BasisTypes
import Language.Refal.PatternMatching.Types

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern = matchPattern' mempty

matchPattern' :: Substitutions -> [PatternExpression] -> [ObjectExpression] -> Maybe Substitutions
matchPattern' subs [] [] = pure subs
matchPattern' subs ((PSym p) : ps) ((OSym o) : os)
  | p == o = matchPattern' subs ps os
  | otherwise = Nothing
matchPattern' subs@Substitutions {sType = ss} ((PVar (SType v)) : ps) ((OSym sx) : os) =
  case lookup v ss of
    Just defined
      | sx == defined -> matchPattern' subs ps os
      | otherwise -> Nothing
    Nothing -> matchPattern' (mempty {sType = [(v, sx)]} <> subs) ps os
matchPattern' subs@Substitutions {tType = ts} ((PVar (TType v)) : ps) (ox : os) =
  case lookup v ts of
    Just defined
      | ox == defined -> matchPattern' subs ps os
      | otherwise -> Nothing
    Nothing -> matchPattern' (mempty {tType = [(v, ox)]} <> subs) ps os
matchPattern' subs@Substitutions {eType = es} ((PVar (EType v)) : ps) o =
  case lookup v es of
    Just defined
      | let (elts, rest) = splitAt (length defined) o,
        elts == defined ->
          matchPattern' subs ps rest
      | otherwise -> Nothing
    Nothing -> choice $ map (withEVar . (`splitAt` o)) [0 .. length o]
  where
    withEVar (elts, rest) = matchPattern' (mempty {eType = [(v, elts)]} <> subs) ps rest
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing
