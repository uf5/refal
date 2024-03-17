module Language.Refal.PatternMatching (matchPattern, Substitution) where

import Control.Applicative
import Language.Refal.Types

type Substitution = (Var, ObjectExpression)

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern = matchPattern' []

matchPattern' :: [Substitution] -> [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern' subs [] [] = pure subs
matchPattern' subs ((PSym p) : ps) ((OSym o) : os) =
  if p == o
    then matchPattern' subs ps os
    else Nothing
matchPattern' subs ((PVar v@(Var S _)) : ps) (ox@(OSym _) : os) = case lookup v subs of
  Just defined ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' ((v, ox) : subs) ps os
matchPattern' subs ((PVar v@(Var T _)) : ps) (ox : os) = case lookup v subs of
  Just defined ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' ((v, ox) : subs) ps os
matchPattern' subs ((PVar v@(Var E _)) : ps) o@(ox : os) = case lookup v subs of
  (Just defined) ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  _ -> takeUntilNextMatches [] o
  where
    takeUntilNextMatches elts o' =
      matchPattern' ((v, OSt elts) : subs) ps o'
        <|> case o' of
          (ox' : os') -> takeUntilNextMatches (elts <> [ox']) os'
          [] -> pure [(v, OSt elts)]
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing
