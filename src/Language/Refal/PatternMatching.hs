{-# LANGUAGE LambdaCase #-}

module Language.Refal.PatternMatching (matchPattern, Substitution) where

import Control.Applicative
import Language.Refal.Types

type Substitution = (Var, ObjectExpression)

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern = matchPattern' []

matchPattern' :: [Substitution] -> [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern' subs [] = \case
  [] -> pure subs
  _ -> Nothing
matchPattern' subs ((PSym p) : ps) = \case
  ((OSym o) : os) ->
    if p == o
      then matchPattern' subs ps os
      else Nothing
  _ -> Nothing
matchPattern' subs ((PVar v@(Var S _)) : ps) = \case
  (ox@(OSym _) : os) -> case lookup v subs of
    Just defined ->
      if ox == defined
        then matchPattern' subs ps os
        else Nothing
    Nothing -> matchPattern' ((v, ox) : subs) ps os
  _ -> Nothing
matchPattern' subs ((PVar v@(Var T _)) : ps) = \case
  (ox : os) -> case lookup v subs of
    Just defined ->
      if ox == defined
        then matchPattern' subs ps os
        else Nothing
    Nothing -> matchPattern' ((v, ox) : subs) ps os
  _ -> Nothing
matchPattern' subs ((PVar v@(Var E _)) : ps) = \case
  o@(ox : os) -> case lookup v subs of
    (Just defined) ->
      if ox == defined
        then matchPattern' subs ps os
        else Nothing
    _ -> takeUntilNextMatches [] o
  _ -> Nothing
  where
    takeUntilNextMatches elts o =
      matchPattern' ((v, OSt elts) : subs) ps o
        <|> case o of
          (ox : os) -> takeUntilNextMatches (elts <> [ox]) os
          [] -> pure [(v, OSt elts)]
matchPattern' subs ((PSt p) : ps) = \case
  ((OSt o) : os) -> do
    subs' <- matchPattern' subs p o
    matchPattern' subs' ps os
  _ -> Nothing
