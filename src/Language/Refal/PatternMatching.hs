module Language.Refal.PatternMatching (matchPattern, Substitution) where

import Control.Applicative
import Data.List
import Data.Semigroup (cycle1)
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
matchPattern' subs (px@(PVar v@(Var E _)) : ps) o =
  case lookup v subs of
    (Just (OSt eltsDefined)) ->
      let (elts, rest) = splitAt (length eltsDefined) o
       in if eltsDefined == elts
            then matchPattern' subs ps rest
            else undefined
    (Just _) -> error "TODO: make illegal state unrepresentable"
    Nothing -> repeating <|> takeUntilNextMatches [] o
  where
    takeUntilNextMatches elts o' =
      matchPattern' ((v, OSt elts) : subs) ps o'
        <|> case o' of
          (ox' : os') -> takeUntilNextMatches (elts <> [ox']) os'
          [] -> pure [(v, OSt elts)]
    nNeighboring = 1 + length (takeWhile (== px) ps)
    restP = dropWhile (== px) ps
    isValidSub sub seglen = and $ zipWith (==) sub (cycle1 (take seglen sub))
    firstJust = foldl' (<|>) Nothing
    repeating =
      if nNeighboring > 1
        then firstJust (map rep [0 .. (length o `div` nNeighboring)])
        else Nothing
    rep seglen = do
      let (sub, restO) = splitAt (nNeighboring * seglen) o
      let seg = take seglen sub
      if isValidSub sub seglen
        then matchPattern' ((v, OSt seg) : subs) restP restO
        else Nothing
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing
