module Language.Refal.PatternMatching (matchPattern, Substitution) where

import Control.Applicative
import Data.Bifunctor (second)
import Data.List
import Data.Semigroup (cycle1)
import Language.Refal.Types

type Substitution = (Var, ObjectExpression)

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern p o = subs2subs <$> matchPattern' emptySubs p o
  where
    subs2subs Substitution' {stType = s, eType = e} = s <> map (second OSt) e
    emptySubs = Substitution' {stType = [], eType = []}

data Substitution' = Substitution'
  { stType :: [(Var, ObjectExpression)],
    eType :: [(Var, [ObjectExpression])]
  }

consStType :: Var -> ObjectExpression -> Substitution' -> Substitution'
consStType v x subs = (subs {stType = (v, x) : stType subs})

consEType :: Var -> [ObjectExpression] -> Substitution' -> Substitution'
consEType v x subs = (subs {eType = (v, x) : eType subs})

matchPattern' :: Substitution' -> [PatternExpression] -> [ObjectExpression] -> Maybe Substitution'
matchPattern' subs [] [] = pure subs
matchPattern' subs ((PSym p) : ps) ((OSym o) : os) =
  if p == o
    then matchPattern' subs ps os
    else Nothing
matchPattern' subs ((PVar v@(Var S _)) : ps) (ox@(OSym _) : os) = case lookup v (stType subs) of
  Just defined ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' (consStType v ox subs) ps os
matchPattern' subs ((PVar v@(Var T _)) : ps) (ox : os) = case lookup v (stType subs) of
  Just defined ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' (consStType v ox subs) ps os
matchPattern' subs (px@(PVar v@(Var E _)) : ps) o =
  case lookup v (eType subs) of
    (Just eltsDefined) ->
      let (elts, rest) = splitAt (length eltsDefined) o
       in if eltsDefined == elts
            then matchPattern' subs ps rest
            else undefined
    Nothing -> repeating <|> takeUntilNextMatches o
  where
    takeUntilNextMatches = takeUntilNextMatches' []
    takeUntilNextMatches' elts o' =
      matchPattern' (consEType v elts subs) ps o'
        <|> case o' of
          (ox' : os') -> takeUntilNextMatches' (elts <> [ox']) os'
          [] -> pure $ consEType v elts subs
    repeating =
      if nNeighboring > 1
        then firstJust (map matchWithSegmentLength [0 .. (length o `div` nNeighboring)])
        else Nothing
    matchWithSegmentLength seglen = do
      let (sub, restO) = splitAt (nNeighboring * seglen) o
      let seg = take seglen sub
      if isValidSub sub seglen
        then matchPattern' (consEType v seg subs) restP restO
        else Nothing
    nNeighboring = 1 + length (takeWhile (== px) ps)
    restP = dropWhile (== px) ps
    isValidSub sub seglen = and $ zipWith (==) sub (cycle1 (take seglen sub))
    firstJust = foldl' (<|>) Nothing
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing
