module Language.Refal.PatternMatching (matchPattern, Substitution) where

import Control.Applicative
import Data.Bifunctor (second)
import Data.List
import Data.Semigroup (cycle1)
import Language.Refal.Types

type Substitution = (Var, ObjectExpression)

matchPattern :: [PatternExpression] -> [ObjectExpression] -> Maybe [Substitution]
matchPattern p o = flattenSubs <$> matchPattern' mempty p o
  where
    flattenSubs Substitution' {sType = s, tType = t, eType = e} = map (second OSym) s <> t <> map (second OSt) e

data Substitution' = Substitution'
  { sType :: [(Var, Symbol)],
    tType :: [(Var, ObjectExpression)],
    eType :: [(Var, [ObjectExpression])]
  }
  deriving (Show)

instance Semigroup Substitution' where
  Substitution' {sType = s1, tType = t1, eType = e1}
    <> Substitution' {sType = s2, tType = t2, eType = e2} =
      Substitution' {sType = s1 <> s2, tType = t1 <> t2, eType = e1 <> e2}

instance Monoid Substitution' where
  mempty = Substitution' {sType = mempty, tType = mempty, eType = mempty}

matchPattern' :: Substitution' -> [PatternExpression] -> [ObjectExpression] -> Maybe Substitution'
matchPattern' subs [] [] = pure subs
matchPattern' subs ((PSym p) : ps) ((OSym o) : os) =
  if p == o
    then matchPattern' subs ps os
    else Nothing
matchPattern' subs ((PVar v@(Var S _)) : ps) ((OSym sx) : os) = case lookup v (sType subs) of
  Just defined ->
    if sx == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' (mempty {sType = [(v, sx)]} <> subs) ps os
matchPattern' subs ((PVar v@(Var T _)) : ps) (ox : os) = case lookup v (tType subs) of
  Just defined ->
    if ox == defined
      then matchPattern' subs ps os
      else Nothing
  Nothing -> matchPattern' (mempty {tType = [(v, ox)]} <> subs) ps os
matchPattern' subs (px@(PVar v@(Var E _)) : ps) o =
  case lookup v (eType subs) of
    (Just eltsDefined) ->
      let (elts, rest) = splitAt (length eltsDefined) o
       in if eltsDefined == elts
            then matchPattern' subs ps rest
            else undefined
    Nothing -> repeating <|> takeUntilNextMatches o
  where
    -- \| find a substitutiton with the least amount of elements, so that the next pattern is statisfied.
    takeUntilNextMatches = takeUntilNextMatches' []
    takeUntilNextMatches' taken o' =
      let nextPattern = matchPattern' (mempty {eType = [(v, taken)]} <> subs) ps o'
       in nextPattern <|> case o' of
            (ox' : os') -> takeUntilNextMatches' (taken <> [ox']) os' -- try substitution of size + 1
            [] -> Nothing -- there is no E-Type substitution, such that the next pattern is satisfied

    -- \| a special case of >1 neighboring same-identifier E-Vars.
    -- finds a substitution that is equal for each neighboring same-identifier E-Var.
    -- example: pattern e.x e.x e.x would match 'abcabcabc' with final substitution being (e.x, 'abc')
    repeating =
      if nNeighboring > 1
        then firstJust (map matchWithSegmentLength [0 .. (length o `div` nNeighboring)])
        else Nothing
    matchWithSegmentLength seglen = do
      let (sub, restO) = splitAt (nNeighboring * seglen) o
      let seg = take seglen sub
      if isValidSub sub seglen
        then matchPattern' (mempty {eType = [(v, seg)]} <> subs) restP restO
        else Nothing
    nNeighboring = 1 + length (takeWhile (== px) ps)
    restP = dropWhile (== px) ps
    isValidSub sub seglen = and $ zipWith (==) sub (cycle1 (take seglen sub))
    firstJust = foldl' (<|>) Nothing
matchPattern' subs ((PSt p) : ps) ((OSt o) : os) = do
  subs' <- matchPattern' subs p o
  matchPattern' subs' ps os
matchPattern' _ _ _ = Nothing
