{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Language.Refal.PatternMatching (
  matchPatternApplicative,
  matchPatternRecursive,
  matchPatternState,
  matchPattern,
  module Types,
)
where

import qualified Language.Refal.PatternMatching.Implementations.Applicative as ImpApplicative
import qualified Language.Refal.PatternMatching.Implementations.Recursive as ImpRecursive
import qualified Language.Refal.PatternMatching.Implementations.State as ImpState
import Language.Refal.PatternMatching.Types as Types

matchPatternApplicative = ImpApplicative.matchPattern

matchPatternRecursive = ImpRecursive.matchPattern

matchPatternState = ImpState.matchPattern

-- default implementation
matchPattern = matchPatternState
