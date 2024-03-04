{-# LANGUAGE LambdaCase #-}

module Language.Refal.PatternMatching (Substitutions, mkPOP) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Void (Void)
import Language.Refal.Types
import Text.Megaparsec

type Substitutions = [(Var, ObjectExpression)]

type POP = StateT Substitutions (ParsecT Void [ObjectExpression] Identity)

lookupVar :: Monad m => Var -> StateT Substitutions m (Maybe ObjectExpression)
lookupVar = gets . lookup

putVar :: Monad m => Var -> ObjectExpression -> StateT Substitutions m ()
putVar v o = modify ((v, o) :)

mkPOP :: [PatternExpression] -> POP ()
mkPOP [] = eof
mkPOP ((PSym psym) : xs) = satisfy p >> mkPOP xs
  where
    p (OSym osym) = psym == osym
    p _ = False
mkPOP ((PVar v@(Var S _)) : xs) =
  ( lookupVar v
      >>= maybe
        (satisfy isOSym >>= putVar v)
        (\o1 -> void $ satisfy $ \o2 -> isOSym o2 && o1 == o2)
  )
    >> mkPOP xs
  where
    isOSym (OSym _) = True
    isOSym _ = False
mkPOP ((PVar v@(Var T _)) : xs) =
  ( lookupVar v
      >>= maybe
        (anySingle >>= putVar v)
        (void . satisfy . (==))
  )
    >> mkPOP xs
mkPOP ((PVar v@(Var E _)) : xs) =
  ( lookupVar v
      >>= maybe
        ( case xs of
            [] -> takeRest >>= putVar v . OSt
            (x' : xs') -> undefined
        )
        ( \case
            (OSt elts) -> mapM_ (satisfy . (==)) elts
            _ -> error "An e-type variable was bound to a value, with a constructor other than OSt"
        )
  )
    >> mkPOP xs
mkPOP ((PSt elts) : xs) = undefined
