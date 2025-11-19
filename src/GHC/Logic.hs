{-# LANGUAGE NamedFieldPuns #-}

module GHC.Logic (State(..), gdpEv, isGdpType) where

import Data.Foldable (find)
import Data.Maybe (isJust)
import GHC.TcPlugin.API
import GHC.Plugins (($$), (<+>))
import qualified GHC.Plugins as GHC

import Debug.Trace


gdpEv :: State -> Type -> Type -> Maybe Coercion
gdpEv state lhs rhs
  | isGdpType state lhs && solve state lhs rhs
      = Just $ mkPluginUnivCo "GDP Evidence" Nominal [] lhs rhs
  | otherwise
      = Nothing


-- | Determine whether lhs implies rhs
solve :: State -> Type -> Type -> Bool
solve state@State{gdpAnd, gdpOr, gdpNot} lhs rhs =
  let decomp = splitTyConApp_maybe lhs
  in case traceShow (GHC.showPprUnsafe decomp) decomp of
    Just (tyCon, [_, _, lhs', rhs'])
      | tyCon == gdpAnd -> solve state lhs' rhs || solve state rhs' rhs
      | tyCon == gdpOr  -> solve state lhs' rhs && solve state rhs' rhs

    Just (tyCon, [_, _p])
      | tyCon == gdpNot -> undefined

    Just (lhs', []) -> mkTyConTy lhs' `eqType` rhs
    Nothing         -> lhs `eqType` rhs
    _               -> False


-- Helpers
-- ==========

data State = State
  { gdpAnd :: TyCon
  , gdpOr  :: TyCon
  , gdpNot :: TyCon
  }


instance Outputable State where
  ppr State{gdpAnd, gdpOr, gdpNot} =
    GHC.empty
    $$ (GHC.text "AND:" <+> ppr gdpAnd)
    $$ (GHC.text "OR: " <+> ppr gdpOr)
    $$ (GHC.text "NOT:" <+> ppr gdpNot)


isGdpType :: State -> Type -> Bool
isGdpType = isJust ... classifyTyCon


classifyTyCon :: State -> Type -> Maybe TyCon
classifyTyCon state t = find ((== tyConAppTyCon_maybe t) . pure) (gdpTyCons state)


gdpTyCons :: State -> [TyCon]
gdpTyCons State{gdpAnd, gdpOr, gdpNot} = [gdpAnd, gdpOr, gdpNot]


(...) :: (c -> r) -> (a -> b -> c) -> a -> b -> r
(...) = (.) . (.)
