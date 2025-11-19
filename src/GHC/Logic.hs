{-# LANGUAGE NamedFieldPuns #-}

module GHC.Logic (State(..), gdpEv, isGdpType) where

import Data.Foldable (find)
import Data.Maybe (isJust)
import GHC.TcPlugin.API
import GHC.Plugins (($$), (<+>))
import qualified GHC.Plugins as GHC


gdpEv :: State -> Type -> Type -> Maybe Coercion
gdpEv state lhs rhs
  | isGdpType state lhs && solve state lhs rhs
      = Just $ mkPluginUnivCo "GDP Evidence" Nominal [] lhs rhs
  | otherwise
      = Nothing


-- | Determine whether lhs implies rhs
solve :: State -> Type -> Type -> Bool
solve State{gdpAnd, gdpOr, gdpNot} lhs rhs =
  case splitTyConApp_maybe lhs of
    Just (tyCon, [_, _, lhs', rhs']) | tyCon == gdpAnd -> eqType lhs' rhs || eqType rhs' rhs
                                     | tyCon == gdpOr  -> False
    Just (tyCon, _p)                 | tyCon == gdpNot -> undefined
    Nothing -> lhs `eqType` rhs
    _ -> False


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
