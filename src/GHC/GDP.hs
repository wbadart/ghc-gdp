{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

module GHC.GDP (plugin) where

import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import GHC.TcPlugin.API
import GHC.Plugins (($$), (<+>))
import qualified GHC.Plugins as GHC

import Logic.Propositional (And, Or, Not)


plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
  { GHC.tcPlugin = Just . mkTcPlugin . tcPlugin
  , GHC.pluginRecompile = GHC.purePlugin
  }


tcPlugin :: [String] -> GHC.TcPlugin.API.TcPlugin
tcPlugin _args = TcPlugin
  { tcPluginSolve = solve
  , tcPluginRewrite = rewrite
  , tcPluginInit = initPlugin
  , tcPluginStop = \_ -> pure ()
  }


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


initPlugin :: TcPluginM 'Init State
initPlugin = State
  <$> (lookupTHName ''And >>= tcLookupTyCon)
  <*> (lookupTHName ''Or  >>= tcLookupTyCon)
  <*> (lookupTHName ''Not >>= tcLookupTyCon)


-- Constraint solving
-- ==========

solve :: State -> [Ct] -> [Ct] -> TcPluginM 'Solve TcPluginSolveResult
solve state givens wanteds = do
  tcPluginTrace "----- SOLVING -----" GHC.empty
  tcPluginTrace "State:" (ppr state)
  tcPluginTrace "----- GIVENS  -----" (GHC.text "\n" <+> ppr givens)
  tcPluginTrace "----- WANTEDS -----" $
    foldl' (\doc ct -> doc $$ (pprCt ct $$ GHC.text "----------")) GHC.empty wanteds
  let
    solved = mapMaybe solveGDP wanteds
    new    = []
  pure (TcPluginOk solved new)


solveGDP :: Ct -> Maybe (EvTerm, Ct)
solveGDP ct = case classifyCt ct of
  EqPred NomEq lhs rhs -> gdpEv lhs rhs <&> \co -> (evCoercion co, ct)
  _                    -> Nothing


gdpEv :: Type -> Type -> Maybe Coercion
gdpEv = Just ... mkPluginUnivCo "GDP Evidence" Nominal []


pprCt :: Ct -> SDoc
pprCt ct = case classifyCt ct of
  ClassPred{} -> GHC.text "ClassPred"
  EqPred role lhs rhs ->
    GHC.text "EqPred"
    $$ (GHC.text "role" <+> ppr role)
    $$ (GHC.text "lhs " <+> ppr lhs)
    $$ (GHC.text "rhs " <+> ppr rhs)
  IrredPred{} -> GHC.text "IrredPred"
  ForAllPred{} -> GHC.text "ForAllPred"


classifyCt :: Ct -> Pred
classifyCt = classifyPredType . ctPred


-- Type family rewriting
-- ==========

rewrite :: State -> UniqFM TyCon TcPluginRewriter
rewrite _ = emptyUFM


-- Helpers
-- ==========

(...) :: (c -> r) -> (a -> b -> c) -> a -> b -> r
(...) = (.) . (.)
