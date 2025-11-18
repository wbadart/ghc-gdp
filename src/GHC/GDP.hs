{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module GHC.GDP (plugin) where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import GHC.TcPlugin.API
import GHC.Plugins (($$), (<+>))
import qualified GHC.Plugins as GHC

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
  { GHC.tcPlugin = Just . mkTcPlugin . tcPlugin
  , GHC.pluginRecompile = GHC.purePlugin
  }


-- ==========

type State = ()


tcPlugin :: [String] -> GHC.TcPlugin.API.TcPlugin
tcPlugin _args = TcPlugin
  { tcPluginSolve = solve
  , tcPluginRewrite = rewrite
  , tcPluginInit = pure ()
  , tcPluginStop = \_ -> pure ()
  }


-- Constraint solving
-- ==========

solve :: State -> [Ct] -> [Ct] -> TcPluginM 'Solve TcPluginSolveResult
solve _ givens wanteds = do
  tcPluginTrace "----- SOLVING -----" GHC.empty
  tcPluginTrace "----- GIVENS  -----" (GHC.text "\n" <+> ppr givens)
  tcPluginTrace "----- WANTEDS -----" $
    foldl' (\doc ct -> doc $$ (pprCt ct $$ GHC.text "----------")) GHC.empty wanteds
  let
    solved = mapMaybe solveGDP wanteds
    new    = []
  pure (TcPluginOk solved new)


solveGDP :: Ct -> Maybe (EvTerm, Ct)
solveGDP ct = case classifyCt ct of
  EqPred NomEq lhs rhs -> (, ct) <$> gdpEv lhs rhs
  _                    -> Nothing


gdpEv :: Type -> Type -> Maybe EvTerm
gdpEv = Just ... mkPluginUnivEvTerm "GDP Evidence" Nominal []


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
