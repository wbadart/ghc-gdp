{-# LANGUAGE DataKinds #-}

module GHC.GDP (plugin) where

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
  tcPluginTrace "----- GIVENS  -----" (ppr givens)
  tcPluginTrace "----- WANTEDS -----" (ppr wanteds)

  let wanted = head wanteds
  tcPluginTrace "Wanted detail:" (pprCt wanted)
  let
    solved =
      [ (uncurry (mkPluginUnivEvTerm "GDP TERM" Nominal []) (unsafeOperands wanted), wanted)
      ]
    new = []
  pure $ TcPluginOk solved new

pprCt :: Ct -> SDoc
pprCt ct = case classifyPredType (ctPred ct) of
  ClassPred{} -> GHC.text "ClassPred"
  EqPred role lhs rhs ->
    GHC.text "EqPred"
    $$ (GHC.text "role" <+> ppr role)
    $$ (GHC.text "lhs " <+> ppr lhs)
    $$ (GHC.text "rhs " <+> ppr rhs)
  IrredPred{} -> GHC.text "IrredPred"
  ForAllPred{} -> GHC.text "ForAllPred"

unsafeOperands :: Ct -> (Type, Type)
unsafeOperands ct = case classifyPredType (ctPred ct) of
  EqPred NomEq lhs rhs -> (lhs, rhs)
  _ -> error "not a nominal equality"


-- Type family rewriting
-- ==========

rewrite :: State -> UniqFM TyCon TcPluginRewriter
rewrite _ = emptyUFM
