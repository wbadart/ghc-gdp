{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module GHC.GDP (plugin) where

import qualified GHC.Plugins as GHC
import GHC.TcPlugin.API
import GHC.Tc.Types.Constraint hiding (isGiven, isWanted)


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
  tcPluginTrace "Wanted detail:" (pprCt $ head wanteds)
  let
    solved =
      [ (mkPluginUnivEvTerm "GDP TERM" Nominal [] (error "lhs") (error "rhs"), head wanteds)
      ]
    new = []
  pure $ TcPluginOk solved new

pprCt :: Ct -> SDoc
pprCt = \case
  CEqCan{} -> GHC.text "CEqCan"
  CDictCan{} -> GHC.text "CDictCan"

  ct@CIrredCan{cc_ev=ev, cc_reason=reason} ->
    GHC.text "IrredCt"
    GHC.$$ (GHC.text "ev    " GHC.<+> ppr ev)
    GHC.$$ (GHC.text "reason" GHC.<+> ppr reason)
    GHC.$$ (GHC.text "extra " GHC.<+> ppr (ctOrigin ct))

  CQuantCan{} -> GHC.text "QCInst"
  CNonCanonical{} -> GHC.text "CtEvidence"


-- Type family rewriting
-- ==========

rewrite :: State -> UniqFM TyCon TcPluginRewriter
rewrite _ = emptyUFM
