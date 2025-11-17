{-# LANGUAGE DataKinds #-}

module GHC.GDP (plugin) where

import qualified GHC.Plugins
import GHC.Utils.Outputable (($$))
import GHC.TcPlugin.API


plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin
  { GHC.Plugins.tcPlugin = Just . mkTcPlugin . tcPlugin
  , GHC.Plugins.pluginRecompile = GHC.Plugins.purePlugin
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

solve :: State -> [Ct] -> [Ct] -> TcPluginM 'Solve TcPluginSolveResult
solve () givens wanteds = do
  tcPluginTrace "----- SOLVING -----" (ppr givens $$ ppr wanteds)
  pure $ TcPluginOk [] []

rewrite :: State -> UniqFM TyCon TcPluginRewriter
rewrite () = emptyUFM
