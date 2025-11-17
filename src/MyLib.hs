{-# LANGUAGE DataKinds #-}

module MyLib
( plugin
) where

import qualified GHC.Plugins
import GHC.Utils.Outputable (($$))
import GHC.TcPlugin.API


plugin :: GHC.Plugins.Plugin
plugin = GHC.Plugins.defaultPlugin
  { GHC.Plugins.tcPlugin = Just . mkTcPlugin . tcPlugin
  }

tcPlugin :: [String] -> GHC.TcPlugin.API.TcPlugin
tcPlugin _args = TcPlugin
  { tcPluginSolve = \() -> solve
  , tcPluginRewrite = \() -> rewrite
  , tcPluginInit = pure ()
  , tcPluginStop = \_ -> pure ()
  }

solve :: [Ct] -> [Ct] -> TcPluginM 'Solve TcPluginSolveResult
solve givens wanteds = do
  tcPluginTrace "SOLVING" (ppr givens $$ ppr wanteds)
  pure $ TcPluginOk [] []

rewrite :: UniqFM TyCon TcPluginRewriter
rewrite = emptyUFM
