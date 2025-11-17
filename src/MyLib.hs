{-# LANGUAGE DataKinds #-}

module MyLib
( plugin
) where

import qualified GHC.Plugins
import GHC.TcPlugin.API
  ( Ct
  , TcPlugin(..)
  , TcPluginM
  , TcPluginRewriter
  , TcPluginSolveResult
  , TcPluginStage(..)
  , TyCon
  , UniqFM
  , mkTcPlugin
  )

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
solve _givens _wanteds = undefined

rewrite :: UniqFM TyCon TcPluginRewriter
rewrite = undefined
