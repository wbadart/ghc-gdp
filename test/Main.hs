{-# OPTIONS_GHC -fplugin=GHC.GDP #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Logic.Proof
import Logic.Propositional

{-
This tests whether the plugin's goal is met: to allow proof of a proposition to be used
in place of a proof that it entails. In this case, we have a proof of `This && That`,
which, naturally, entails `That`, which we try to provide to a function expecting proof
of `That`.
 -}

main :: IO ()
main = print $ needsThat proof

-- ==========

data This
data That

proof :: Proof (This && That)
proof = axiom

needsThat :: Proof That -> ()
needsThat _ = ()
