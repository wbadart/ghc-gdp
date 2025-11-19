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
main = do
  print $ needsThat proof1
  print $ needsThat proof2
  -- print $ needsThat notProof1
  print $ needsThat notProof2

-- ==========

data This
data That
data TheOther

proof1 :: Proof (This && That)
proof1 = axiom

proof2 :: Proof (That && This)
proof2 = axiom

notProof1 :: Proof TheOther
notProof1 = axiom

notProof2 :: Proof (This || That)
notProof2 = axiom

needsThat :: Proof That -> ()
needsThat _ = ()
