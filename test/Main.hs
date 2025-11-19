{-# OPTIONS_GHC -fplugin=GHC.GDP #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Logic.Proof
import Logic.Propositional

{----
 - The "test" of this test suite is simply whether or not the module compiles with the
 - negative tests (notProofN) commented out.
 -
 - The proofN cases have types I expect to be reducible to the proof type required by
 - needsX. The notProofN cases I do _not_ expect to be reducible to the needsX type.
 ----}

main :: IO ()
main = do
  print $ needsThat proof1
  print $ needsThat proof2
  print $ needsThat proof3
  print $ needsThat notProof1
  print $ needsThat notProof2

-- ==========

data This
data That
data TheOther

proof1 :: Proof (This && That)
proof1 = axiom

proof2 :: Proof (That && This)
proof2 = axiom

proof3 :: Proof (That || That)
proof3 = axiom

notProof1 :: Proof TheOther
notProof1 = axiom

notProof2 :: Proof (This || That)
notProof2 = axiom

needsThat :: Proof That -> ()
needsThat _ = ()
