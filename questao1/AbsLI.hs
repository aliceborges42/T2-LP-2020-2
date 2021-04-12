-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AbsLI where

import Prelude (Char, Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

data Program = Prog Stm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stm
    = SAss Ident Exp | SBlock [Stm] | SWhile Exp Stm | SdoWhile Stm Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EAdd Exp Exp
    | ESub Exp Exp
    | EMul Exp Exp
    | EDiv Exp Exp
    | EInt Integer
    | EVar Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

