-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Types
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
--
-----------------------------------------------------------------------------

module Disco.Interactive.Types
  ( REPLExpr(..), ReplCommand(..), ReplCommandType(..)
  ) where

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.Eval                       (Disco, IErr)
import           Disco.Extensions
import           Disco.Parser

------------------------------------------------------------
-- REPL expression type
------------------------------------------------------------

data REPLExpr =
   Using Ext                    -- Enable an extension
 | Let (Name Term) Term         -- Toplevel let-expression: for the REPL
 | TypeCheck Term               -- Typecheck a term
 | Eval Term                    -- Evaluate a term
 | ShowDefn (Name Term)         -- Show a variable's definition
 | Parse Term                   -- Show the parsed AST
 | Pretty Term                  -- Pretty-print a term
 | Ann Term                     -- Show type-annotated typechecked term
 | Desugar Term                 -- Show a desugared term
 | Compile Term                 -- Show a compiled term
 | Import String                -- Import a library module.
 | Load FilePath                -- Load a file.
 | Reload                       -- Reloads the most recently loaded file.
 | Doc (Name Term)              -- Show documentation.
 | Nop                          -- No-op, e.g. if the user just enters a comment
 | Help
 | Names
 deriving Show


------------------------------------------------------------
-- REPL command types
------------------------------------------------------------

data ReplCommandType =
    User
  | Dev
  deriving Show
------------------------------------------------------------
-- Commands
------------------------------------------------------------
data ReplCommand = ReplCommand
  { name  :: String 
  , shortHelp :: String
  , longHelp :: String
  , cmdType :: ReplCommandType
  , cmdAction :: String --REPLExpr -> Disco IErr ()
  , cmdParser :: Parser REPLExpr
  }