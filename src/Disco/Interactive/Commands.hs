-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Commands
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-----------------------------------------------------------------------------

module Disco.Interactive.Commands
  ( discoCommands
  ) where

import           Disco.Parser   (sc, ident, term)

import Disco.Interactive.Types
import Disco.Interactive.Parser

discoCommands :: [ReplCommand]
discoCommands = 
  [
--   ReplCommand {
--       name = "help",
--       shortHelp = "Show help",
--       longHelp = "Show help",
--       cmdType = User,
--       cmdAction = "foo",
--       cmdParser = return Help
--     },
    ReplCommand {
      name = "type",
      shortHelp = "Typecheck a term",
      longHelp = "Typecheck a term",
      cmdType = User,
      cmdAction = handleTypeCheck t        >>= iputStrLn
      cmdParser = TypeCheck <$> parseTypeTarget
    }
    -- ,
    -- ReplCommand {
    --   name = "names",
    --   shortHelp = "Show all names in current scope",
    --   longHelp = "Show all names in current scope",
    --   cmdType = User,
    --   cmdAction = "foo",
    --   cmdParser = return Names
    -- },
    -- ReplCommand {
    --   name = "defn",
    --   shortHelp = "",
    --   longHelp = "",
    --   cmdType = User,
    --   cmdAction = "foo",
    --   cmdParser = ShowDefn  <$> (sc *> ident)
    -- },
    -- ReplCommand {
    --   name = "parse",
    --   shortHelp = "",
    --   longHelp = "",
    --   cmdType = User,
    --   cmdAction = "foo",
    --   cmdParser = Parse <$> term
    -- },
    -- ReplCommand {
    --   name = "load",
    --   shortHelp = "",
    --   longHelp = "",
    --   cmdType = User,
    --   cmdAction = "foo",
    --   cmdParser = Load <$> fileParser
    -- }
    
  ]

    -- parsers =
    --   [ ("type",    TypeCheck <$> parseTypeTarget)
    --   , ("defn",    ShowDefn  <$> (sc *> ident))
    --   , ("parse",   Parse     <$> term)
    --   , ("pretty",  Pretty    <$> term)
    --   , ("ann",     Ann       <$> term)
    --   , ("desugar", Desugar   <$> term)
    --   , ("compile", Compile   <$> term)
    --   , ("load",    Load      <$> fileParser)
    --   , ("reload",  return Reload)
    --   , ("doc",     Doc       <$> (sc *> ident))
    --   , ("help",    return Help)
    --   , ("names",  return Names)
    --   ]
