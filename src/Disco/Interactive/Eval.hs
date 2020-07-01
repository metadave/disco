-----------------------------------------------------------------------------
-- |
-- Module      :  Disco.Interactive.Eval
-- Copyright   :  disco team and contributors
-- Maintainer  :  byorgey@gmail.com
--
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Evaluation for things entered at an interactive disco prompt.
--
-----------------------------------------------------------------------------

module Disco.Interactive.Eval where

import           System.Console.Haskeline                as H
import           Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)

import           Control.Arrow                           ((&&&))
import           Control.Lens                            (use, (%=), (.=))
import           Control.Monad.Except
import           Data.Coerce
import qualified Data.Map                                as M
import           System.FilePath                         (splitFileName)

import           Unbound.Generics.LocallyNameless

import           Disco.AST.Surface
import           Disco.AST.Typed
import           Disco.Compile
import           Disco.Context
import           Disco.Desugar
import           Disco.Eval
import           Disco.Extensions
import           Disco.Interactive.Commands
import           Disco.Interactive.Parser
import           Disco.Interpret.Core
import           Disco.Module
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Typecheck.Monad
import           Disco.Types


------------------------------------------------------------

import qualified Data.IntMap                             as IM
import           Data.List                               (intercalate)

showVal :: Int -> Value -> String
showVal 0 _            = "_"
showVal _ (VNum _ r)   = show r
showVal k (VCons i vs) = "K" ++ show i ++ " [" ++ intercalate ", " (map (showVal (k-1)) vs) ++ "]"
showVal _ (VConst op)  = show op
showVal _ (VClos {})   = "<closure>"
showVal _ (VPAp {} )   = "<pap>"
showVal _ (VThunk {})  = "<thunk>"
showVal _ (VIndir l)   = "-> " ++ show l
showVal _ (VFun_ {})   = "<fun>"
showVal _ (VDelay_ {}) = "<delay>"
showVal _ (VBag {})    = "<bag>"
showVal _ (VType {})   = "<type>"

printMem :: Disco IErr ()
printMem = do
  env <- use topEnv
  mem <- use memory

  io $ print env

  forM_ (IM.assocs mem) $ \(k, Cell v _) ->
    io $ putStrLn $ show k ++ ": " ++ showVal 3 v

handleCMD :: String -> Disco IErr ()
handleCMD "" = return ()
handleCMD s = do
    exts <- use enabledExts
    case (parseLine discoCommands exts s) of
      Left msg -> io $ putStrLn msg
      Right l -> handleLine l `catchError` (io . print  {- XXX pretty-print error -})
  where
    handleLine :: REPLExpr -> Disco IErr ()
    handleLine r = execCommand r
    -- handleLine (Using e)     = enabledExts %= addExtension e
    -- handleLine (Let x t)     = handleLet x t
    -- handleLine (Eval t)      = evalTerm t
    -- handleLine (Import m)    = handleImport m
    -- handleLine Nop           = return ()


