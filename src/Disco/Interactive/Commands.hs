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
  ( 
    discoCommands, 
    execCommand,
    handleLoad,
    loadFile,
    typeCheckCmd
  ) where

import           Disco.Parser   (sc, ident, term)


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
import           Disco.Interactive.Parser
import           Disco.Interactive.Types
import           Disco.Interpret.Core
import           Disco.Module
import           Disco.Pretty
import           Disco.Property
import           Disco.Typecheck
import           Disco.Typecheck.Erase
import           Disco.Typecheck.Monad
import           Disco.Types

-- TODO: ERROR handling :-)
execCommand :: REPLExpr -> Disco IErr ()
execCommand r = fn r
                where 
                    fn = cmdAction cmd
                    cmd = head $ filter (\t -> name t == tag) discoCommands
                    tag = toTag r


discoCommands :: [ReplCommand]
discoCommands = 
  [
    annCmd,
    compileCmd,
    desugarCmd,
    docCmd,
    helpCmd,
    -- importCmd
    --letCmd,
    loadCmd,
    namesCmd,
    -- nopCmd,
    parseCmd,
    reloadCmd,
    showDefnCmd,
    typeCheckCmd
    -- usingCmd
  ]

toTag :: REPLExpr -> String
toTag (TypeCheck _) = name typeCheckCmd
toTag (Let _ _ ) = "let"
toTag (Eval _) = "foo"
toTag (ShowDefn _ ) = name showDefnCmd
toTag (Parse _) = name parseCmd
toTag (Pretty _) = name prettyCmd
toTag (Ann _) = name annCmd
toTag (Desugar _) = name desugarCmd
toTag (Compile _) = name compileCmd
toTag (Import _) = "foo"
toTag (Load _) = name loadCmd
toTag Reload = name reloadCmd
toTag (Doc _) = name docCmd
toTag Nop = "foo"
toTag Help = name helpCmd
toTag Names = name namesCmd


    -- parsers =
    
    --   , ("parse",   Parse     <$> term)
    --   , ("pretty",  Pretty    <$> term)
    --   , ("ann",     Ann       <$> term)
    --   , ("desugar", Desugar   <$> term)
    --   , ("compile", Compile   <$> term)
    --   , ("load",    Load      <$> fileParser)
    --   , ("reload",  return Reload)
    --   , ("doc",     Doc       <$> (sc *> ident))
    --   , ("names",  return Names)
    --   ]

------------------------------------------
-- Commands
------------------------------------------

annCmd :: ReplCommand
annCmd = 
    ReplCommand {
      name = "ann",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleAnn,
      cmdParser = Ann <$> term
    }


compileCmd :: ReplCommand
compileCmd = 
    ReplCommand {
      name = "compile",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleCompile,
      cmdParser = Compile   <$> term
    }

desugarCmd :: ReplCommand
desugarCmd = 
    ReplCommand {
      name = "desugar",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleDesugar,
      cmdParser = Desugar   <$> term
    }

docCmd :: ReplCommand
docCmd = 
    ReplCommand {
      name = "doc",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleDoc,
      cmdParser = Doc <$> (sc *> ident)
    }


helpCmd :: ReplCommand
helpCmd = 
    ReplCommand {
      name = "help",
      shortHelp = "Show help",
      longHelp = "Show help",
      cmdType = User,
      cmdAction = handleHelp,
      cmdParser = return Help
    }

-- importCmd :: ReplCommand
-- importCmd = 
--     ReplCommand {
--       name = "import",
--       shortHelp = "",
--       longHelp = "",
--       cmdType = User,
--       cmdAction = handleImport,
--       cmdParser = return Help
--     }

-- letCmd :: ReplCommand
-- letCmd = 
--     ReplCommand {
--       name = "let",
--       shortHelp = "",
--       longHelp = "",
--       cmdType = User,
--       cmdAction = handleLet,
--       cmdParser = Load <$> fileParser
--     }

loadCmd :: ReplCommand
loadCmd = 
    ReplCommand {
      name = "load",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleLoadGen,
      cmdParser = Load <$> fileParser
    }

namesCmd :: ReplCommand
namesCmd =
    ReplCommand {
      name = "names",
      shortHelp = "Show all names in current scope",
      longHelp = "Show all names in current scope",
      cmdType = User,
      cmdAction = handleNames,
      cmdParser = return Names
    }

-- nopCmd :: ReplCommand
-- nopCmd =
--     ReplCommand {
--       name = "nop",
--       shortHelp = "",
--       longHelp = "",
--       cmdType = User,
--       cmdAction = return (),
--       cmdParser = return Names
--     }

parseCmd :: ReplCommand
parseCmd = 
    ReplCommand {
      name = "parse",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleParse,
      cmdParser = Parse <$> term
    }

prettyCmd :: ReplCommand
prettyCmd = 
    ReplCommand {
      name = "pretty",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handlePretty,
      cmdParser = Pretty <$> term
    }

reloadCmd :: ReplCommand
reloadCmd = 
    ReplCommand {
      name = "reload",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleReload,
      cmdParser = return Reload
    }

showDefnCmd :: ReplCommand
showDefnCmd = 
    ReplCommand {
      name = "defn",
      shortHelp = "",
      longHelp = "",
      cmdType = User,
      cmdAction = handleShowDefn,
      cmdParser = ShowDefn  <$> (sc *> ident)
    }

typeCheckCmd :: ReplCommand
typeCheckCmd = 
    ReplCommand {
        name = "type",
        shortHelp = "Typecheck a term",
        longHelp = "Typecheck a term",
        cmdType = User,
        cmdAction = handleTypeCheck,
        cmdParser = TypeCheck <$> parseTypeTarget
        }

-- usingCheckCmd :: ReplCommand
-- usingCheckCmd = 
--     ReplCommand {
--         name = "using",
--         shortHelp = "",
--         longHelp = "",
--         cmdType = User,
--         cmdAction = handleUsing,
--         cmdParser = 
--         }


------------------------------------------
--- Command implementations
------------------------------------------
-- handleAnn :: Term -> Disco IErr String
handleAnn :: REPLExpr -> Disco IErr ()
handleAnn (Ann t) = 
  handleAnn2 t >>= iputStrLn

-- TODO
handleAnn2 :: Term -> Disco IErr String
handleAnn2 t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e       -> return . show $ e
    Right (at,_) -> return . show $ at

handleCompile :: REPLExpr -> Disco IErr ()
handleCompile (Compile t) = 
  handleCompile2 t          >>= iputStrLn
  
-- TODO
handleCompile2 :: Term -> Disco IErr String
handleCompile2 t = do
  ctx <- use topCtx
  case evalTCM (extends ctx $ inferTop t) of
    Left e       -> return.show $ e
    Right (at,_) -> return.show.compileTerm $ at


handleDoc :: REPLExpr -> Disco IErr ()
handleDoc (Doc x) = 
  handleDocs x
  where 
    handleDocs :: Name Term -> Disco IErr ()
    handleDocs x = do
      ctx  <- use topCtx
      docs <- use topDocs
      case M.lookup x ctx of
        Nothing -> io . putStrLn $ "No documentation found for " ++ show x ++ "."
        Just ty -> do
          p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
          io . putStrLn $ p
          case M.lookup x docs of
            Just (DocString ss : _) -> io . putStrLn $ "\n" ++ unlines ss
            _                       -> return ()


handleHelp :: REPLExpr -> Disco IErr ()
handleHelp _ = iputStrLn "Help!"


-- | Parses, typechecks, and loads a module by first recursively loading any imported
--   modules by calling loadDiscoModule. If no errors are thrown, any tests present
--   in the parent module are executed.
handleLoadGen :: REPLExpr -> Disco IErr ()
handleLoadGen (Load file) = 
  handleLoad file >> lastFile .= Just file >>return ()

handleLoad :: FilePath -> Disco IErr Bool
handleLoad fp = catchAndPrintErrors False $ do
    let (directory, modName) = splitFileName fp
    m@(ModuleInfo _ props _ _ _) <- loadDiscoModule directory modName
    setLoadedModule m
    t <- withTopEnv $ runAllTests props
    io . putStrLn $ "Loaded."
    garbageCollect
    return t


handleParse :: REPLExpr -> Disco IErr ()
handleParse (Parse t) = iprint $ t

handlePretty :: REPLExpr -> Disco IErr ()
handlePretty (Pretty t) = renderDoc (prettyTerm t) >>= iputStrLn


handleReload :: REPLExpr -> Disco IErr ()
handleReload t = do
                  file <- use lastFile
                  case file of
                    Nothing -> iputStrLn "No file to reload."
                    Just f  -> handleLoad f >> return()

handleShowDefn :: REPLExpr -> Disco IErr ()
handleShowDefn (ShowDefn x) = 
  handleShowDefn_ x >>= iputStrLn
  where
  handleShowDefn_ x = do
    defns   <- use topDefns
    tyDefns <- use topTyDefns
    case M.lookup (coerce x) defns of
      Just d  -> renderDoc $ prettyDefn d
      Nothing -> case M.lookup name tyDefns of
        Just t  -> renderDoc $ prettyTyDef name t
        Nothing -> return $ "No definition for " ++ show x
    where
      name = name2String x


handleTypeCheck :: REPLExpr -> Disco IErr ()
handleTypeCheck (TypeCheck t) = 
    handleTypeCheck_ t >>= iputStrLn
    where 
    handleTypeCheck_ t = do
      ctx <- use topCtx
      tymap <- use topTyDefns
      case (evalTCM $ extends ctx $ withTyDefns tymap $ inferTop t) of
        Left e        -> return.show $ e    -- XXX pretty-print
        Right (_,sig) -> renderDoc $ prettyTerm t <+> text ":" <+> prettyPolyTy sig

-- | show names and types for each item in 'topCtx'
handleNames :: REPLExpr -> Disco IErr ()
handleNames _ = do
  ctx  <- use topCtx
  mapM_ showFn $ M.toList ctx
  where
      showFn (x, ty) = do
        p  <- renderDoc . hsep $ [prettyName x, text ":", prettyPolyTy ty]
        io . putStrLn $ p

------------------------------------------
--- Util functions
------------------------------------------

addModule :: ModuleInfo -> Disco IErr ()
addModule mi = do
  curMI <- use topModInfo
  mi' <- adaptError TypeCheckErr $ combineModuleInfo [curMI, mi]
  topModInfo .= mi'
  populateCurrentModuleInfo

handleImport :: ModName -> Disco IErr ()
handleImport modName = catchAndPrintErrors () $ do
  mi <- loadDiscoModule "" modName
  addModule mi

loadFile :: FilePath -> Disco IErr (Maybe String)
loadFile file = io $ handle (\e -> fileNotFound file e >> return Nothing) (Just <$> readFile file)

fileNotFound :: FilePath -> IOException -> IO ()
fileNotFound file _ = putStrLn $ "File not found: " ++ file


populateCurrentModuleInfo :: Disco IErr ()
populateCurrentModuleInfo = do
  ModuleInfo docs _ tys tyds tmds <- use topModInfo
  let cdefns = M.mapKeys coerce $ fmap compileDefn tmds
  topDocs    .= docs
  topCtx     .= tys
  topTyDefns .= tyds
  topDefns   .= tmds
  loadDefs cdefns
  return ()

-- | Add information from ModuleInfo to the Disco monad. This includes updating the
--   Disco monad with new term definitions, documentation, types, and type definitions.
--   Replaces any previously loaded module.
setLoadedModule :: ModuleInfo -> Disco IErr ()
setLoadedModule mi = do
  topModInfo .= mi
  populateCurrentModuleInfo

-- XXX Return a structured summary of the results, not a Bool;
-- separate out results generation and pretty-printing.  Then move it
-- to the Property module.
runAllTests :: Ctx ATerm [AProperty] -> Disco IErr Bool  -- (Ctx ATerm [TestResult])
runAllTests aprops
  | M.null aprops = return True
  | otherwise     = do
      io $ putStrLn "Running tests..."
      and <$> mapM (uncurry runTests) (M.assocs aprops)
      -- XXX eventually this should be moved into Disco.Property and
      -- use a logging framework?

  where
    numSamples :: Int
    numSamples = 50   -- XXX make this configurable somehow

    runTests :: Name ATerm -> [AProperty] -> Disco IErr Bool
    runTests n props = do
      iputStr ("  " ++ name2String n ++ ":")
      results <- sequenceA . fmap sequenceA $ map (id &&& runTest numSamples) props
      let failures = filter (not . testIsOK . snd) results
      case null failures of
        True  -> iputStrLn " OK"
        False -> do
          iputStrLn ""
          forM_ failures (uncurry prettyTestFailure)
      return (null failures)

-- XXX redo with message framework, with proper support for indentation etc.
-- XXX also move it to Property or Pretty or somewhere like that
prettyTestFailure :: AProperty -> TestResult -> Disco IErr ()
prettyTestFailure _ (TestOK {}) = return ()
prettyTestFailure prop (TestFalse env) = do
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStr "  - Test is false: " >> iputStrLn dp
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestEqualityFailure ty v1 v2 env) = do
  iputStr     "  - Test result mismatch for: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    - Expected: " >> prettyValue ty v2
  iputStr     "    - But got:  " >> prettyValue ty v1
  let qTys = M.fromList . fst . unsafeUnbind $ prop
  prettyCounterexample qTys env
prettyTestFailure prop (TestRuntimeFailure e) = do
  iputStr     "  - Test failed: "
  dp <- renderDoc $ prettyProperty (eraseProperty prop)
  iputStrLn dp
  iputStr     "    " >> iprint e

-- XXX comment, move somewhere else
prettyCounterexample :: Ctx ATerm Type -> Env -> Disco IErr ()
prettyCounterexample ctx env
  | M.null env = return ()
  | otherwise  = do
      iputStrLn "    Counterexample:"
      let maxNameLen = maximum . map (length . name2String) $ M.keys env
      mapM_ (prettyBind maxNameLen) $ M.assocs env
  where
    prettyBind maxNameLen (x,v) = do
      iputStr "      "
      iputStr =<< (renderDoc . prettyName $ x)
      iputStr (replicate (maxNameLen - length (name2String x)) ' ')
      iputStr " = "
      prettyValue (ctx !? coerce x) v
    m !? k = case M.lookup k m of
      Just val -> val
      Nothing  -> error $ "Failed M.! with key " ++ show k ++ " in map " ++ show m


-- UNSORTED BELOW


handleLet :: Name Term -> Term -> Disco IErr ()
handleLet x t = do
  ctx <- use topCtx
  tymap <- use topTyDefns
  let mat = evalTCM (extends ctx $ withTyDefns tymap $ inferTop t)
  case mat of
    Left e -> io.print $ e   -- XXX pretty print
    Right (at, sig) -> do
      let c = compileTerm at
      thnk <- withTopEnv (mkValue c)
      topCtx   %= M.insert x sig
        -- XXX ability to define more complex things at REPL prompt, with patterns etc.
      topDefns %= M.insert (coerce x) (Defn (coerce x) [] (getType at) [bind [] at])
      topEnv   %= M.insert (coerce x) thnk

--handleDesugar :: Term -> Disco IErr String
handleDesugar :: REPLExpr -> Disco IErr ()
handleDesugar (Desugar t) = handleDesugar_ t >>= iputStrLn
  where
    handleDesugar_ t = do
      ctx <- use topCtx
      case evalTCM (extends ctx $ inferTop t) of
        Left e       -> return.show $ e
        Right (at,_) -> renderDoc . prettyTerm . eraseDTerm . runDSM . desugarTerm $ at



evalTerm :: Term -> Disco IErr ()
evalTerm t = do
  ctx   <- use topCtx
  tymap <- use topTyDefns
  case evalTCM (extends ctx $ withTyDefns tymap $ inferTop t) of
    Left e   -> iprint e    -- XXX pretty-print
    Right (at,_) ->
      let ty = getType at
          c  = compileTerm at
      in do
        v <- withTopEnv $ do
          cv <- mkValue c
          prettyValue ty cv
          return cv
        topCtx %= M.insert (string2Name "it") (toPolyType ty)
        topEnv %= M.insert (string2Name "it") v
        garbageCollect






