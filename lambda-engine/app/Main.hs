module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)

import Lambda.Check (Session (..), checkFile, loadSession, prettyResults, reportOk)
import Lambda.Color
  ( ColorMode (..)
  , Palette (..)
  , detectPalette
  , parseColorMode
  , withCode
  )
import Lambda.Command
  ( CommandResult (..)
  , renderResult
  , replHelp
  , runLine
  )
import Lambda.Console (setupConsole)
import Lambda.Eval (Ctx (..))
import Lambda.Syntax (Language (..))

main :: IO ()
main = do
  setupConsole
  args <- getArgs
  case parseArgs args of
    Left err           -> die err
    Right ActionHelp   -> putStr usage
    Right (ActionCheck file) -> runCheck file
    Right (ActionLoad load)  -> runLoad load

data Action
  = ActionHelp
  | ActionCheck FilePath
  | ActionLoad LoadArgs

-- | @lambda load@: an optional file, @-e@ commands, color, and the
-- language of a session started without a file.
data LoadArgs = LoadArgs
  { loadFile     :: Maybe FilePath
  , loadEvals    :: [String]
  , loadColor    :: ColorMode
  , loadLanguage :: Maybe Language
  }

-- | What the REPL runs on: the context and, if there is one, the file
-- @:reload@ re-reads.
data Repl = Repl
  { replFile :: Maybe FilePath
  , replCtx  :: Ctx
  }

parseArgs :: [String] -> Either String Action
parseArgs [] = Left (usage ++ "\nlambda: missing command")
parseArgs ["-h"] = Right ActionHelp
parseArgs ["--help"] = Right ActionHelp
parseArgs ("check" : rest) = parseCheck rest
parseArgs ("load" : rest) = parseLoad rest
parseArgs (cmd : _) =
  Left ("lambda: unknown command ‘" ++ cmd ++ "’\n" ++ usage)

parseCheck :: [String] -> Either String Action
parseCheck ["-h"] = Right ActionHelp
parseCheck ["--help"] = Right ActionHelp
parseCheck [file] = Right (ActionCheck file)
parseCheck [] = Left "lambda check: missing FILE"
parseCheck _ = Left "lambda check: too many arguments"

parseLoad :: [String] -> Either String Action
parseLoad = go (LoadArgs Nothing [] ColorAuto Nothing)
  where
    go args [] =
      case (loadFile args, loadLanguage args) of
        (Just _, Just _) -> Left "lambda load: --language is only for a session without FILE (the file names its own language)"
        _ -> Right (ActionLoad args)
    go args (flag : rest)
      | flag == "-h" || flag == "--help" = Right ActionHelp
      | flag == "--no-color" = go args { loadColor = ColorNever } rest
      | flag == "-e" || flag == "--eval" = case rest of
          (cmd : rest') -> go (addEval cmd args) rest'
          []            -> Left "lambda load: -e needs a command"
      | "--eval=" `isPrefixOf` flag = go (addEval (drop 7 flag) args) rest
      | "--color=" `isPrefixOf` flag = setColor args rest (drop 8 flag)
      | flag == "--color" = case rest of
          (when_ : rest') -> setColor args rest' when_
          []              -> Left "lambda load: --color needs auto|always|never"
      | "--language=" `isPrefixOf` flag = setLanguage args rest (drop 11 flag)
      | flag == "--language" = case rest of
          (lang : rest') -> setLanguage args rest' lang
          []             -> Left "lambda load: --language needs pure|typed"
      | "-" `isPrefixOf` flag =
          Left ("lambda load: unknown option ‘" ++ flag ++ "’")
      | otherwise = case loadFile args of
          Just _  -> Left "lambda load: too many arguments"
          Nothing -> go args { loadFile = Just flag } rest

    addEval cmd args = args { loadEvals = loadEvals args ++ [cmd] }

    setColor args rest when_ =
      case parseColorMode when_ of
        Just c  -> go args { loadColor = c } rest
        Nothing -> Left "lambda load: --color needs auto|always|never"

    setLanguage args rest lang =
      case lang of
        "pure"  -> go args { loadLanguage = Just Pure } rest
        "typed" -> go args { loadLanguage = Just Typed } rest
        _       -> Left "lambda load: --language needs pure|typed"

runCheck :: FilePath -> IO ()
runCheck file = do
  pal <- detectPalette ColorAuto
  result <- checkFile file
  case result of
    Left err -> do
      hPutStrLn stderr (withCode (palError pal) (palReset pal) (stripNL err))
      exitFailure
    Right tasks -> do
      putStrLn (colorStatuses pal (prettyResults tasks))
      if reportOk tasks then exitSuccess else exitFailure

colorStatuses :: Palette -> String -> String
colorStatuses pal text = intercalate "\n" (map paintLine (lines text))
  where
    paintLine l
      | ": DONE" `isSuffixOf` l = withCode (palOk pal) (palReset pal) l
      | ": FAILED" `isSuffixOf` l = withCode (palError pal) (palReset pal) l
      | ": PARTIAL" `isInfixOf` l = withCode (palFalse pal) (palReset pal) l
      | otherwise = l

runLoad :: LoadArgs -> IO ()
runLoad args = do
  pal <- detectPalette (loadColor args)
  started <- case loadFile args of
    Just path -> fmap (fmap fromSession) (loadSession path)
    Nothing -> return (Right (emptySession (fromMaybe Pure (loadLanguage args))))
  case started of
    Left err -> do
      hPutStr stderr (withCode (palError pal) (palReset pal) (stripNL err))
      hPutStrLn stderr ""
      exitFailure
    Right st ->
      case loadEvals args of
        [] -> do
          putStrLn (greeting st)
          repl st pal
        cmds -> runEvals st pal cmds

fromSession :: Session -> Repl
fromSession sess = Repl (Just (sessionFile sess)) (sessionCtx sess)

-- | A session without a file: no definitions, the given language.
emptySession :: Language -> Repl
emptySession lang = Repl Nothing (Ctx [] (lang == Typed))

greeting :: Repl -> String
greeting st = case replFile st of
  Just path -> "Loaded " ++ path
  Nothing ->
    "Empty session, language " ++ (if ctxTyped (replCtx st) then "typed" else "pure")
      ++ ". Define names with ‘name := term’; :help lists the commands."

stripNL :: String -> String
stripNL = reverse . dropWhile (== '\n') . reverse

-- | @:reload@: re-read the session file. On success the fresh context
-- replaces the old one (definitions made at the prompt are dropped);
-- on failure the error is printed and 'Nothing' returned, so the
-- caller can keep working with the old context.
reload :: Repl -> Palette -> IO (Maybe Ctx)
reload st pal = case replFile st of
  Nothing -> do
    complain "error: nothing to reload: the session was started without a file"
    return Nothing
  Just path -> do
    loaded <- loadSession path
    case loaded of
      Left err -> do
        complain (stripNL err)
        return Nothing
      Right sess -> do
        putStrLn ("Loaded " ++ sessionFile sess)
        return (Just (sessionCtx sess))
  where
    complain msg = hPutStrLn stderr (withCode (palError pal) (palReset pal) msg)

runEvals :: Repl -> Palette -> [String] -> IO ()
runEvals st pal = go (replCtx st)
  where
    go _ [] = return ()
    go ctx (cmd : rest) = do
      let (ctx', result) = runLine ctx cmd
          shown          = renderResult pal result
      case result of
        CommandQuit -> exitSuccess
        CommandErr _ -> do
          hPutStrLn stderr shown
          exitFailure
        CommandReload -> do
          fresh <- reload st pal
          maybe exitFailure (`go` rest) fresh
        _ -> do
          putStrLn shown
          go ctx' rest

repl :: Repl -> Palette -> IO ()
repl st pal = runInputT settings (loop (replCtx st))
  where
    -- Filename completion would steal Tab; arrows and history still work.
    -- The prompt is uncolored so Haskeline's cursor width stays correct.
    settings = (defaultSettings :: Settings IO)
      { complete = noCompletion
      , autoAddHistory = True
      }
    loop ctx = handleInterrupt (loop ctx) $ do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just line ->
          case words line of
            [] -> loop ctx
            _  ->
              let (ctx', result) = runLine ctx line
                  shown          = renderResult pal result
              in  case result of
                    CommandQuit -> return ()
                    CommandErr _ -> do
                      liftIO (hPutStrLn stderr shown)
                      loop ctx
                    CommandReload -> do
                      fresh <- liftIO (reload st pal)
                      loop (fromMaybe ctx fresh)
                    _ -> do
                      liftIO (putStrLn shown)
                      loop ctx'

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

usage :: String
usage = unlines
  [ "Usage:"
  , "  lambda check FILE"
  , "  lambda load [OPTIONS] [FILE]"
  , ""
  , "check  Run every task of a .lam file and print DONE / PARTIAL / FAILED."
  , "load   Load the definitions of a .lam file and start a REPL."
  , "       Without FILE: an empty session (no definitions, :reload unavailable)."
  , ""
  , "load options:"
  , "  -e CMD, --eval CMD   run a REPL command and exit (repeatable)"
  , "  --language LANG      pure (default) or typed; only without FILE"
  , "  --color WHEN         auto (default), always, or never"
  , "  --no-color           same as --color=never"
  , ""
  , "File format: see FORMAT.md. Expressions:"
  , "  x y z           application (left-associative)"
  , "  \\x y. body      lambda (also λx y. body and \\x y -> body)"
  , "  (expr)          grouping"
  , "  3               Church numeral ⌜3⌝ (language pure) or Int literal (typed)"
  , "  ...             hole (incomplete)"
  , "  -- comment      to end of line"
  , ""
  , replHelp
  ]
