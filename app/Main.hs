{-
                           Jgy__
                         jWW  `""9Wf
                       _#WWW     IW
                      jWWWWW     IW
              __,yyyyyWWWWW     IWyyyy___
         _jyWWP"^``"`.C"9*,J _.mqD:^^"WWWWWWQg__
       jgW"^.C/"    .C'     I    `D.     'D._"WQg_
     jWP` .C"       C'      I     `D._     `D._ "Qg_
   jQP`  .C    ,d^^b._      I      _.d^^b.   `D._ "Qg
  jQ^  .C"   /`   .+" \     I     / "+.   `\   `D.  XQ
 jQ'  .C'   |`  ."    )    _I    (     ".  |    `D.  4#
 Qf  .C     (   (    /    / /\    \     )  )     `D.  Qg
jW   C'      \__\_.+'    / /  \    `+._/__/       `D  jQ
Qf   C         C        /_/    \         D         D   Qk
Qf   C      _  C        \_\____/         D  _      D   QF
QL   C      \`+.__     _______     ______.+'/      D   QF
B&   C.      \   \|    ||     |    ||      /       D   Qf
jQ   `C.      \   |____|/     |____|/__   /      .D'   jW
 TQ   `C.      \._   `+.__________/___/|_/      .D'   jQ`
  9Q   `C.      C.`+._           |   |/.D'     .D'   jQ'
   "Qg  `C.     `C.   `"+-------'   ' .D'     .D'   pW`
    ^WQy `C.     `C.        I        .D'    _.D' yW"
      ^9Qy_`C.    `C.       I       .D'   _.D jgW"
         `9WQgC.__ `C.      I      .D'  _.Dp@@"`
            `""9WQQggyyyyyygyyyyyQggQWQH""
-}

-- Пример использования решений 1.6, 2.6, 3.2 и 3.3.
-- Код ввода-вывода дан готовым; разбираться в его реализации не требуется.
module Main where

import Control.Exception (evaluate)
import Data.Foldable (for_)
import Examples (tokens)
import Level1 (wordCount)
import Level2 (WordCountShow (..), stdWcShow, wordCountShow)
import Level3 (Document (..), buildInvertedIndex, findMostRelevant)
import Paths_homework (getDataFileName)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName)
import System.IO (hFlush, stdout)

main :: IO ()
main = getArgs >>= \case
  ["wc"] -> mainWc getContents
  ["wc", file] -> mainWc (readFile file)
  "find" : query : queries -> do
    files <- mapM (getDataFileName . (\name -> "text/" ++ name ++ ".txt")) harryBooks
    mainFind (query : queries) files
  "find-in" : query : file : files -> mainFind [query] (file : files)
  [] -> putStrLn usage
  ["--help"] -> putStrLn usage
  _ -> die usage

usage :: String
usage = unlines
  [ "make run ARGS=wc < text/alice.txt — частоты слов (1.6 и 2.6)"
  , "make run ARGS='wc text/alice.txt' — тот же пример без перенаправления ввода"
  , "make run ARGS='find hermione basilisk lol' — поиск по семи книгам (3.2 и 3.3)"
  , "make run ARGS='find-in слово файл1.txt файл2.txt' — поиск по своим файлам"
  ]

mainWc :: IO String -> IO ()
mainWc readInput = do
  putStrLn aliceIntro
  putStrLn "Top used words!"
  input <- readInput
  putStr $ wordCountShow (stdWcShow { topSize = 30 }) $ wordCount $ lines input

mainFind :: [String] -> [FilePath] -> IO ()
mainFind queries files = do
  wordsToFind <- mapM normalizeQuery queries
  putStrLn harryIntro
  putStrLn "Welcome to the Harry-Search!"
  docs <- mapM readDocument files
  putStrLn "Building inverted index..."
  hFlush stdout
  index <- evaluate $ buildInvertedIndex docs
  putStrLn "Searching..."
  for_ wordsToFind \word ->
    putStrLn $ word ++ " - " ++ case findMostRelevant index word of
      Nothing -> "not found :<"
      Just name -> "most used in \"" ++ name ++ "\""

normalizeQuery :: String -> IO String
normalizeQuery query = case tokens [query] of
  [word] -> pure word
  _ -> die $ "Каждый поисковый запрос должен содержать одно слово: " ++ show query

readDocument :: FilePath -> IO Document
readDocument path = do
  input <- readFile path
  pure $ Document { docName = takeBaseName path, docContent = lines input }

aliceIntro :: String
aliceIntro = unlines
  [ "           .'\\   /`."
  , "         .'.-.`-'.-.`."
  , "    ..._:   .-. .-.   :_..."
  , "  .'    '-.(o ) (o ).-'    `."
  , " :  _    _ _`~(_)~`_ _    _  :"
  , ":  /:   ' .-=_   _=-. `   ;\\  :"
  , ":   :|-.._  '     `  _..-|:   :"
  , " :   `:| |`:-:-.-:-:'| |:'   :"
  , "  `.   `.| | | | | | |.'   .'"
  , "    `.   `-:_| | |_:-'   .'"
  , "      `-._   ````    _.-'"
  , "          ``-------''"
  ]

harryIntro :: String
harryIntro = unlines
  [ "        ___                             | '  \\"
  , "   ___  \\ /  ___         ,'\\_           | .-. \\        /|"
  , "   \\ /  | |,'__ \\  ,'\\_  |   \\          | | | |      ,' |_   /|"
  , " _ | |  | |\\/  \\ \\ |   \\ | |\\_|    _    | |_| |   _ '-. .-',' |_   _"
  , "// | |  | |____| | | |\\_|| |__    //    |     | ,'_`. | | '-. .-',' `. ,'\\_"
  , "\\\\_| |_,' .-, _  | | |   | |\\ \\  //    .| |\\_/ | / \\ || |   | | / |\\  \\|   \\"
  , " `-. .-'| |/ / | | | |   | | \\ \\//     |  |    | | | || |   | | | |_\\ || |\\_|"
  , "   | |  | || \\_| | | |   /_\\  \\ /      | |`    | | | || |   | | | .---'| |"
  , "   | |  | |\\___,_\\ /_\\ _      //       | |     | \\_/ || |   | | | |  /\\| |"
  , "   /_\\  | |           //_____//       .||`  _   `._,' | |   | | \\ `-' /| |"
  , "        /_\\           `------'        \\ |  /-\\   _     `.\\  | |  `._,' /_\\"
  , "                                       \\|        |           `.\\"
  ]

harryBooks :: [FilePath]
harryBooks =
  [ "Harry Potter and the Chamber of Secrets"
  , "Harry Potter and the Deathly Hallows"
  , "Harry Potter and the Goblet of Fire"
  , "Harry Potter and The Half-Blood Prince"
  , "Harry Potter and the Order of the Phoenix"
  , "Harry Potter and the Prisoner of Azkaban"
  , "Harry Potter and the Sorcerer's Stone"
  ]
