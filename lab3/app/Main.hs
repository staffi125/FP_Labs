module Main (main) where

import Control.Exception (IOException, try)
import Data.List (intercalate)
import Lexer
import Prelude hiding (Word)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Types
import Vowels

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  args <- getArgs
  let path = case args of
        file : _ -> file
        [] -> "textbook.txt"
  loaded <- try (readUtf8 path) :: IO (Either IOException String)
  case loaded of
    Left err -> hPutStrLn stderr ("Не вдалося відкрити файл " ++ path ++ ": " ++ show err) >> exitFailure
    Right content -> run path content

readUtf8 :: FilePath -> IO String
readUtf8 path = do
  h <- openFile path ReadMode
  hSetEncoding h utf8
  content <- hGetContents h
  length content `seq` hClose h
  pure content

run :: FilePath -> String -> IO ()
run path content = do
  let text@(Text sentences) = parseText content
      allWords = textWords text
      groups = vowelWordsByInitial text
  putStrLn ("Файл: " ++ path)
  putStrLn "\nТекст після заміни табуляцій і послідовностей пробілів одним пробілом:"
  putStr (squeezeSpaces content)
  putStrLn ("\nРечень: " ++ show (length sentences) ++ ", слів: " ++ show (length allWords)
            ++ ", різних слів на голосну: " ++ show (sum (map (length . snd) groups)))
  putStrLn "\nСлова, що починаються з голосних, у алфавітному порядку за першою літерою:"
  mapM_ (\(letter, ws) -> putStrLn ("  " ++ [letter] ++ ": " ++ intercalate ", " ws)) groups
