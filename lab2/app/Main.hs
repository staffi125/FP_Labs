module Main (main) where

import Control.Concurrent (setNumCapabilities)
import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad (forM_, when)
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors)
import Gauss
import Matrix
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout NoBuffering
  cores <- getNumProcessors
  args <- getArgs
  putStrLn "Розв'язання СЛАР паралельним методом Гаусса (з вибором головного елемента)"
  printf "Доступно процесорних ядер: %d\n\n" cores
  case args of
    [path] -> fromFile path cores
    _ -> interactive cores

ask :: String -> IO String
ask label = putStr label >> hFlush stdout >> getLine

askInt :: String -> Int -> IO Int
askInt label def = do
  s <- ask (label ++ " [" ++ show def ++ "]: ")
  if null s then pure def else maybe (putStrLn "Потрібне ціле число." >> askInt label def) pure (readMaybe s)

interactive :: Int -> IO ()
interactive cores = do
  putStrLn "1 — ввести систему з клавіатури"
  putStrLn "2 — згенерувати випадкову систему й виміряти час"
  mode <- askInt "Режим" 2
  if mode == 1 then fromKeyboard cores else benchmark cores

fromKeyboard :: Int -> IO ()
fromKeyboard cores = do
  n <- askInt "Кількість рівнянь n" 3
  putStrLn ("Введіть " ++ show n ++ " рядків по " ++ show (n + 1) ++ " числа (коефіцієнти і вільний член):")
  rows <- mapM (\i -> readRow i (n + 1)) [1 .. n]
  solveAndPrint cores (System n (map init rows) (map last rows))
  where
    readRow i len = do
      s <- ask (show (i :: Int) ++ ": ")
      case mapM readMaybe (words s) of
        Just xs | length xs == len -> pure xs
        _ -> putStrLn ("Потрібно " ++ show len ++ " чисел.") >> readRow i len

fromFile :: FilePath -> Int -> IO ()
fromFile path cores = do
  text <- readFile path
  case readSystem text of
    Left message -> hPutStrLn stderr message >> exitFailure
    Right system -> solveAndPrint cores system

solveAndPrint :: Int -> System -> IO ()
solveAndPrint cores system = do
  setNumCapabilities cores
  result <- try (solveParallel cores system)
  case result of
    Left (ErrorCall message) -> putStrLn message
    Right x -> do
      putStrLn "\nРозв'язок:"
      forM_ (zip [1 :: Int ..] x) $ \(i, v) -> printf "  x%d = %.6f\n" i v
      printf "Нев'язка max|Ax - b| = %.3e\n" (maxError (multiply (coefficients system) x) (rightSide system))

timed :: IO a -> IO (a, Double)
timed action = do
  start <- getMonotonicTime
  r <- action
  end <- getMonotonicTime
  pure (r, end - start)

benchmark :: Int -> IO ()
benchmark cores = do
  n <- askInt "Розмір системи n" 800
  k <- askInt "Максимальна кількість потоків" cores
  let (system, exact) = randomSystem n 2026
  _ <- evaluate (sum (map sum (coefficients system)) + sum (rightSide system))
  printf "\n%-24s %8s %10s %10s %12s\n" "Метод" "Потоків" "Час, с" "Прискор." "Похибка x"
  putStrLn (replicate 68 '-')
  setNumCapabilities 1
  (xs, base) <- timed (solveSequential system >>= \x -> evaluate (sum x) >> pure x)
  printf "%-24s %8d %10.3f %10.2f %12.2e\n" "Послідовний" (1 :: Int) base (1 :: Double) (maxError xs exact)
  forM_ (takeWhile (< k) (iterate (* 2) 1) ++ [k]) $ \t -> do
    setNumCapabilities t
    (xp, sec) <- timed (solveParallel t system >>= \x -> evaluate (sum x) >> pure x)
    printf "%-24s %8d %10.3f %10.2f %12.2e\n" "Паралельний" t sec (base / sec) (maxError xp exact)
  when (n <= 10) $ forM_ (zip [1 :: Int ..] xs) $ \(i, v) -> printf "  x%d = %.6f\n" i v
