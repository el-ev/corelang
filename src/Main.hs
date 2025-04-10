module Main (main) where
import Lang (preludeDefs, prettyPrint)

main :: IO ()
main = putStrLn $ prettyPrint preludeDefs