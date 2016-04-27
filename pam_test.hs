module Main where

import PAM


conversation :: [PamMessage] -> IO [PamResponse]
conversation = undefined

main :: IO ()
main = do
  putStrLn "doing start"
  ( handle, retcode) <- pamStart "slim" "test" conversation
  putStrLn "did start"
  pamAuthenticate handle 0
  putStrLn "doing stop"
  pamEnd handle 0
  putStrLn "did stop"
  return ()
