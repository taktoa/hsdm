module Main where

import           System.HSDM.PAM

conversation :: [PamMessage] -> IO [PamResponse]
conversation m = do
  print m
  results <- mapM go m
  return results
  where
    go (PamMessage str style) = do
      print str
      print style
      return $ PamResponse "test"

main :: IO ()
main = do
  putStrLn "doing start"
  ( handle, retcode) <- pamStart "slim" "test" conversation
  print retcode
  putStrLn "did start"
  putStrLn "auth"
  ret1 <- pamAuthenticate handle 0
  print ret1
  putStrLn "setcred"
  ret2 <- pamSetCred handle (False,PAM_ESTABLISH_CRED)
  print ret2
  putStrLn "open"
  ret3 <- pamOpenSession handle False
  print ret3
  -- do things
  putStrLn "close"
  ret10 <- pamCloseSession handle False
  print ret10
  putStrLn "clear"
  ret11 <- pamSetCred handle (False,PAM_DELETE_CRED)
  print ret11
  putStrLn "doing stop"
  ret12 <- pamEnd handle 0
  print ret12
  putStrLn "did stop"
  return ()
