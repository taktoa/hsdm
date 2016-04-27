{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.HSDM.PAM where

import           Data.Bits
import           Data.Monoid
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           System.HSDM.PAM.Internals
import           System.IO

-- the opaque void* from pam
data CPamHandle = CPamHandle
-- the handle returned to the app
data PamHandle  = PamHandle (Ptr CPamHandleT, FunPtr ConvFunc) deriving (Show)

data PamMessage = PamMessage { pamString :: String
                             , pamStyle  :: PamStyle }
                deriving (Show, Eq)

data PamResponse = PamResponse String deriving (Show, Eq)

data PAMReturnCode = PAM_SUCCESS | PAM_OPEN_ERR | PAM_SYMBOL_ERR | PAM_SERVICE_ERR | PAM_SYSTEM_ERR | PAM_BUF_ERR | PAM_PERM_DENIED | PAM_AUTH_ERR | PAM_CRED_INSUFFICIENT | PAM_AUTHINFO_UNAVAIL | PAM_USER_UNKNOWN | PAM_MAXTRIES | PAM_NEW_AUTHTOK_REQD deriving (Enum, Show, Eq)

data PamCredFlags = PAM_ESTABLISH_CRED | PAM_DELETE_CRED | PAM_REINITIALIZE_CRED | PAM_REFRESH_CRED deriving (Enum, Show)

data PamStyle = PamPromptEchoOff
              | PamPromptEchoOn
              | PamErrorMsg | PamTextInfo deriving (Show, Eq)

data PAMItem = PAM_TTY { tty :: String }

type PAMConv = [PamMessage] -> IO [PamResponse]

messageFromC :: CPamMessage -> IO PamMessage
messageFromC cmes = (\s -> PamMessage s style) <$> peekCString (msg cmes)
  where
    style = case msg_style cmes
            of 1 -> PamPromptEchoOff
               2 -> PamPromptEchoOn
               3 -> PamErrorMsg
               4 -> PamTextInfo
               a -> error $ "unknown style value: " ++ show a

convWrapper :: PAMConv               -- A callback given by the user
            -> CInt                  -- Number of items in the array
            -> Ptr (Ptr ())          -- Array of messages
            -> Ptr (Ptr ())          -- Responses going back out to PAM (?)
            -> Ptr ()                -- Pointer for application data (useless)
            -> IO CInt               -- Status code (0 indicates success)
convWrapper _     c _    _    _  | c <= 0 = return 19 -- an error code?
convWrapper userC c msgs resp _           = do
  p1 <- peek msgs

  let mesArr = castPtr p1 :: Ptr CPamMessage

  -- turn input into array of pointers
  cMessages <- peekArray (fromIntegral c) mesArr

  -- turn array of pointers into array of data's
  messages <- mapM messageFromC cMessages

  replies <- userC messages

  cResponses <- mapM responseToC replies

  respArr <- mallocArray $ fromIntegral c
  pokeArray respArr cResponses

  poke resp $ castPtr respArr

  return 0
  where
    responseToC (PamResponse str) = do
      resp' <- newCString str
      return $ CPamResponse resp' 0

pamStart :: String -> String -> PAMConv -> IO (PamHandle,PAMReturnCode)
pamStart service user conv = do
  cService <- newCString service
  cUser <- newCString user

  wrapped <- mkconvFunc $ convWrapper conv
  print wrapped
  let convStructHs = CPamConv wrapped nullPtr

  convPtr <- malloc
  poke convPtr convStructHs

  pamHandlePtr <- malloc
  poke pamHandlePtr nullPtr

  r1 <- c_pam_start cService cUser convPtr pamHandlePtr

  pamHandle <- peek pamHandlePtr
  return (PamHandle (pamHandle, wrapped), toEnum $ fromIntegral r1)

pamAuthenticate :: PamHandle -> CInt -> IO (PAMReturnCode)
pamAuthenticate (PamHandle (hnd,_)) flags = do
  ret <- c_pam_authenticate hnd flags
  return $ toEnum $ fromIntegral ret

pamSetCred :: PamHandle -> (Bool,PamCredFlags) -> IO PAMReturnCode
pamSetCred (PamHandle (hnd,_)) (silent, flag) = do
  let flag' = case flag of
        PAM_ESTABLISH_CRED -> 0x2
        PAM_DELETE_CRED -> 0x4
        PAM_REINITIALIZE_CRED -> 0x8
        PAM_REFRESH_CRED -> 0x10
  let silent' = if silent then 0x8000 else 0
  ret <- c_pam_setcred hnd (flag' .|. silent')
  return $ toEnum $ fromIntegral ret

pamEnd :: PamHandle -> Int -> IO PAMReturnCode
pamEnd (PamHandle (hnd,_)) status = do
  putStrLn "ending"
  hFlush stdout
  ret <- c_pam_end hnd $ fromIntegral status
  return $ toEnum $ fromIntegral ret

pamOpenSession :: PamHandle -> Bool -> IO PAMReturnCode
pamOpenSession (PamHandle (hnd,_)) silent = do
  ret <- c_pam_open_session hnd $ if silent then 0x8000 else 0
  return $ toEnum $ fromIntegral ret
pamCloseSession :: PamHandle -> Bool -> IO PAMReturnCode
pamCloseSession (PamHandle (hnd,_)) silent = do
  ret <- c_pam_close_session hnd $ if silent then 0x8000 else 0
  return $ toEnum $ fromIntegral ret

pamSetItem :: PamHandle -> PAMItem -> IO PAMReturnCode
pamSetItem (PamHandle (hnd,_)) item = do
  let (itemType,func) = getPtr item
  ptr <- func
  let ptr' = castPtr ptr :: Ptr ()
  ret <- c_pam_set_item hnd itemType ptr'
  free ptr
  return $ toEnum $ fromIntegral ret
  where
    getPtr :: PAMItem -> (CInt, IO CString)
    getPtr (PAM_TTY tty) = (3, newCString tty)
