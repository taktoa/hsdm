{-# OPTIONS_GHC -fno-warn-unused-do-bind -Wall #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.HSDM.PAM where

import           Data.Monoid
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable

-- the opaque void* from pam
data CPamHandle = CPamHandle
-- the handle returned to the app
data PamHandle  = PamHandle (Ptr CPamHandle, FunPtr ConvFunc)

data PamMessage = PamMessage { pamString :: String
                             , pamStyle  :: PamStyle }
                deriving (Show, Eq)

data PamResponse = PamResponse String deriving (Show, Eq)

data CPamMessage = CPamMessage { messageStyle :: CInt
                               , msg          :: CString }
                 deriving (Show, Eq)
data CPamResponse

data PamStyle = PamPromptEchoOff
              | PamPromptEchoOn
              | PamErrorMsg | PamTextInfo deriving (Show, Eq)
data PamRetCode = PamSuccess
                | PamRetCode Int
                deriving (Show, Eq)

data CPamConv = CPamConv { conv        :: FunPtr ConvFunc
                         , appdata_ptr :: Ptr () }

instance Storable CPamConv where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = sizeOf (undefined :: FunPtr()) + sizeOf (undefined :: Ptr())
  peek p = CPamConv <$> (peekByteOff p 0 :: IO (FunPtr ConvFunc))
                    <*> (peekByteOff p 8 :: IO (Ptr ()))
  poke p (CPamConv c ap) = do
    (\ptr val -> pokeByteOff ptr 0 (val :: FunPtr ConvFunc)) p c
    (\ptr val -> pokeByteOff ptr 8 (val :: Ptr ())) p ap

instance Storable CPamMessage where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = sizeOf (undefined :: CInt) + sizeOf (undefined :: Ptr())
  peek p = CPamMessage <$> (peekByteOff p 0 :: IO CInt)
                       <*> (peekByteOff p 8 :: IO CString)
  poke p (CPamMessage st msg) = do
    (\ptr val -> do { pokeByteOff ptr 0 (val::CInt)}) p st
    (\ptr val -> do { pokeByteOff ptr 8 (val::CString)}) p msg


type PAMConv = [PamMessage] -> IO [PamResponse]

type ConvFunc = CInt -> Ptr ( Ptr CPamMessage) -> Ptr ( Ptr ()) -> Ptr () -> IO CInt
foreign import ccall "wrapper" mkconvFunc :: ConvFunc -> IO (FunPtr ConvFunc)

messageFromC :: CPamMessage -> IO PamMessage
messageFromC cmes = (\s -> PamMessage s style) <$> peekCString (msg cmes)
  where
    style = case messageStyle cmes
            of 1 -> PamPromptEchoOff
               2 -> PamPromptEchoOn
               3 -> PamErrorMsg
               4 -> PamTextInfo
               a -> error $ "unknown style value: " ++ show a

convWrapper :: PAMConv               -- A callback given by the user
            -> CInt                  -- Number of items in the array
            -> Ptr (Ptr CPamMessage) -- Array of messages
            -> Ptr (Ptr ())          -- Responses going back out to PAM (?)
            -> Ptr ()                -- Pointer for application data (useless)
            -> IO CInt               -- Status code (0 indicates success)
convWrapper _     c _    _    _  | c <= 0 = return 19 -- an error code?
convWrapper userC c msgs resp dp          = do
  putStrLn $ "in wrapper with " <> (show c)
  p1 <- peek msgs

  -- turn input into array of pointers
  cMessages <- peekArray (fromIntegral c) p1
  print cMessages

  -- turn array of pointers into array of data's
  messages <- mapM messageFromC cMessages
  print messages
  return 0

pamStart :: String -> String -> PAMConv -> IO (PamHandle,PamRetCode)
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
  putStrLn $ "r1: " ++ (show r1)

  pamHandle <- peek pamHandlePtr
  let
    retCode = if (r1 == 0) then
      PamSuccess
    else
      PamRetCode $ fromIntegral r1
  return (PamHandle (pamHandle, wrapped), retCode)

pamAuthenticate :: PamHandle -> CInt -> IO (CInt)
pamAuthenticate (PamHandle (hnd,_)) flags = do
  ret <- c_pam_authenticate hnd flags
  return ret

pamEnd :: PamHandle -> CInt -> IO (CInt)
pamEnd (PamHandle (hnd,_)) status = do
  ret <- c_pam_end hnd status
  return ret

foreign import ccall "pam_start" c_pam_start :: CString -> CString -> Ptr CPamConv -> Ptr (Ptr CPamHandle) -> IO CInt
foreign import ccall "pam_authenticate" c_pam_authenticate :: Ptr CPamHandle -> CInt -> IO CInt
foreign import ccall "pam_end" c_pam_end :: Ptr CPamHandle -> CInt -> IO CInt
