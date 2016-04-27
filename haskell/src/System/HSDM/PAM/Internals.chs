{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.HSDM.PAM.Internals where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

#include <security/pam_appl.h>

data CPamMessage = CPamMessage { msg_style :: CInt
                               , msg :: CString
                               }
                               deriving (Show,Eq)

instance Storable CPamMessage where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = sizeOf (undefined :: CInt) + sizeOf (undefined :: CString)
    peek p = CPamMessage <$> ({#get pam_message.msg_style #} p)
                         <*> ({#get pam_message.msg #} p)
    poke p (CPamMessage ms m) = do
        {#set pam_message.msg_style #} p ms
        {#set pam_message.msg #} p m

data CPamResponse = CPamResponse { resp :: CString
                                 , resp_retcode :: CInt
                                 }
                                 deriving (Show,Eq)

instance Storable CPamResponse where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = sizeOf (undefined :: CString) + sizeOf (undefined :: CInt)
    peek p = CPamResponse <$> ({#get pam_response.resp #} p)
                          <*> ({#get pam_response.resp_retcode #} p)
    poke p (CPamResponse r rc) = do
        {#set pam_response.resp #} p r
        {#set pam_response.resp_retcode #} p rc

data CPamConv = CPamConv { conv :: FunPtr (CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr () -> IO CInt)
                         , appdata_ptr :: Ptr ()
                         }
                         deriving (Show, Eq)

type ConvFunc = CInt -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> Ptr () -> IO CInt
foreign import ccall "wrapper" mkconvFunc :: ConvFunc -> IO (FunPtr ConvFunc)

instance Storable CPamConv where
    alignment _ = alignment (undefined :: CDouble)
    sizeOf _ = sizeOf (undefined :: FunPtr ()) + sizeOf (undefined :: Ptr ())
    peek p = CPamConv <$> ({#get pam_conv.conv #} p)
                      <*> ({#get pam_conv.appdata_ptr #} p)
    poke p (CPamConv c ap) = do
        {#set pam_conv.conv #} p c
        {#set pam_conv.appdata_ptr #} p ap

type CPamHandleT = ()

foreign import ccall "security/pam_appl.h pam_start" c_pam_start :: CString -> CString -> Ptr CPamConv -> Ptr (Ptr CPamHandleT) -> IO CInt
foreign import ccall "security/pam_appl.h pam_end" c_pam_end :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_authenticate" c_pam_authenticate :: Ptr CPamHandleT -> CInt -> IO CInt
foreign import ccall "security/pam_appl.h pam_acct_mgmt" c_pam_acct_mgmt :: Ptr CPamHandleT -> CInt -> IO CInt
