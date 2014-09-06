{-# LANGUAGE ForeignFunctionInterface #-}

module XMonad.Util.XKB ( setKbdGroup
                       , getKbdGroup
                       , getKbdGroups
                       , Group(..)) where

import Foreign
import Foreign.C.Types
import Control.Applicative
import Control.Monad
import XMonad ( X, Atom, Status,  Display(..), openDisplay, getAtomName, withDisplay, io )

#include <X11/XKBlib.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

foreign import ccall unsafe "X11/XKBlib.h XkbGetKeyboard"
  xkbGetKeyboard :: Display -> CUInt -> CUInt -> IO (Ptr XkbDescRec)

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
  xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO Status

foreign import ccall unsafe "X11/XKBlib.h XkbLockGroup"
  xkbLockGroup :: Display -> CUInt -> CUInt -> IO Bool

data XkbStateRec =
  XkbStateRec { group              :: CUChar
              , base_group         :: CUChar
              , latched_group      :: CUChar
              , locked_group       :: CUChar
              , mods               :: CUChar
              , base_mods          :: CUChar
              , latched_mods       :: CUChar
              , locked_mods        :: CUChar
              , compat_state       :: CUChar
              , grab_mods          :: CUChar
              , compat_grab_mods   :: CUChar
              , lookup_mods        :: CUChar
              , compat_lookup_mods :: CUChar
              , ptr_buttons        :: CUShort }

data XkbDescRec = XkbDescRec   { names  :: Ptr XkbNamesRec } 

data XkbNamesRec = XkbNamesRec { groups :: [Atom]
                               , syms   :: Atom }

instance Storable XkbStateRec where
  sizeOf    _ = (#size XkbStateRec)
  alignment _ = (#alignment XkbDescRec)
  peek ptr = do
    group              <- (#peek XkbStateRec, group) ptr
    locked_group       <- (#peek XkbStateRec, locked_group) ptr
    base_group         <- (#peek XkbStateRec, base_group) ptr
    latched_group      <- (#peek XkbStateRec, latched_group) ptr
    mods               <- (#peek XkbStateRec, mods) ptr
    base_mods          <- (#peek XkbStateRec, base_mods) ptr
    latched_mods       <- (#peek XkbStateRec, latched_mods) ptr
    locked_mods        <- (#peek XkbStateRec, locked_mods) ptr
    compat_state       <- (#peek XkbStateRec, compat_state) ptr
    grab_mods          <- (#peek XkbStateRec, grab_mods) ptr
    compat_grab_mods   <- (#peek XkbStateRec, compat_grab_mods) ptr
    lookup_mods        <- (#peek XkbStateRec, lookup_mods) ptr
    compat_lookup_mods <- (#peek XkbStateRec, compat_lookup_mods) ptr
    ptr_buttons        <- (#peek XkbStateRec, ptr_buttons) ptr

    return $ XkbStateRec group locked_group base_group latched_group      
                         mods base_mods latched_mods locked_mods
                         compat_state grab_mods compat_grab_mods
                         lookup_mods compat_lookup_mods ptr_buttons        

instance Storable XkbDescRec where
  sizeOf _    = (#size XkbDescRec)
  alignment _ = (#alignment XkbDescRec)
  peek ptr = do
    names <- (#peek XkbDescRec, names) ptr
    return $ XkbDescRec names 

instance Storable XkbNamesRec where
  sizeOf _    = (#size XkbNamesRec)
  alignment _ = (#alignment XkbNamesRec)
  peek ptr = do
    groups <- peekArray 4 $ (#ptr XkbNamesRec, groups) ptr
    syms   <- (#peek XkbNamesRec, symbols) ptr
    return $ XkbNamesRec groups syms

getKbdGroups :: Display -> IO [Maybe String]
getKbdGroups dpy = do
  xkbDescPtr <- xkbGetKeyboard dpy 0x7f (#const XkbUseCoreKbd)
  xkbDesc    <- peek xkbDescPtr
  xkbNames   <- (peek . names) $ xkbDesc

  let xkbGroups = groups xkbNames

  mapM (getAtomName dpy >=> return) . filter (/=0) $ xkbGroups
  
getKbdGroup :: Display -> IO CUChar
getKbdGroup d = alloca $ \stPtr -> do
  xkbGetState d (#const XkbUseCoreKbd) stPtr
  group <$> peek stPtr


data Group = FirstGroup | SecondGroup | ThirdGroup | FourthGroup

setKbdGroup :: Group -> Display -> IO Bool
setKbdGroup group dpy =
  xkbLockGroup dpy (#const XkbUseCoreKbd) $ case group of
    FirstGroup  -> 0
    SecondGroup -> 1
    ThirdGroup  -> 2
    FourthGroup -> 3

