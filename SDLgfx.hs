{-# LANGUAGE ForeignFunctionInterface #-}

module SDLgfx where

import Foreign (Int16, Word8, Ptr, withForeignPtr)
import Foreign.C (CInt(..) )
import Graphics.UI.SDL.Utilities (intToBool, fromCInt)
import qualified Graphics.UI.SDL as SDL

foreign import ccall unsafe "boxRGBA" gfxBoxRGBA ::
	Ptr SDL.SurfaceStruct ->
	Int16 -> Int16 -> Int16 -> Int16 ->
	Word8 -> Word8 -> Word8 -> Word8 ->
	IO CInt

boxAlpha :: SDL.Surface -> SDL.Rect -> SDL.Color -> Word8 -> IO Bool
boxAlpha surface (SDL.Rect x y w h) (SDL.Color r g b) a = withForeignPtr surface (\ptr ->
		intToBool (-1) (fmap fromCInt $ gfxBoxRGBA ptr (fromIntegral x) (fromIntegral y) (fromIntegral $ x+w) (fromIntegral $ y+h) r g b (fromIntegral a))
	)
