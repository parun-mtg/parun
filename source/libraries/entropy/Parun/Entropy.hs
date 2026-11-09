{-# LANGUAGE CPP #-}

-- | Random bytes from the operating system's cryptographically secure
-- generator: @/dev/urandom@ on POSIX, @BCryptGenRandom@ on Windows.
module Parun.Entropy where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Parun.Exceptions.EntropyUnavailable as EntropyUnavailable
#if defined(mingw32_HOST_OS)
import qualified Data.Int as Int
import qualified Data.Word as Word
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Foreign.Ptr as Ptr
#else
import qualified System.IO as IO
#endif

-- | How many bytes 'seed' draws: a ChaCha20 key's 256 bits.
size :: Int
size = 32

-- | 'size' random bytes, or 'EntropyUnavailable.EntropyUnavailable' thrown.
seed :: IO ByteString.ByteString
seed = do
  bytes <- draw
  Monad.when (ByteString.length bytes /= size) $
    Exception.throwIO (EntropyUnavailable.MkEntropyUnavailable (Text.pack ("drew " <> show (ByteString.length bytes) <> " bytes of " <> show size)))
  pure bytes

#if defined(mingw32_HOST_OS)
draw :: IO ByteString.ByteString
draw = Alloc.allocaBytes size $ \buffer -> do
  status <- bCryptGenRandom Ptr.nullPtr buffer 32 systemPreferredRng
  Monad.unless (status == 0) $
    Exception.throwIO (EntropyUnavailable.MkEntropyUnavailable (Text.pack ("BCryptGenRandom returned NTSTATUS " <> show status)))
  ByteString.packCStringLen (Ptr.castPtr buffer, size)

-- | BCRYPT_USE_SYSTEM_PREFERRED_RNG: no algorithm handle, the system's own
-- generator.
systemPreferredRng :: Word.Word32
systemPreferredRng = 2

foreign import ccall unsafe "BCryptGenRandom"
  bCryptGenRandom :: Ptr.Ptr () -> Ptr.Ptr Word.Word8 -> Word.Word32 -> Word.Word32 -> IO Int.Int32
#else
draw :: IO ByteString.ByteString
draw = IO.withBinaryFile "/dev/urandom" IO.ReadMode $ \handle -> ByteString.hGet handle size
#endif
