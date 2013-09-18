import Control.Concurrent ( threadDelay )
import Control.Monad ( unless )
import System.IO ( hPutStrLn, stderr )
import Sound.OpenAL

main :: IO ()
main = do
   maybeDevice <- openDevice (Just "'( ( devices '( native null ) ) )")
   case maybeDevice of
      Nothing -> hPutStrLn stderr "openDevice failed"
      Just device -> do
         threadDelay 1000000
         ok <- closeDevice device
         unless ok $
            hPutStrLn stderr "closeDevice failed"
