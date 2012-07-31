module Spear.Setup
(
    Setup
,   Resource
,   register
,   release
,   runSetup
,   runSetup_
,   setupError
,   setupIO
)
where


import Control.Monad.Error
import qualified Control.Monad.Resource as R
import qualified Control.Monad.Trans.Class as MT (lift)


type Setup = R.ResourceT (ErrorT String IO)

type Resource = R.ReleaseKey


-- | Register the given cleaner.
register :: IO () -> Setup Resource
register = R.register


-- | Release the given 'Resource'.
release :: Resource -> Setup ()
release = R.release


-- | Run the given 'Setup', freeing all of its allocated resources.
runSetup :: Setup a -> IO (Either String a)
runSetup = runErrorT . R.runResourceT


-- | Run the given 'Setup', freeing all of its allocated resources.
runSetup_ :: Setup a -> IO ()
runSetup_ s = (runErrorT . R.runResourceT) s >> return ()


-- | Throw an error from the 'Setup' monad.
setupError :: String -> Setup a
setupError = MT.lift . throwError


-- | Lift the given IO action into the 'Setup' monad.
setupIO :: IO a -> Setup a
setupIO = MT.lift . MT.lift
