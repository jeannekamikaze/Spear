module Spear.Game
(
    Game
,   Resource
,   ResourceClass(..)
    -- * Game State
,   getGameState
,   saveGameState
,   modifyGameState
    -- * Game Resources
,   register
,   unregister
,   gameError
,   assertMaybe
    -- * Running and IO
,   runGame
,   runGame'
,   evalSubGame
,   execSubGame
,   gameIO
)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict
import Control.Monad.Error
import qualified Control.Monad.Trans.Resource as R

type Resource = R.ReleaseKey
type Game s = StateT s (R.ResourceT (ErrorT String IO))

class ResourceClass a where
      getResource :: a -> Resource
      
      release :: a -> Game s ()
      release = unregister . getResource
      
      clean :: a -> IO ()
      clean = R.release . getResource

-- | Retrieve the game state.
getGameState :: Game s s
getGameState = get

-- | Save the game state.
saveGameState :: s -> Game s ()
saveGameState = put

-- | Modify the game state.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState = modify

-- | Register the given cleaner.
register :: IO () -> Game s Resource
register = lift . R.register

-- | Release the given 'Resource'.
unregister :: Resource -> Game s ()
unregister = lift . R.release

-- | Throw an error from the 'Game' monad.
gameError :: String -> Game s a
gameError = lift . lift . throwError

-- | Throw the given error string if given 'Nothing'.
assertMaybe :: Maybe a -> String -> Game s a
assertMaybe Nothing err = gameError err
assertMaybe (Just x) _  = return x

-- | Run the given game.
runGame :: Game s a -> s -> IO (Either String (a,s))
runGame game state = runErrorT . R.runResourceT . runStateT game $ state

-- | Run the given game.
runGame' :: Game s a -> s -> IO ()
runGame' game state = runGame game state >> return ()

-- | Run the given game and return its result.
evalSubGame :: Game s a -> s -> Game t a
evalSubGame g s = lift $ evalStateT g s

-- | Run the given game and return its state.
execSubGame :: Game s a -> s -> Game t s
execSubGame g s = lift $ execStateT g s

-- | Perform the given IO action in the 'Game' monad.
gameIO :: IO a -> Game s a
gameIO = lift . lift . lift
