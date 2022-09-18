module Spear.Game
  ( Game,
    GameException (..),
    Resource,
    ResourceClass (..),

    -- * Game state
    getGameState,
    saveGameState,
    modifyGameState,

    -- * Game resources
    register,
    unregister,

    -- * Error handling
    gameError,
    assertMaybe,
    catchGameError,
    catchGameErrorFinally,

    -- * Running and IO
    runGame,
    runGame',
    runSubGame,
    runSubGame',
    evalSubGame,
    execSubGame,
    gameIO,
  )
where

import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Resource as R

type Resource = R.ReleaseKey

type Game s = StateT s (R.ResourceT IO)

class ResourceClass a where
  getResource :: a -> Resource

  release :: a -> Game s ()
  release = unregister . getResource

  clean :: a -> IO ()
  clean = R.release . getResource

newtype GameException = GameException String deriving (Show)

instance Exception GameException

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
gameError = gameError' . GameException

-- | Throw an error from the 'Game' monad.
gameError' :: GameException -> Game s a
gameError' = lift . lift . throwM

-- | Throw the given error if given 'Nothing'.
assertMaybe :: Maybe a -> GameException -> Game s a
assertMaybe Nothing err = gameError' err
assertMaybe (Just x) _ = return x

-- | Run the given game with the given error handler.
catchGameError :: Game s a -> (GameException -> Game s a) -> Game s a
catchGameError = catch

-- | Run the given game, catch any error, run the given finaliser and rethrow the error.
catchGameErrorFinally :: Game s a -> Game s a -> Game s a
catchGameErrorFinally game finally = catch game $ \err -> finally >> gameError' err

-- | Run the given game.
runGame :: Game s a -> s -> IO (a, s)
runGame game = R.runResourceT . runStateT game

-- | Run the given game and discard its state.
runGame' :: Game s a -> s -> IO a
runGame' g s = fst <$> runGame g s

-- | Fully run the given sub game, unrolling the entire monad stack.
runSubGame :: Game s a -> s -> Game t (a, s)
runSubGame g s = gameIO $ runGame g s

-- | Like 'runSubGame', but discarding the result.
runSubGame' :: Game s a -> s -> Game t ()
runSubGame' g s = void $ runSubGame g s

-- | Run the given game and return its result.
evalSubGame :: Game s a -> s -> Game t a
evalSubGame g s = fst <$> runSubGame g s

-- | Run the given game and return its state.
execSubGame :: Game s a -> s -> Game t s
execSubGame g s = snd <$> runSubGame g s

-- | Perform the given IO action in the 'Game' monad.
gameIO :: IO a -> Game s a
gameIO = lift . lift
