module Spear.Game
(
    Game
,   gameIO
,   getGameState
,   saveGameState
,   modifyGameState
,   runGame
)
where


import Control.Monad.Trans.Class (lift)
import Control.Monad.State.Strict


type Game s = StateT s IO


-- | Perform the given IO action in the 'Game' monad.
gameIO :: IO a -> Game s a
gameIO = lift


-- | Retrieve the game state.
getGameState :: Game s s
getGameState = get


-- | Save the game state.
saveGameState :: s -> Game s ()
saveGameState = put


-- | Modify the game state.
modifyGameState :: (s -> s) -> Game s ()
modifyGameState = modify


-- | Run the given game.
runGame :: Game s a -> s -> IO ()
runGame game state = runStateT game state >> return ()
