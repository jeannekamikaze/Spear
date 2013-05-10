module Spear.Sys.Store.ID
(
    ID   
,   IDStore
,   emptyIDStore
,   newID
,   freeID
)
where


import Data.Vector.Unboxed as U
import Control.Monad.State -- test
import Text.Printf -- test


type ID = Int


data IDStore = IDStore
    { assigned :: Vector Bool -- ^ A bit array indicating used IDs.
    , last     :: Int -- ^ The greatest ID assigned so far.
    }
    deriving Show


-- | Create an empty ID store.
emptyIDStore :: IDStore
emptyIDStore = IDStore U.empty (-1)


-- | Request an ID from the ID store.
newID :: IDStore -> (ID, IDStore)
newID store@(IDStore assigned last) =
    if last == U.length assigned - 1
    then case findIndex (==False) assigned of
        Just i  -> assign i store
        Nothing -> newID $ IDStore (assigned U.++ U.replicate (max 1 last + 1) False) last
    else
        assign (last+1) store


-- Assign the given ID in the ID store.
assign :: ID -> IDStore -> (ID, IDStore)
assign i (IDStore assigned last) =
    let assigned' = assigned // [(i,True)]
    in (i, IDStore assigned' (max last i))


-- | Free the given ID from the ID store.
freeID :: ID -> IDStore -> IDStore
freeID i (IDStore assigned last) =
    let assigned' = assigned // [(i,False)]
    in  if i == last
        then case findLastIndex (==True) assigned' of
            Just j  -> IDStore assigned' j
            Nothing -> IDStore assigned' 0
        else
            IDStore assigned' last


findLastIndex :: Unbox a => (a -> Bool) -> Vector a -> Maybe Int
findLastIndex p v = findLastIndex' p v Nothing 0
    where
        findLastIndex' p v current i =
            if i >= U.length v then current
            else if p $ v U.! i then let x = Just i in x `seq` findLastIndex' p v x (i+1)
            else findLastIndex' p v current (i+1) 


-- test
test :: IO ()
test = evalStateT test' emptyIDStore


test' :: StateT IDStore IO ()
test' = do
    x <- request
    y <- request
    z <- request
    w <- request
    free y
    request
    free w
    request
    a <- request
    free a
    request
    return ()


request :: StateT IDStore IO ID
request = do
    store <- get
    let (i, store') = newID store
    put store'
    lift $ printf "ID requested, got %d; %s\n" i (show store')
    return i


free :: ID -> StateT IDStore IO ()
free i = do
    store <- get
    let store' = freeID i store
    put store'
    lift $ printf "ID %d freed; %s\n" i (show store')
