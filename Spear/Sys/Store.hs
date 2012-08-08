module Spear.Sys.Store
(
    Store
,   Index
,   emptyStore
,   store
,   storeFree
,   element
)
where


import Data.Maybe (isJust, isNothing)
import Data.Vector as V
import Control.Monad.State -- test
import Text.Printf -- test


type Index = Int


data Store a = Store
    { assigned :: Vector (Maybe a) -- ^ An array of objects.
    , last     :: Index -- ^ The greatest index assigned so far.
    }
    deriving Show


-- | Create an empty store.
emptyStore :: Store a
emptyStore = Store V.empty (-1)


-- | Store the given element in the store.
store :: a -> Store a -> (Index, Store a)
store elem s@(Store assigned last) =
    if last == V.length assigned - 1
    then case findIndex isNothing assigned of
        Just i  -> assign i elem s
        Nothing -> store elem $ Store (assigned V.++ V.replicate (max 1 last + 1) Nothing) last
    else
        assign (last+1) elem s


-- Assign a slot the given element in the store.
assign :: Index -> a -> Store a -> (Index, Store a)
assign i elem (Store assigned last) =
    let assigned' = assigned // [(i,Just elem)]
    in (i, Store assigned' (max last i))


-- | Free the given element from the store.
storeFree :: Index -> Store a -> Store a
storeFree i (Store assigned last) =
    let assigned' = assigned // [(i,Nothing)]
    in  if i == last
        then case findLastIndex isJust assigned' of
            Just j  -> Store assigned' j
            Nothing -> Store assigned' 0
        else
            Store assigned' last


findLastIndex :: (a -> Bool) -> Vector a -> Maybe Index
findLastIndex p v = findLastIndex' p v Nothing 0
    where
        findLastIndex' p v current i =
            if i >= V.length v then current
            else if p $ v V.! i then let x = Just i in x `seq` findLastIndex' p v x (i+1)
            else findLastIndex' p v current (i+1) 


-- | Access the element in the given slot.
element :: Index -> Store a -> Maybe a
element index (Store assigned _) = assigned V.! index


-- test
test :: IO ()
test = evalStateT test' emptyStore


test' :: StateT (Store Int) IO ()
test' = do
    x <- store' 1
    y <- store' 2
    z <- store' 3
    w <- store' 4
    free y
    store' 5
    free w
    store' 6
    a <- store' 7
    free a
    store' 8
    return ()


store' :: Int -> StateT (Store Int) IO Int
store' elem = do
    s <- get
    let (i, s') = store elem s
    put s'
    lift $ printf "%d stored at %d; %s\n" elem i (show s')
    return i


free :: Index -> StateT (Store Int) IO ()
free i = do
    s <- get
    let s' = storeFree i s
    put s'
    lift $ printf "Slot %d freed; %s\n" i (show s')

