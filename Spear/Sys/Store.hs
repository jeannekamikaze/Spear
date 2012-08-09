module Spear.Sys.Store
(
    Store
,   Index
,   emptyStore
,   store
,   storel
,   storeFree
,   storeFreel
,   element
,   setElement
,   withElement
)
where


import Data.List as L (find)
import Data.Maybe (isJust, isNothing)
import Data.Vector as V
import Control.Monad.State -- test
import Text.Printf -- test


type Index = Int


data Store a = Store
    { objects :: Vector (Maybe a) -- ^ An array of objects.
    , last    :: Index -- ^ The greatest index assigned so far.
    }
    deriving Show


instance Functor Store where
    fmap f (Store objects last) = Store (fmap (fmap f) objects) last


-- | Create an empty store.
emptyStore :: Store a
emptyStore = Store V.empty (-1)


-- | Store the given element in the store.
store :: a -> Store a -> (Index, Store a)
store elem s@(Store objects last) =
    if last == V.length objects - 1
    then case findIndex isNothing objects of
        Just i  -> assign i elem s
        Nothing -> store elem $ Store (objects V.++ V.replicate (max 1 last + 1) Nothing) last
    else
        assign (last+1) elem s


-- Assign a slot the given element in the store.
assign :: Index -> a -> Store a -> (Index, Store a)
assign i elem (Store objects last) =
    let objects' = objects // [(i,Just elem)]
    in (i, Store objects' (max last i))


-- | Store the given elements in the store.
storel :: [a] -> Store a -> ([Index], Store a)
storel elems s@(Store objects last) =
    let n = Prelude.length elems
        (count, slots) = freeSlots objects
    in
       let -- place count elements in free slots.
            (is, s'') = storeInSlots slots (Prelude.take count elems) s
            
            -- append the remaining elements
            (is', s') = append (Prelude.drop count elems) s''
        in
            (is Prelude.++ is', s')


-- Count and return the free slots.
freeSlots :: Vector (Maybe a) -> (Int, Vector Int)
freeSlots v = let is = findIndices isNothing v in (V.length is, is) 


-- Store the given elements in the given slots.
-- Pre: valid indices.
storeInSlots :: Vector Int -> [a] -> Store a -> ([Index], Store a)
storeInSlots is elems (Store objects last) =
    let objects' = V.update_ objects is (V.fromList $ fmap Just elems)
        last' = let i = V.length is - 1
                in if i < 0 then last else max last $ is ! i
    in
        (V.toList is, Store objects' last')


-- Append the given elements to the last slot of the store, making space if necessary.
append :: [a] -> Store a -> ([Index], Store a)
append elems (Store objects last) =
    let n = Prelude.length elems
        indices = [last+1..last+n]
        objects'' = if V.length objects <= last+n
                    then objects V.++ V.replicate n Nothing
                    else objects
        objects'  = objects'' // (Prelude.zipWith (,) indices (fmap Just elems))
    in
        (indices, Store objects' $ last+n)


-- | Free the given slot.
storeFree :: Index -> Store a -> Store a
storeFree i (Store objects last) =
    let objects' = objects // [(i,Nothing)]
    in  if i == last
        then case findLastIndex isJust objects' of
            Just j  -> Store objects' j
            Nothing -> Store objects' 0
        else
            Store objects' last


findLastIndex :: (a -> Bool) -> Vector a -> Maybe Index
findLastIndex p v = findLastIndex' p v Nothing 0
    where
        findLastIndex' p v current i =
            if i >= V.length v then current
            else if p $ v V.! i then let x = Just i in x `seq` findLastIndex' p v x (i+1)
            else findLastIndex' p v current (i+1) 


-- | Free the given slots.
storeFreel :: [Index] -> Store a -> Store a
storeFreel is (Store objects last) =
    let objects' = objects // Prelude.zipWith (,) is (repeat Nothing)
        last' = case L.find (==last) is of
                    Nothing -> last
                    Just _  -> case findLastIndex isJust objects' of
                        Just j  -> j
                        Nothing -> (-1)
    in
        Store objects' last'


-- | Access the element in the given slot.
element :: Index -> Store a -> Maybe a
element index (Store objects _) = objects V.! index


-- | Set the element in the given slot.
setElement :: Index -> a -> Store a -> Store a
setElement index elem s = s { objects = objects s // [(index,Just elem)] }


-- | Apply a function to the element in the given slot.
withElement :: Index -> Store a -> (a -> a) -> Store a
withElement index store f = store { objects = objects' }
    where
        objects' = objects store // [(index, obj')]
        obj' = case element index store of
            Nothing -> Nothing
            Just x  -> Just $ f x


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

