-- State Transformer exercise

-- 1. write a function to relabel a Tree with Unique number 
--          data Tree a = Leaf a | Node (Tree a) (Tree a)
 
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show 

newtype State s a = State {runState :: s -> (a, s)}

instance Functor Tree where 
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (g <$> l) (g <$> r)


instance Applicative Tree where 
    -- pure :: a -> Tree a
    pure x = Leaf x

    -- (<*>) :: Tree (a -> b) -> Tree a -> Tree b 
    Leaf f <*> t2 = f <$> t2 
    (Node l _) <*> Leaf x =  l <*> pure x
    (Node l1 r1) <*> (Node l2 r2) = Node (l1 <*> l2) (r1 <*> r2)


instance Monad  Tree where 
    -- return :: a -> Tree a 
    return = pure

    -- (>>=) :: Tree a -> (a -> Tree b) -> Tree c
    (Leaf x) >>= g =  g x
    (Node l r) >>= g = Node (l >>= g) (r >>= g)


-- defining Functor for State
instance Functor (State s) where 
    -- fmap :: (a -> b) -> State s a -> State s b
    fmap g st = State (\oldState -> let (x, newState) = runState st oldState in (g x, newState)) 

instance Applicative (State s) where 
    -- pure :: a -> State s a
    pure x = State (\s -> (x, s))

    -- (<*>) :: State s (a -> b) -> State s a -> State s b
    s1 <*> s2 = State (\oldState -> 
        let (fs1, newState) = runState s1 oldState
            (x, state2) =  runState s2 newState
        in (fs1 x, state2))


instance Monad (State s) where
    -- return :: a -> State s a
    return  = pure

    -- (>>=) :: State s a -> (a -> State s b) -> State s c
    m >>= k = State (\oldState -> let (x, new_s1) = runState m oldState in runState (k x) new_s1) 
    
sampleTree :: Tree Char 
sampleTree = Node (Node (Leaf 'A') (Leaf 'B')) (Leaf 'C')

inc :: State Int Int
inc = State (\n -> (n, n+ 1))

relabel :: Tree a -> State Int (Tree Int)
relabel (Leaf x) = do
    n <- inc
    return (Leaf n)

relabel (Node l r) = 
    do l' <- relabel l
       r' <- relabel r
       return (Node l' r')