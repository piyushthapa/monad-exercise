-- Redefine Functors, Applicative, Monad for different types 
--  Maybe, List, Tree etc


data MaybeNew a = JustNew a | NothingNew

-- functor instance for MaybeNew
instance Functor MaybeNew where
    -- fmap :: (a ->b) -> MaybeNew a -> MaybeNew b
    fmap g NothingNew = NothingNew
    fmap g (JustNew x) = JustNew (g x)

-- Applicative instance for MaybeNew
instance Applicative MaybeNew where
    -- pure :: a -> JustNew a
    pure x = JustNew x

    -- (<*>) :: MaybeNew (a -> b) -> MaybeNew a -> MaybeNew b
    NothingNew <*> mx = NothingNew
    (JustNew g) <*> mx = fmap g mx


instance Monad MaybeNew where 
    -- (>>=) :: MaybeNew a -> (a -> MaybeNew b) -> MaybeNew b
    NothingNew >>= f = NothingNew
    (JustNew x) >>= f = f x

    -- return :: a -> MaybeNew a
    return = pure

-- representing [] with custom Data type
data List a = Empty | Cons a (List a)

-- display List data type as normal List
instance (Show a) =>  Show (List a) where
    show Empty = ""
    show (Cons x xs) = show x ++ ", " ++ show xs

instance Functor List where 
    -- fmap :: (a -> b) -> List a -> List b
    fmap g Empty = Empty
    fmap g (Cons x lst) = Cons (g x) (fmap g lst)

instance Applicative List where 
    -- pure :: a -> List a
    pure x = Cons x Empty

    -- (<*>) :: List (a -> b) -> List a -> List b
    Empty <*> lst = Empty
    Cons g _ <*> lst = fmap g lst

instance Monad List where
    -- (>>=) :: (a -> List a) -> List b -> List c
    Empty >>= f = Empty
    Cons a lst >>= f = concatList $ fmap f lst

-- concatList Cons (Cons 5 Empty) (Cons 10 (Cons 12 Empty)) = Cons 5 (Cons 10 (Cons 12 Empty)) 
concatList :: List (List a) -> List a 
concatList Empty = Empty
concatList (Cons xs xss) = extract xs (concatList xss)

extract :: List a -> List a -> List a
extract Empty ys = ys 
extract (Cons x xs) ys = Cons x (extract xs ys) 