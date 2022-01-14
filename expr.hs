-- Monad exercise given by University of Southern Denmark
-- https://shaagerup.github.io/dm552/files/MonadsLab.pdf

-- 1)  create Monad for 
-- data Expr a = Var a | Add (Expr a) (Expr a) deriving Show

data Expr a = Var a | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap g (Var a) = Var (g a)
    fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where 
    -- pure :: a -> Expr a 
    pure x = Var x

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    Var g <*> expr = fmap g expr
    (Add g _) <*> expr  =  g <*> expr


instance Monad Expr where
    -- return :: a -> Expr a 
    return = pure

    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= f = f x
    (Add x y) >>= f = Add (x >>= f) (y >>= f)


replace :: Eq a => [(a, b)]  -> Expr a -> Expr (Maybe b) 
replace [] _ = return  Nothing 
replace xs mx = mx >>= (\x -> return (lookup x xs))


-- convert :: Expr (Maybe a) -> Maybe (Expr a)
-- convert expr = expr >>= (\x -> (x >>= (\e -> return e)))

