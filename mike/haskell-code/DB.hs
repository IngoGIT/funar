module DB where

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x+y))
-}

type Key = String
type Value = Integer

{-
data DBCommand result =
    Put Key Value 
  | Get Key
  | Return result

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike" (x+1)]
-}

data DB a =
    Get Key (Value -> DB a)
  | Put Key Value (() -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

put :: Key -> Value -> DB ()
put key value  = Put key value Return

get :: Key -> DB Value
get key = Get key Return

-- return :: a -> DB a
-- return = Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next = 
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = splice (put "Mike" 100) (\() -> 
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show(x+y))))))

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

instance Applicative DB where
    pure = Return

instance Monad DB where
    (>>=) = splice -- "bind"
--    return = Return

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show(x+y))