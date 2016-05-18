fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs :: [Integer]
fibs = map fib [0..]

fibs2 :: [Integer]
fibs2 = fib 0 1
  where fib x y = x : fib y (x+y)

data Stream a = Elem a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Elem x y) = x : streamToList y

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Elem x (streamRepeat x)

streamMap :: ( a -> b ) -> Stream a -> Stream b
streamMap f (Elem x y) = Elem (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Elem x (streamFromSeed f x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleavesStreams :: Stream a -> Stream a -> Stream a
interleavesStreams =
