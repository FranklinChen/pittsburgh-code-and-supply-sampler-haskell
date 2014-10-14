-- Sample Haskell

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

import Data.Foldable as Foldable
import Data.Traversable as Traversable
import qualified Data.Char as Char
import Control.Lens
import Control.Category ((>>>))
import Data.List (sort, sortBy)
import Control.Applicative ((<$>))

-- Type names must be Capitalized
dozen :: Int
dozen = 12

no :: Bool
no = False

-- Product (tuple) types
groceryItem :: (String, Int)
groceryItem = ("apple", 2)

-- Type synonym: syntactic sugar, not new type
type Item = (String, Int)

anotherItem :: Item
anotherItem = ("banana", 5)

-- Unit (0-tuple)
onlyOneOfThese :: ()
onlyOneOfThese = ()

-- For illustration only
type Email = String
type Phone = String
type Reason = String

-- Constructors: Yes has 2 fields, No has 1, Ignore has 0
data OptIn = Yes Email Phone | No Reason | Ignore

violateMyPrivacy :: OptIn
violateMyPrivacy = Yes "fake@fake.com" "111-1111"

data Info = Contact { email :: Email, phone :: Phone }

scaryAdd :: Bool -> Int -> Int -> Int
scaryAdd evil x y =
  if evil then
    x + y + 1
  else
    x + y

-- hypothetical back door
sendToNSA :: Int -> Int -> IO ()
sendToNSA = undefined

{-
unsafeAdd :: Int -> Int -> Int
unsafeAdd x y =
  -- Type error, must have effect
  sendToNSA x y
-}

notFour :: Int
notFour = scaryAdd True 2 2

sayHelloTo :: String -> String
sayHelloTo s = "hello " ++ s

helloWorld :: String
helloWorld = sayHelloTo "world"

good :: Either String Int
good = Right 42

bad :: Either String Int
bad = Left "no number entered"

{-
    data List a = Nil | Cons a (List a)

    -- Not legal Haskell but reflects special syntax
    -- with infix cons operator :
    data [a] = [] | a:[a]

    empty = []
    three = [1, 2, 3]
    threeDesugared = 1:(2:(3:[]))
-}

-- List OptIn
responses :: [OptIn]
responses = [
  Ignore,
  Yes "fake@fake.com" "111-1111",
  No "I hate spam"]

odds :: [Integer]
odds = [1, 3..]

sumFiveOdds :: Integer
sumFiveOdds = Prelude.sum (take 5 odds)

hugeList :: [Int]
hugeList = reverse [1..1000]

smallest2 = take 2 (sort hugeList)

{-
holeyGreeting = "hello " ++ _huh
-- Type checker says:
--
-- Found hole ‘_huh’ with type: [Char]

-- type error becomes warning in this mode
illTypedGreeting = "hello " ++ True
-- If program reaches this code, then runtime error:
--
-- Couldn't match expected type ‘[Char]’ with
-- actual type ‘Bool’
-}

main :: IO ()
main = do -- "begin block for IO context"
  s <- getLine -- "get String s within IO context"
  putStrLn ("hello " ++ s ++ "!!")

{-
oldGrandparents =
  [grandparent | parent <- [mother, father]
               , grandparent <- parentsOf parent
               , ageOf grandparent > 75]
-}

-- Operator section (/ 100.0)
-- <$> is infix map operator
percents :: [Float]
percents = (/ 100.0) <$> [58.8, 95.5]

-- Composition pipeline
addOneThenDouble :: [Int]
addOneThenDouble = ((+1) >>> (*2)) <$> [3, 4, 5]

type KeyValues key value = [(key, value)]

newtype UserName = U String

toLower :: String -> String
toLower = (Char.toLower <$>)

instance Eq UserName where
  U x == U y =
    toLower x == toLower y

itsTrue = U "Chen" == U "chen"

foundCity = lookup (U "chEn")
                   [(U "bob", "LAX"),
                    (U "chen", "PIT")]

-- We define
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Show, Functor, Foldable)

tree1 :: Tree Int
tree1 = Node (Node (Leaf 1) 2 (Leaf 3))
             4
             (Node (Leaf 5) 6 Empty)

added100 = (+100) <$> tree1

twentyOne = Foldable.sum tree1

oneToSix = Foldable.foldr (:) [] tree1
