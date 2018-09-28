{-# LANGUAGE InstanceSigs #-}

module HW4 where

import Data.Monoid hiding (All, Any, (<>))
import Data.Semigroup hiding (All, Any)



{- 4.1 -- Colorful semigroups -}

{-
The Color type enumerates the primary and secondary colors. The primary
colors are red, blue, and yellow. The rules regarding how these colors
combine is given by the following poem written by yours truly:

  When you mix yellow with blue, there will fall a great green dew.
  When you mix blue with red, all will be purple with dread.
  When you mix red with yellow, you'll be eating orange Jell-o.
-}

data Color
  = Red
  | Blue
  | Yellow
  | Purple
  | Green
  | Orange
  | Indeterminate
  deriving (Eq, Show)

{-
Your job is to write a Semigroup instance for the Color type according
to how the colors can be mixed. As a reminder, we mostly referred to
Semigroup in lecture as Mergeable. They are two names for the same
concept, one coming from abstract algebra and the other coming from
intuition (I'll let you guess which is which).

In addition to the poem, make sure your implementation follows these rules:

  1. Mixing colors is reflexive, meaning that mixing any color with itself
     will give you back that same color.
  2. Mixing colors is symmetric, meaning that if mixing red with blue
     gives you purple then mixing blue with red will also give you purple.
  3. Any combination of colors not covered above results in indeterminate.
  4. Likewise, mixing anything with indeterminate will give you indeterminate.

One way to do this is by enumerating all 49 possible combinations of colors
by hand, but that is tedious and error-prone. For full credit, reduce the
number of cases you need to consider to at most 10 using the above rules.
We filled in the first case to get you started.
-}

instance Semigroup Color where
  (<>) :: Color -> Color -> Color
  Red <> Blue = Purple
  _   <> _    = undefined



{- 4.2 -- Your very own Int type -}

{-
TallyMark models a unary representation of the positive integers. It is
semantically equivalent to the PosInt definition from lec5 but we changed
the naming to add clarity given the context in which we will be using it.

Note that TallyMark derives the Eq and Ord typeclasses. That means
we can immediately use (==), (<=), (>), min, etc. with TallyMark
as well as functions that depend on those functions. Take a minute
to make sure that these definitions work as expected. In particular,
consider what would happen if we switched the order of the constructors.
-}

data TallyMark
  = One
  | OnePlus TallyMark
  deriving (Eq, Ord)

{-
Additionally, we defined a nicer instance of Show for you so that a
TallyMark will be printed as groups of 5 pipe characters. Compare
this instance to the one that deriving Show would give you.
-}

instance Show TallyMark where
  show :: TallyMark -> String
  show One = "|"
  show (OnePlus One) = "||"
  show (OnePlus (OnePlus One)) = "|||"
  show (OnePlus (OnePlus (OnePlus One))) = "||||"
  show (OnePlus (OnePlus (OnePlus (OnePlus One)))) = "|||||"
  show (OnePlus (OnePlus (OnePlus (OnePlus (OnePlus n))))) = "||||| " ++ show n

{-
MyInt will consist of either Zero, some number of tally marks marked
with either the Pos constructor, or some number of tally marks marked
with the Neg constructor. We can use this type to represent any
integer-- positive, negative, or otherwise!
-}

data MyInt
  = Neg TallyMark
  | Zero
  | Pos TallyMark
  deriving (Eq)

{-
Look at the below definitions (and fill in the blank ones) to make sure
that you understand how we are representing the integers.
-}

positiveThree, zero, negativeOne :: MyInt
positiveThree = undefined
zero = Zero
negativeOne = Neg One

{-
We are now going to write instances of some common typeclasses for MyInt.

The first one is Show, which we give you at no cost. Recall that although
we are calling show in the definition of show, this definition is not
actually recursive since they are different shows! The show that we are
calling in the definition is for the TallyMark type. To help you remember
that, we added a type annotation to n. (Yes, you can put annotations
smack in the middle of an expression.) This annotation is not necessary,
but it does help the reader disambiguate the show we are calling.
-}

instance Show MyInt where
  show :: MyInt -> String
  show (Neg n) = "- " ++ show (n :: TallyMark)
  show Zero    = "z"
  show (Pos n) = "+ " ++ show (n :: TallyMark)

{-
You will now write an Ord instance for MyInt. Why not derive it you ask?
Try it out and see what happens when you compare two negative numbers.
-}

instance Ord MyInt where
  compare :: MyInt -> MyInt -> Ordering
  compare = undefined



{- 4.3 -- The Knights Who Say Num -}

{-
Our next goal is to define a Num instance for MyInt. Since the minimal
complete definition of Num requires defining six functions, we are
going to help you break this task up into several smaller steps. We
begin by defining a few functions that operate only on TallyMarks.
-}

{-
Add two positive integers returning the sum as a TallyMark.

    tallyMarkPlus One One
      == OnePlus One
    tallyMarkPlus One (OnePlus One)
      == (OnePlus . OnePlus $ One)
    tallyMarkPlus (OnePlus (OnePlus One)) (OnePlus One)
      == (OnePlus . OnePlus . OnePlus . OnePlus $ One)
-}

tallyMarkPlus :: TallyMark -> TallyMark -> TallyMark
tallyMarkPlus = undefined

{-
Multiply two positive integers returning the product as a TallyMark.

    tallyMarkTimes One (OnePlus One) == (OnePlus One)
    tallyMarkTimes (OnePlus One) (OnePlus One)
      == (OnePlus . OnePlus . OnePlus $ One)

HINT: Keep in mind the identity, n * m = m + (n - 1) * m.
-}

tallyMarkTimes :: TallyMark -> TallyMark -> TallyMark
tallyMarkTimes = undefined

{-
Subtract two positive integers returning the difference as a MyInt.
Note that while the input is only positive integers, the return
type is MyInt. Why can't we return a TallyMark in this case? If
we subtract a number from itself, the difference will be zero which
is not representable as a TallyMark. Likewise, if we subtract a
larger number from a smaller one, the difference will be negative
which is also not representable as a TallyMark.

    tallyMarkMinus (OnePlus One) One == Pos One
    tallyMarkMinus (OnePlus One) (OnePlus One) == Zero
    tallyMarkMinus (OnePlus One) (OnePlus . OnePlus $ One) == Neg One
-}

tallyMarkMinus :: TallyMark -> TallyMark -> MyInt
tallyMarkMinus = undefined

{-
And now the moment you've been waiting for...

Refer to http://bit.ly/haskell-docs-num for more information about
the functions you need to implement and laws of the Num typeclass.

fromInteger is on us. For (+) and (*) in particular, you should
look back at the tallyMark* functions you defined above.
-}

instance Num MyInt where
  (+) :: MyInt -> MyInt -> MyInt
  (+) = undefined

  (*) :: MyInt -> MyInt -> MyInt
  (*) = undefined

  abs :: MyInt -> MyInt
  abs = undefined

  signum :: MyInt -> MyInt
  signum = undefined

  negate :: MyInt -> MyInt
  negate = undefined

  fromInteger :: Integer -> MyInt
  fromInteger 0 = Zero
  fromInteger n
    | n > 0     = Pos . toTallyMark $ n
    | otherwise = Neg . toTallyMark . abs $ n
    where
      toTallyMark 1 = One
      toTallyMark n = OnePlus . toTallyMark $ n - 1



{- 4.4 -- Will the real instance please stand up? -}

{-
Suppose we try to write a Monoid instance for Bool.

We come up with

    instance Semigroup Bool where
      (<>) :: Bool -> Bool -> Bool
      (<>) = (&&)

    instance Monoid Bool where
      mempty :: Bool
      mempty = True

while our friend comes up with

    instance Semigroup Bool where
      (<>) :: Bool -> Bool -> Bool
      (<>) = (||)

    instance Monoid Bool where
      mempty :: Bool
      mempty = False

Which is the correct Monoid instance for Bool? Turns out they are both
correct! How can we have two Monoid instances of the same type though?!
If I create both, I run into a problem called overlapping instances.
The trick is to make two wrapped versions of the Bool type.

Here we use the newtype keyword. It is like the data keyword except that
it only works for custom types that have exactly one value constructor
and that constructor must take a single argument. The advantage is that
newtype is more performant. It allows the compiler to eliminate all the
wrapping and unwrapping once the code is typechecked. If you find this
paragraph confusing, just mentally replace newtype with data.

The other interesting thing to note is that we cannot write a Monoid
instance without a corresponding Semigroup instance as of GHC 8.4
(which is what most of you are using). If we think about it, this
actually makes sense. All Monoids have an identity element, mempty,
and a binary merge operation, mappend, while for a Semigroup instance
you only need a binary merge operation, namely (<>). Looking at the
Haskell docs, we can also check that mappend and (<>) follow the same
laws and learn that mappend and (<>) should be functionally equivalent.
One plus that we get from this is that we do not need to define mappend.
The compiler will just use the default definition: mappend = (<>).
-}

newtype All
  = All Bool
  deriving (Eq, Ord, Show)

newtype Any
  = Any Bool
  deriving (Eq, Ord, Show)

instance Semigroup All where
  (<>) :: All -> All -> All
  All b <> All c = All $ b && c

instance Monoid All where
  mempty :: All
  mempty = All True

instance Semigroup Any where
  (<>) :: Any -> Any -> Any
  Any b <> Any c = Any $ b || c

instance Monoid Any where
  mempty :: Any
  mempty = Any False

{-
Based off the above example, write two instances of Monoid for MyInt
using the wrapped types MySum and MyProduct to avoid overlapping instances.
-}

newtype MySum
  = MySum MyInt
  deriving (Eq, Ord, Show)

newtype MyProduct
  = MyProduct MyInt
  deriving (Eq, Ord, Show)

{-
Write the Semigroup and Monoid instances for MySum.

    MySum (Pos . OnePlus . OnePlus $ One) `mappend` MySum (Pos . OnePlus $ One)
      == MySum (Pos . OnePlus . OnePlus . OnePlus . OnePlus $ One)
-}

instance Semigroup MySum where
  (<>) :: MySum -> MySum -> MySum
  _ <> _ = undefined

instance Monoid MySum where
  mempty :: MySum
  mempty = undefined


{-
Write the Semigroup and Monoid instances for MyProduct.

    MyProduct (Pos . OnePlus $ One) `mappend` MyProduct Zero == MyProduct Zero
-}

instance Semigroup MyProduct where
  (<>) :: MyProduct -> MyProduct -> MyProduct
  _ <> _ = undefined

instance Monoid MyProduct where
  mempty :: MyProduct
  mempty = undefined
