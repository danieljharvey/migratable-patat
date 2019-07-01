---
title: Migrating
author: Daniel J. Harvey
patat:
  wrap: true
  margins:
    left: 10
    right: 10
    top: 10
    bottom: 10
  incrementalLists: true
  images:
    backend: iterm2

---

# Who I am and what I want

# Daniel J Harvey

Proud computer owner

Typer of words

etc

# What I want

Answers to my problems

# What are those problems?

Old data, new data

# The Traditional Monolith

When the backend changed, we also changed the frontend.

If everything type checked, we had a great time.

(Side quest: contract tests)

Everything deploys together, so there is no concept of the past.

# PROBLEM ONE

We event sourced

Our new data changed with us, but our old data stayed behind

How do we still read this old data?

# PROBLEM TWO

We made our APIs public

We made promises

(that we really should do our best to keep)

# We are now tied forever to our past

Oh no.

# Concrete examples

# Our first data type

```haskell
data OldUser
  = OldUser 
    { firstName :: String
    , surname   :: String
    , pet       :: String
    , age       :: Int
    }
    deriving (Generic, JSON.FromJSON, JSON.ToJSON)
```

# Oh no! A change!

## We should we using newtypes

```haskell
newtype Name
  = Name { getName :: Text.Text }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)
```

## And replace our `pet` with a sum type.

```haskell
data OldPet
  = OldDog
  | OldCat
  | NoPet
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)
```

# here is the data type

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Generic, JSON.FromJSON, JSON.ToJSON)
```

# We've now got old API users

# Or indeed old data saved of our own

# What options do we have?

## Keep code that deals with the old data

## ...and new code for the new data?

## Or migrate the data from one to the other?

# What does that look like?

```haskell
migrate :: OldUser -> NewUser
migrate = _
```

- Actually. Let's be realistic about life.

```haskell
migrate :: OldUser -> Maybe NewUser
migrate = _
```

# So, let's decode some JSON data!

We can try and decode the old version

```haskell
parseOldUser :: JSON -> Maybe OldUser
parseOldUser json
  = parseMaybe (parseJSON json)
```

And the new version..

```haskell
parseNewUser :: JSON -> Maybe NewUser
parseNewUser
  = parseMaybe . parseJSON
```

...and then hack together a combined function

```haskell
parseSomeKindOfUser :: JSON -> Maybe NewUser
parseSomeKindOfUser json
  =    parseNewUser json
  <|> (parseOldUser json >>= migrate)
```

# However

- This

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
```

- Can

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
```

- Soon

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
```

- Get

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse f >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
```

- Out

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse f >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse g >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse h >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
```

- Of

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse f >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse g >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse h >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse i >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse j >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
```

- Hand

```haskell
thing = parse a
    <|> parse b >>= migrate
    <|> parse c >>= migrate >>= migrate
    <|> parse d >>= migrate >>= migrate >>= migrate
    <|> parse e >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse f >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse g >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse h >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse i >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse j >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse k >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
    <|> parse l >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate 
    <|> parse m >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate >>= migrate
```


etc etc etc
etc
etc
etc

# So what do we want?

CONCRETE DATA TYPES

- I want to be able to derive fromJSON, toJSON, generic, arbitrary without
  getting deep into type hell

- I don't want caring about my older types to make the code that deals with my
  current data types harder

- I don't want my code to grow in complexity at an exponential rate

# Good, but maybe too clever.

```haskell
data Horse (v :: Nat) where
  HorseV1 :: { name :: String } -> Horse 1
  HorseV2
    :: { firstName :: String
       , lastName  :: String
       }
    -> Horse 2
```

- Nice - but I don't GADTs leaking all over my application, sorry.

- Let's tag our POHR (Plain Old Haskell Records) instead

```
class Versioned (label :: Symbol) (num :: Nat) where
  type num `VersionOf` label :: Type
```

- For instance:

```haskell
instance Versioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser
```

```haskell
instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser
```

# What about our migrations?

We make a typeclass for migrations that uses our `VersionOf` type function

```haskell
class Migratable (label :: Symbol) (num :: Nat) where
  fromPrevious
    :: (num - 1) `VersionOf` label
    -> Maybe (num `VersionOf` label)
```

```haskell
instance Migratable "User" 2 where
  fromPrevious :: OldUser -> Maybe NewUser
  fromPrevious older
    = Just $ NewUser 
              { newFirstName = Name (Text.pack (firstName older))
              , newSurname   = Name (Text.pack (surname older))
              , newPet       = readPet (pet older)
              , newAge       = age older
              }
    where
      readPet s
        | s == "dog" = OldDog
        | s == "cat" = OldCat
        | otherwise  = NoPet
```

# What does all that buy us then?

```haskell
parseSomeKindOfUser :: JSON -> Maybe NewUser
parseSomeKindOfUser = parseJSONVia @"User" @1 @2
```

- What are those @ symbols?

Type applications - this means we are telling the `parseJSONVia` function we
want to use the label `User`, start at version `1`, and go up to version `2`.



# Why not a functional dependency or something?

Ahh yes

But what if we want to have a data type be in more than one set?



```haskell
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog

```

```haskell
data AgelessDog
  = AgelessDog
      { name :: String }

instance Versioned "Dog" 2 where
  type 2 `VersionOf` "Dog" = AgelessDog

```

```haskell
data WithAgeDog
  = WithAgeDog
      { name :: String
      , age  :: Int
      , tail :: Bool
      }

instance Versioned "Dog" 3 where
  type 3 `VersionOf` "Dog" = WithAgeDog
```

This is lossier than it needs to be though - any version `1` piece of data will
convert through version 2 and lose everything on the way.

So let's have two import paths!

```
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog

instance Versioned "AgeDog" 1 where
  type 1 `VersionOf` "AgeDog" = Dog
```

```haskell
data WithAgeDog
  = WithAgeDog
      { name :: String
      , age  :: Int
      , tail :: Bool
      }

instance Versioned "Dog" 3 where
  type 3 `VersionOf` "Dog" = WithAgeDog

instance Versioned "AgeDog" 2 where
  type 2 `VersionOf` "AgeDog" = WithAgeDog
```

Then our parsing function becomes (something like)

```
parseSomeKindOfDog :: JSON -> Maybe WithAgeDog
parseSomeKindOfDog json
  =  parseJSONVia @"AgeDog" @1 @2 json
 <|> parseJSONVia @"Dog" @1 @3 json
```

- We try the lossless path

- Failing that, we try the lossy path to pick up any `AgeLessDog` values.




# Datatype confusion

- Money example

```
data Info
  = Info 
      { amount: Pounds
      }
```  


```haskell
data NewInfo
  = NewInfo
      { amount :: Pennies
      }
```

```json
{ "amount": 100 }
```

- is 100 pennies?

- 100 pounds?

```haskell
describe "Uses Arbitrary to generate said tests" $ do
      it "Checks how many matches we got on our non-matching schema" $ do
        found <- matchAll @1 @4 @"User"
        found `shouldBe` Right [1,2,3,4]

      it "Spots our problematic matching schema" $ do
        found <- matchAll @1 @2 @"Same"
        found `shouldBe` Left [Duplicates 1 [2,1], Duplicates 2 [2,1]]
```


# Questions

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

whyDidntYouUse :: forall a. a -> String
whyDidntYouUse = const "Because I am a terrible programmer"
```

# More of a comment really.

```haskell
{-# LANGUAGE ExplicitForAll #-}

whyDidntYouUse :: forall a. a -> String
whyDidntYouUse = const "Because I am a terrible programmer"
```

