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

# Friendly introduction

- Hello. My name is Daniel J. Harvey.

- I work at a company called Habito

- You are sitting in their offices

- We (they?) are hiring

- (I think?)

# What I am not going to talk about today

* Dependant Types

* Free Monad-based effects

* Data.Dynamic

* HLists

* GADTs

# What I am going to talk about today

- Data

- Time

- How datatypes change over time and what we can do to not lose our minds
  completely coping with this.

# Context

- My background is in making web applications of some sort or another

- Therefore my perspectives are entirely biased to these use-cases

- Forgive me Padre, etc.

# In the beginning...

We had server side applications.

* If the code agrees with the DB schema...

* *Great!*

# Then came Javascript

* Suddenly all our data was spread around the place

* And things didn't necessarily agree with one another 

* There was sometimes *JQuery*.

# The Traditional Backend / Frontend Monolith

However, not everything was terrible, because most applications were
monolithic things.

- When the back end changes...

- Change the front end too.

- *Deploy everything together*

- (did you forget to update the DB schema?)

- *DO THAT QUICKLY*

- Forget about the past

- *YOLO*

# So...

What could possibly go wrong with this?

# PROBLEM ONE

* `Company A` decide that the DB schema changes make changing the application
  less flexible than they like.

* Therefore they choose to use *Event Sourcing*

* As the application changes, the DB schemas keep up

* But we are left with an event table full of various versions of JSON data.

- *What is this data?*

- *Does this un-named company still understand said data*?

# PROBLEM TWO

* `Company B` decide their code is so good that they are going to create a *public API*

* Other companies decide to use this API, and want it not to change

* Suddenly `Company B` have made lots of promises not to change their API

* Even though they bloody love changing their API because they are 10x hackers who
just can't stop delivering business value.

- *How can they make changes to this without breaking everything?*

# PROBLEM THREE

* `Company C` has noticed that after a while *monoliths take ages to deploy*

* Plus it is easy to parallelise work if many teams *work independently of one
  another*

* Suddenly, services that talk to one another aren't guaranteed to have
  versions or interfaces that match

* So a old service can be receiving requests from a very old service that
  hasn't been updated

- *How will this unnamed company cope with communicating with any number of historical deployments?*

# So basically...

* We are now tied forever to our past

* Business requirements, changing over time...

* ...data, of many shapes and sizes...

- ...if only there was a solution..

# Richy boy

![](./hickey.png)

# Whoa

- ...let's also stipulate that we would like said solution to be statically
  typed

# Our first data type

- Here's a data type that we use in our business critical application.

- It is called `OldUser`, which we never really questioned at the time.

```haskell
data OldUser
  = OldUser 
    { firstName :: String
    , surname   :: String
    , pet       :: String
    , age       :: Int
    }
```

- Business is going pretty well.

* I can't imagine my meeting this afternoon will go badly

# Oh no! A change!

![](./angry-boss.png)

# UH OH

A Change In Business Requirements Has Been Spotted

# Pivot immediately

- Apparently we can increase profitability by *30%* by using newtypes properly

```haskell
newtype Name
  = Name { getName :: Text.Text }
```

- And replacing `String` selections of `pet` types with a more restrictive sum
  type.

```haskell
data OldPet
  = OldDog
  | OldCat
  | NoPet
```

- These changes took all night.

- ...but you really pulled through there.

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
```

# Great job.

- Let's celebrate by deriving some typeclasses.

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq)
```

- Codegen

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq, Ord)
```

- Makes

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq, Ord, Show)
```

- Me

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq, Ord, Show, Generic)
```

- Feel

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
```

- Good

```haskell
data NewUser
  = NewUser
    { newFirstName :: Name
    , newSurname   :: Name
    , newPet       :: OldPet
    , newAge       :: Int
    }
    deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
```

# HOLD ON THOUGH

Business is obviously booming now, but what are we going to do about:

- Third parties that will insist on using `OldUser` in their API calls for the
  next 18 months

- Stored JSON data with the old data shape

# What options do we have?

- Keep *old code* for dealing with the *old data*

  * this seems fine
  * till you're fixing bugs in the old system too
  * shit. more fixes!
  * now you're got an new-old system as well as the new-new one
  * then a few more rounds give you a new-old-new-old system to maintain as well as your main one, which is a new-new-new system by now? It may have been superceded too. Oh dear.

-  Or migrate the *old datatypes* to the *new datatypes*

  * Logic stays in new code
  * Bug fixes in business logic happen once

# Disclaimer

- For the rest of this talk, assume I have chosen option `2`. 

# How would we attack this problem?

- We're going to need a few tools.

- Firstly, a function to convert our *old terrible datatype* into our *new
  incredible exciting datatype*

- We're going to need to convert `OldUser` into `NewUser`
```haskell
migrate :: OldUser -> NewUser
```

- Actually. Let's be realistic about this as life is a bin.
```haskell
migrate :: OldUser -> Maybe NewUser
```

# Decoding JSON data

We'll also need some functions for decoding JSON.

- We'll use these functions from `Aeson`

- This will try and convert some `JSON` into an `OldUser`
```haskell
parseOldUser :: JSON -> Maybe OldUser
parseOldUser json
  = parseMaybe (parseJSON json)
```

- And this very similar function will try and convert from `JSON` into a
  `NewUser`.
```haskell
parseNewUser :: JSON -> Maybe NewUser
parseNewUser
  = parseMaybe . parseJSON
```

- We can then make a function that takes some `JSON`, and then tries to decode
  it into a `NewUser`

- If it can't, it tries to parse it into an `OldUser`...

- ...and if that succeeds, it uses some sort of `migrate` function to turn
  `OldUser` into a `NewUser`

- It looks something like this.
```haskell
parseSomeKindOfUser :: JSON -> Maybe NewUser
parseSomeKindOfUser json
   =   parseNewUser json
  <|> (parseOldUser json >>= migrate)
```

# So

It seems to do the job.

- However

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

# Is this what we want?

* I mean, no

* But sort of, yes.

* Why can't I get the compiler to do this for me?

# Let's give it a smash

OK!

- Let's make a typeclass and use it to version tag our POHR (*Plain Old Haskell Records*)
```haskell
class Versioned (label :: Symbol) (num :: Nat) where
  type num `VersionOf` label :: Type
```

- This `VersionOf` type here is an `associated type family` - a type level
  function that is scoped to only work inside the typeclass it is defined in.

- It defines a function we can use to find a datatype from the `label` and the
  `num`.

- For example, we can make a label called `"User"`, and make `OldUser` version
  `1` of it.
```haskell
instance Versioned "User" 1 where
  type 1 `VersionOf` "User" = OldUser
```

- We'd then make `NewUser` version `2` of it.
```haskell
instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = NewUser
```

# Linking them together

- Let's make a typeclass for migrations that uses our `VersionOf` type function
```haskell
class Migratable (label :: Symbol) (num :: Nat) where
  fromPrevious
    :: (num - 1) `VersionOf` label
    -> Maybe (num `VersionOf` label)
```

- It lets us define a function from the previous version of a datatype to
  the current one.

- We can use it to migrate `OldUser` to `NewUser`.
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

- Firstly, this more exciting version of our `migrate` function
```haskell
migrate 
  :: earliest `VersionOf` label 
  -> Maybe (target `VersionOf` label)
```

- Which we can pass `versions` and a `label` to convert and old datatype to a
  new one.
```haskell
oldToNew :: OldUser -> Maybe NewUser
oldToNew = migrate @1 @2 @"User"
```  

- This one only does one conversion - but we can use the same code to do as
  many versions as we like.
```haskell
veryOldToVeryNew :: OldUser -> Maybe VeryNewUser
veryOldToVeryNew = migrate @1 @100 @"User"
```  

# What about that JSON we talked about earlier?

- We also get this function:
```haskell
parseJSONVia
  :: JSON 
  -> Maybe (target `VersionOf` label)
```

- Which works like this:
```haskell
parseSomeKindOfUser 
  :: JSON 
  -> Maybe NewUser
parseSomeKindOfUser 
  = parseJSONVia @"User" @1 @2
```

- Which, to clarify, is just automatically doing this:
```haskell
thing = parse a
    <|> parse b >>= migrate
```

# What are those @ symbols?

- Type applications!

- This is the `Schema` typeclass that provides this functionality:
```haskell
class Schema (label :: Symbol) (earliest :: Nat) (target :: Nat) where
  parseJSONVia :: JSON.Value -> JSON.Parser (target `VersionOf` label)
```

- So when we use it like such...
```haskell
parseJSONVia @"User" @1 @2
```

- We are passing the type-level symbol `"User"` as the first argument `label`.

- Then a type-level natural number `1` as the starting version `earliest`.

- And another type-level natural `2` as the target version `target`.

# BUT HOLD ON

That's a fair chunk of complexity you've just introduced there

* Type-level symbols

* Type-level natural numbers

* A shameful number of language extensions
```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
```

* ...what do I get for all this?

# What else do I get?

* Automatic uniqueness checking for free! 

- Let's say that we have this data type `Info`.
```haskell
data Info
  = Info 
      { amount: Pounds }
```  

- Then, after another hard pivot, we change the units.
```haskell
data NewInfo
  = NewInfo
      { amount :: Pennies }
```

- In our static typed ivory tower, we are fine, but our clients keep sending us
  this:
```json
{ "amount": 100 }
```

- What is it?

- 100 pennies?

- 100 pounds?

- How can we stop this confusion?

# MatchAll

* We get this typeclass defined
```haskell
class MatchAll (earliest :: Nat) (latest :: Nat) (label :: Symbol) where
  matchAll :: IO (Either [MatchError] [Integer])
```

* It uses `QuickCheck` and it's `Arbitrary` instances to generate random `JSON`
  values for each datatype

* Then tries to load each generated value as each version of the datatype

* And tells us how many version of a datatype each generated instance is able
  to decode

* If it's one each - we're going to have a good time.
```haskell
describe "Uses Arbitrary to generate said tests" $ do
  it "Checks if our datatypes will get confused" $ do
    found <- matchAll @1 @4 @"User"
    found `shouldBe` Right [1,2,3,4]
```

* But if our `JSON` representations are non-unique, we'll know. 
```haskell
describe "Our Pennies and Pounds schema" $ do
  it "Spots our problematic matching schema" $ do
    found <- matchAll @1 @2 @"Same"
    found `shouldBe` Left [Duplicates 1 [2,1], Duplicates 2 [2,1]]
```

* And we can fix our data types to ensure uniqueness.

```haskell
data Info
  = Info 
      { amountPounds :: Pounds }

data NewInfo
  = NewInfo
      { amountPennies :: Pennies }
```

- Good job.

# A big clever FromJSON instance

- The `Aeson` library works by making datatypes define instances of the
  `FromJSON` typeclass.

- Packages like `Servant` allow us to automagically create web servers that use
  these types.

- Therefore, we can create a `newtype` that wraps all our API versions...
```haskell
newtype APIUser
  = APIUser { getAPIUser :: WhateverTheNewestUserTypeIsTheseDays }
```

- ...and use `parseJSONVia` to create a `FromJSON` instance for that
  datatype...
```haskell
instance JSON.FromJSON APIUser where
  parseJSON a
    = APIUser <$> parseJSONVia @"User" @1 @4 a
```

- And make a `Servant` server that can read any of our historical datatypes.
```haskell
type ExcellentApi =
  "user" :> Get '[JSON] [APIUser] 
```

- Great job!

# So, to sum up

- I want to define my migrations outside my main code, throw those into a file
  and forget about them forever until my next migration.

- I want to be able to use simple ADTs for my types if I feel like it.

- No, *seriously*. I want to be able to derive `FromJSON`, `ToJSON`, `Generic`, `Arbitrary` etc and use most of the standard library without
  getting deep into type hell just because my library was too clever and wanted
to put GADTs everywhere.

- I don't want historical code to make my new code more complicated

- (As much as possible)

- I just want to survive the next goddamn pivot without losing my mind.

# Have we achieved this?

Who knows?

- See the code at
[https://github.com/danieljharvey/migratable](https://github.com/danieljharvey/migratable)

- Shout at me at `@yevrahjleinad` on twitter.

# Questions

- Any questions?

# Disclaimer

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

whyDidntYouUse :: forall a. a -> String
whyDidntYouUse = const "Because I am a terrible programmer"
```



# BONUS CONTENT

- These will get big

- But our datatypes can be in more than one set.
```haskell
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog
```

- Notice we remove `age` in this one.
```haskell
data AgelessDog
  = AgelessDog
      { name :: String }

instance Versioned "Dog" 2 where
  type 2 `VersionOf` "Dog" = AgelessDog
```

- Oops. We needed that.
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

- Any version `1` piece of data will
convert through version 2 and lose everything on the way.

- So let's have two import paths!
```haskell
data Dog
  = Dog { name :: String
        , age  :: Int
        }

instance Versioned "Dog" 1 where
  type 1 `VersionOf` "Dog" = Dog

instance Versioned "AgeDog" 1 where
  type 1 `VersionOf` "AgeDog" = Dog
```

- (We've ignored the middle one for now - it is the same)
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

- Then our parsing function becomes (something like)
```haskell
parseSomeKindOfDog :: JSON -> Maybe WithAgeDog
parseSomeKindOfDog json
  =  parseJSONVia @"AgeDog" @1 @2 json
 <|> parseJSONVia @"Dog" @1 @3 json
```

- We try the lossless path

- Failing that, we try the lossy path to pick up any `AgeLessDog` values.

