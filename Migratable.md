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

# Disclaimer

- My background is in making web applications of some sort or another

- Therefore my perspective is completely based on this

- Forgive me Padre, etc.

# In the beginning...

We had server side applications.

* If the code agrees with the DB schema...

* Great!

* (if we have types too, then just wow really) 

# Then came Javascript

* Suddenly all our data was spread around the place

* And things didn't necessarily agree with one another 

* There was sometimes JQuery.

* At some point, our applications had a Front End. 

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

- etc

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

# We are now tied forever to our past

* Business requirements, changing over time...

* ...data, of many shapes and sizes...

- ...if only there was a solution..

# Richy boy

![](./hickey.png)

# Whoa

- ...and that solution also had types

# Concrete examples

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
    deriving stock (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)
```

- Good job.

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

- This `VersionOf` type here is an `associated type synonym` that works a bit
  like a functional dependency

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

- In short, this function:
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

- What are those @ symbols?

Type applications - this means we are telling the `parseJSONVia` function we
want to use the label `User`, start at version `1`, and go up to version `2`.

# Why else do I get for free from this?

- Avoiding datatype confusion

- Back to business. We have this data type `Info`.

```haskell
data Info
  = Info 
      { amount: Pounds
      }
```  

- Then, after another hard pivot, we change the units.
```haskell
data NewInfo
  = NewInfo
      { amount :: Pennies
      }
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

```haskell
describe "Uses Arbitrary to generate said tests" $ do
      it "Checks how many matches we got on our non-matching schema" $ do
        found <- matchAll @1 @4 @"User"
        found `shouldBe` Right [1,2,3,4]

      it "Spots our problematic matching schema" $ do
        found <- matchAll @1 @2 @"Same"
        found `shouldBe` Left [Duplicates 1 [2,1], Duplicates 2 [2,1]]
```

# Multiple data sets

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








# Here is my solution

It uses.

- Stand back

# Whoa

- Plain

- Old

- Haskell

- Records

# POHR

* We can pronounce these *poor* if you like.

* Not really sure why.


# A BRIEF INTERLUDE IN DEFENSE OF CONCRETE DATA TYPES

- I want to be able to derive fromJSON, toJSON, generic, arbitrary without
  getting deep into type hell

- I don't want caring about my older types to make the code that deals with my
  current data types harder

- I don't want my code to grow in complexity at an exponential rate

- I want as much of the standard library available to me as possible because I
  am lazy as hell  

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


