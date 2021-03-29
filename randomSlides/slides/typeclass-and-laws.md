---
paginate: true
---

# <!-- fit --> Generics in Haskell

---

# Simple Haskell syntax

## Type annotation

```haskell
expr :: type
```

## example

```haskell
"hello" :: String

42 :: Int

-- `a` is called `type variable`
length :: [a] -> Int
```

---

# Simple Haskell syntax

## Type variables

```haskell
[String]

HashSet Int

HashMap String Int
```

Is like...

```cpp
list<std::string>

set<int>

map<std::string, int>
```

---

# Simple Haskell syntax

## Function type

```haskell
f :: a -> b -> c
```

Is like this in C

```c++
c f(a, b)
```

## example

```haskell
preppend :: a -> [a] -> [a]

filter :: (a -> Bool) -> [a] -> [a]
```

---

# Simple Haskell syntax

## Function application

```haskell
f a b
f (a) (b) -- () is optional
```

Is like this in C

```c++
f(a, b)
```
---

# <!-- fit --> The famous **map**

---

# List

## Type

```haskell
listMap :: (a -> b) -> [a] -> [b]
```

## Examples

```haskell
listMap toUpperString ["apple", "book"] :: [String]
> ["APPLE", "BOOK"]

listMap length ["apple", "book"] :: [Int]
> [5, 4]
```

---

# Maybe

## Definition

```haskell
data Bool = True | False -- As reference

data Maybe a = Just a | Nothing
```
## Examples

```haskell
Nothing :: Maybe Int
> Nothing

Just "hello world" :: Maybe String
> Just "hello world"
```

---

# Maybe

## Type

```haskell
maybeMap :: (a -> b) -> Maybe a -> Maybe b
```

## Examples

```haskell
maybeMap toUpperString (Just "apple") :: Maybe String
> Just "apple"

maybeMap length (Just "apple") :: Maybe Int
> Just 5

maybeMap length Nothing :: Maybe Int
> Nothing
```

---

# <!-- fit --> Too many **maps**


---

# Too many maps

## Our maps in *same* shape

```haskell
listMap :: (a -> b) -> [a] -> [b]
maybeMap :: (a -> b) -> Maybe a -> Maybe b
```

## The shape

```haskell
someMap :: (a -> b) -> f a -> f b
```

---

# <!-- fit --> The not so famous **Functor**

This is different from the C++ functor, sorry

---

# Functor

## Definition

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

---

# Functor

## Are these **semantic** ?

```haskell
fmap length ["apple", "book"]
> []

fmap toUpperString ["apple", "book"]
> []

fmap length (Just "apple")
> Nothing

fmap toUpperString (Just "apple")
> Nothing
```

---

# Functor

## Laws

```haskell
fmap id == id
fmap (f . g)  ==  fmap f . fmap g
```

## Example

```haskell
fmap length ["apple", "book"]
> []

-- This breaks the law, we want ["apple", "book"] back
fmap id ["apple", "book"]
> []
```
---

# Functor

## Implement Functor for [] and Maybe

```haskell
instance Functor [] where
  fmap = listMap -- Reuse the function!

instance Functor Maybe where
  fmap = maybeMap -- Reuse the function!
```

## Example

```haskell
fmap length ["apple", "book"]
> [5, 4]

fmap length (Just "apple")
> Just 5
```

---

# Functor

From wikipedia: https://en.wikipedia.org/wiki/Functor

> In mathematics, specifically **category theory**, a functor is a mapping between categories.


We don't need to know all the details to use Functor.

---

# <!-- fit --> Other examples

---

# Semigroup

## Definition

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

## Laws

```haskell
x <> (y <> z) = (x <> y) <> z
```

---

# Semigroup

## Example

```haskell
"hello" <> "world" <> "x":: String
> "helloworldx"

(Set.fromList [1]) <> (Set.fromList [2, 5]) :: Set Int
> { 1, 2, 5 }
```

---

# Semigroup

> In mathematics, a semigroup is an algebraic structure consisting of a set together with an associative binary operation. 

We don't need to worry that too much

---

# Build functions => find pattern => find laws => create generic interface

