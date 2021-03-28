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

# <!-- fit --> The famous map

---

# List

## Type

```haskell
listMap :: (a -> b) -> [a] -> [b]
```

## Examples

```haskell
listMap toUpperString ["apple", "book"] :: [Int]
> ["APPLE", "BOOK"]

listMap length ["apple", "book"] :: [Int]
> [5, 4]
```

---
