---
theme: default
highlighter: shiki
canvasWidth: 800
info: |
  ## Slidev Starter Template
  Presentation slides for developers.

  Learn more at [Sli.dev](https://sli.dev)
title: Welcome to Slidev
layout: cover
defaults:
  layout: 'center'
---

# Haskell，不年輕卻前衛的優雅程式語言

KK - COSCUP 2021

<a href="https://github.com/slidevjs/slidev" target="_blank" alt="GitHub"
  class="abs-br m-6 text-xl icon-btn opacity-50 !border-none !hover:text-white">
  <carbon-logo-github />
</a>

---

# Content

- stories and history
- basic grammar
- unique features
  - IO
  - concurrency
  - parser
  - etc

---
layout: cover
---

# Stories and history

chapter I

---

# Hello, Haskell ! 

This is the icon of haskell

<logos-haskell-icon class="text-9xl" />

---

# What's inside Haskell?

- statically typed
- purely functional
  - no side effect !?
- type inference
- lazy evaluation

---

# Brief history of Haskell

some important events

- 1990 - Haskell 1.0
  - almost 31 years old !
- 1996 - Haskell 1.3
  - do notation (we'll talk about it later)
- 1999 - Haskell 98
  - Concurrent Haskell
- 2010 - Haskell 2010
  - FFI support

---
layout: image-right
image: /HaskellBCurry.jpg
---

# Why it's called "Haskell"?

> Haskell Brooks Curry (/ˈhæskəl/; September 12, 1900 – September 1, 1982) was an American mathematician and logician.

---
layout: cover
---

# Basic grammar

chapter II

---

# What does Haskell look like?

Here's my favorite snippet

```haskell
students & sortBy (comparing height)
```

you'll be about to read these after this chapter

<!--
提醒，Haskell 是靜態型別的語言，所以這邊的程式碼都是有固定型別的
-->

---

# Literals

```haskell
42

"hello world"

("egg", 5)

True

["apple", "orange", "banana"]

(\x -> "yes")
```

---

# Function application

```haskell
-- "+" is a function
-- When the function name is "symbol", it's infix function
1 + 2

-- "sum" is a function
-- When the function name is "alphabet", it's prefix function
sum([1, 2, 3])
```

---

# Function application
    
```haskell
-- infix function application
1 + 2

-- chained infix functions application
1 + 2 + 3

-- prefix function application
sum([1, 2, 3])

-- but the parentheses is "optional" in most case
sum [1, 2, 3]

-- no one like parentheses, right?
1 + (2 + 3)

-- more than one argument
mapList (\s -> length s) ["apple", "orange", "banana"]
```

---

# Function application

We'll use this format to display result of expression in the following slides
    
```haskell
1 + 2
> 3

1 + 2 + 3
> 6

sum([1, 2, 3])
> 6

sum [1, 2, 3]
> 6

1 + (2 + 3)
> 6

mapList (\s -> length s) ["apple", "orange", "banana"]
> [5, 6, 6]
```

---

# Function application

Don't forget the lambda expression
    
```haskell
(\x -> x + 1) 41
> 42

(\x y -> x + y) 3 3
> 6

(\x -> "yes") "no"
> "yes"

(\x -> "yes") "I say no!!!"
> "yes"

(\x -> x) "ok"
> "ok"

(\x -> x) [True, True, False]
> [True, True, False]
```

---

# How we write down type of expressions

Type of literals
    
`12 :: Int`, the type of `12` is `Int`

`"Hello" :: String`, the type of `"Hello"` is `String`

`True :: Bool`, the type of `True` is `Bool`

more examples

`("yes", 123) :: (String, Int)`

`[1, 2, 3] :: [Int]`

`[[1, 2, 3], [4, 5, 6]] :: [[Int]]`
 
---

# How we write down type of expressions

Type of functions

You could treat the last type as return type

- lambda expressions have type

`(\a -> a + 1) :: Int -> Int`, argument is `Int`, return type is `Int`

`(\a b -> a + b) :: Int -> Int -> Int`, arguments are `Int` and `Int`, return type is `Int`

- named functions have type as well

`sum :: [Int] -> Int`

`mapStringToLength :: [String] -> [Int]`

---

# Define value in code

```haskell
theAnswer :: Int
theAnswer = 42
```

So basically you tell compiler (or your co-worker):

- what's the type of `theAnswer`
- what's the implementation of `theAnswer`

---

# Define function in code

```haskell
addThree :: Int -> Int
addThree = (\n -> n + 3)
```

So basically you tell compiler (or your co-worker):

- what's the type of `addThree`
- what's the implementation of `addThree`

---

# Define value in code

Looks familiar

```haskell
theAnswer :: Int
theAnswer = 42
```

```haskell
addThree :: Int -> Int
addThree = (\n -> n + 3)
```

For sure, **function** is one kind of **value**

---

# Define function in code

So many possibilities

```haskell
addThree :: Int -> Int
addThree = (\n -> n + 3)
```

To reduce the number of parentheses and arrows, you could define **functoin** in the following form

```haskell
addThree :: Int -> Int -- type is the same
addThree n = n + 3     -- move argument the the left
```

or in elementary school style, if you prefer (yes it's valid syntax)

```haskell
f :: Int -> Int
f(x) = x + 3
```

---

# Go back to this

We've learned enough to read this

```haskell
students & sortBy (comparing height)
```

types of these

```haskell
students :: [Student]

sortBy :: (a -> a -> Ordering) -> [a] -> [a]

-- please ignore the weird Ord here.
comparing :: Ord a => (b -> a) -> b -> b -> Ordering

height :: Student -> Int

(&) :: a -> (a -> b) -> b
```

---

# Solving this

```haskell
students & sortBy (comparing height)
```

```haskell
comparing :: Ord a => (b -> a) -> b -> b -> Ordering
height ::        Student -> Int

-- Step 1: "b" is Student, "a" in Int
(comparing height) :: Student -> Student -> Ordering

sortBy ::             (a      -> a       -> Ordering) -> [a] -> [a]

-- Step 2: "a" is Student
sortBy (comparing height) :: [Student] -> [Student]

students :: [Student]
(&) :: a -> (a -> b) -> b

-- Step 3: "a" is [Student], "b" is [Student] as well
students & sortBy (comparing height)
```


<!--
慢慢看沒有問題的
-->

---
layout: cover
---

# Unique features

chapter III

---

# Function application

Prefix, infix, your choice
    
```haskell
-- "non-symbol" functions are prefix by default
mod 10 3
> 1

-- calling them with `` make them infix
10 `mod` 3
> 1

-- "symbol" functions are infix by default
[1, 2] ++ [3, 4]
> [1, 2, 3, 4]

-- calling them with () make them prefix
(++) [1, 2] [3, 4]
> [1, 2, 3, 4]
```

<!--
maybe move the to feature section?
-->

---

# Generic


