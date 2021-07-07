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
map length ["apple", "orange", "banana"]
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

fmap length ["apple", "orange", "banana"]
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

Literals
    
`12 :: Int`, the type of `12` is `Int`

`"Hello" :: String`, the type of `"Hello"` is `String`

`True :: Bool`, the type of `True` is `Bool`

more examples

`("yes", 123) :: (String, Int)`

`[1, 2, 3] :: [Int]`

`[[1, 2, 3], [4, 5, 6]] :: [[Int]]`
 
---

# How we write down type of expressions

Functions

You could treat the last type as return type

- lambda expressions have type

`(\a -> a + 1) :: Int -> Int`, argument is `Int`, return type is `Int`

`(\a b -> a + b) :: Int -> Int -> Int`, arguments are `Int` and `Int`, return type is `Int`

- named functions have type as well

`sum :: [Int] -> Int`

`mapStringToLength :: [String] -> [Int]`

---

# define function

  - define function in code
    - f(x) = x^2

---

# define value

  - define value in code

---

# Simple generic function

  - simple generic function
    - map :: (a -> b) -> [a] -> [b]
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

