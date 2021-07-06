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

my favorite example

```haskell
students & sortBy (comparing height)
```

```haskell
"Hello" <> "World"
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

True

["apple", "orange", "banana"]

(\x -> "yes")
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

-- but the parentheses is "optional"
sum [1, 2, 3]

-- no one like parentheses, right?
1 + (2 + 3)

-- more than one argument
fmap length ["apple", "orange", "banana"]
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

Don't forget the lambda literal
    
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

# How we write down types

  - how we write down types
    - a -> b
    - a -> b -> c

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
