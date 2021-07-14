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
  - repl
  - function
  - data
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

# Key points of Haskell

It's dangerous to go alone! Take these.

- everything is function
  - value is like function without argument, wow
- Haskell is lazy
  - I won't focus on this part in this talk
- Haskellers are lazy
  - we don't like duplicate stuff

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

# Simple generic in functions

```haskell
mapStringToInt :: (String -> Int) -> ([String] -> [Int])
```

how to make it more generic? Simple! Make some `type variable`!

```haskell
-- String becomes 'a'
-- Int becomes 'b'
map :: (a -> b) -> ([a] -> [b])
```

---

# Simple generic in functions

```haskell
map :: (a -> b) -> ([a] -> [b])

length :: String -> Int
(map length) :: [String] -> [Int]

not :: Bool -> Bool
(map not) :: [Bool] -> [Bool]

toString :: Int -> String
(map toString) :: [Int] -> [String]
```

now `map` is generic that you can map **everything** inside list `[]`

---


# Simple generic in functions, examples

```haskell
(map length) :: [String] -> [Int]
(map not) :: [Bool] -> [Bool]
(map toString) :: [Int] -> [String]

(map length) ["a", "aa", "aaa"]
> [1, 2, 3]

(map not) [True, True, False]
> [False, False, True]

(map toString) [9, 10, 11]
> ["9", "10", "11"]

(map length) ((map toString) [9, 10, 11])
> [1, 2, 2]
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

sortBy :: (a -> a -> Ordering) -> ([a] -> [a])

-- please ignore the weird Ord here.
comparing :: Ord a => (b -> a) -> (b -> b -> Ordering)

height :: Student -> Int

(&) :: a -> (a -> b) -> b
```

---

# Solve this now!

```haskell
students & sortBy (comparing height)
```

```haskell
comparing :: Ord a => (b -> a) -> (b -> b -> Ordering)
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

# REPL

Read–eval–print loop

You can see result directly

```shell
>_ ghci
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/kk/.ghci,,

> 1 + 41
42

> fmap show [1..10]
["1","2","3","4","5","6","7","8","9","10"]

> not True
False

> length "apple"
5
```

<!--
雖然是靜態型別，但是還是有有 repl
-->

---

# REPL

Read–eval–print loop

You can check types of expression

```shell
> :type not True
not True :: Bool

> :type fmap show [1..10]
fmap show [1..10] :: [String]

> :type length "apple"
length "apple" :: Int
```

---

# REPL

Read–eval–print loop

You can load any Haskell source code and reload it after editing

```shell
> :load /tmp/Main.hs
[1 of 1] Compiling Main             ( /tmp/Main.hs, interpreted )
Ok, one module loaded.
> :browse Main
# After some edits
> :reload
[1 of 1] Compiling Main             ( /tmp/Main.hs, interpreted )
Ok, one module loaded.
> :browse Main
haha :: String
> haha
"hahaha"
```

You don't need to import certain module and running some functions to *partially* reload your module. ([StackOverflow: How do I unload (reload) a Python module?](https://stackoverflow.com/questions/437589/how-do-i-unload-reload-a-python-module))

---

# Function is useful and everywhere

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

---

# Function is useful and everywhere

Everyone wants infix function, but they pretend they don't
    
```haskell
numberA `compare` numberB
```

```c#
// C#
// if method is so great, why don't they write "123.add(100)?
123 + 100 // 223
123.CompareTo(100) // 1
```

```rust
// Rust
123 + 100 // 223
123_i32.cmp(&100) // Ordering::Greater
```

Now you know why people sometimes are obsessive about "object", because "method" is actually infix function!

---

# Function is useful and everywhere

Don't like it? Then use function to change it!

```haskell
(&) :: a -> (a -> b) -> b
sort :: [Int] -> [Int]

-- missing the object-like method?
sort [4,3,2,1]

-- no thanks!
[4,3,2,1] & sort
```

---

# Function is useful and everywhere

Like the idea of **TRUE-ZERO-PARENTHESE**?

```haskell
gameCollections :: Set Game
metroidDread :: Game
silksong :: Game

insert :: Game -> (Set Game -> Set Game)

-- oh my
insert silksong (insert metroidDread gameCollections)

-- oh, is that...
gameCollections & insert metroidDread & insert silksong

-- it's method chaining! it's fluent interface!
-- NO! It's just function...
gameCollections
  & insert metroidDread
  & insert silksong
```

---

# Currying

```haskell
-- follows the type
addThreeInt       :: Int -> Int -> Int -> Int
addThreeInt 1     :: Int -> Int -> Int
addThreeInt 1 2   :: Int -> Int
addThreeInt 1 2 3 :: Int
```

it means that `addThreeInt 1 2` is valild and returns a **function** with type `Int -> Int`!

you don't need to write code below like many other languages

```haskell
-- even though it's still valid
(\n -> addThreeInt 1 2 n) 3
```

```javascript
// JavaScript, so many parentheses, is that Lisp?
(n => addThreeInt(1, 2, n)) (3) 
```

---

# Currying

In haskell, currying happends to all functions, and that's useful

```haskell
numbers :: [Int]
filter :: (a -> Bool) -> [a] -> [a]

-- too many words
filter (\n -> n < 10) numbers

-- that's good
filter (< 10) numbers
```

it means that `addThreeInt 1 2` returns a **function** with type `Int -> Int`!

---

# Data

how do we define & construct data?

---

# Data - declare them

Like enum

```haskell
-- Define a type named MyBool
data MyBool = MyTrue | MyFalse

-- And you'll get following functions automatically

-- MyTrue  :: MyBool
-- MyFalse :: MyBool
```

It's like `enum` in most language

`MyBool` is either `MyTrue` or `MyFalse`

---

# Data - declare them

Like struct

```haskell
-- Define a type named Person
data Person = Person Int String

-- And you'll get following functions automatically
-- I know it's confusing, but
-- one Person is *function*, the other is *type*

-- Person :: Int -> String -> Person
```

It's like `struct` in most language

`Person` stores two field, `Int` and `String`

---

# Data - declare them

More like struct

```haskell
-- Define a type named Person
data Person = Person
  { age  :: Int
  , name :: String
  }

-- And you'll get following functions automatically

-- Person :: Int -> String -> Person

-- age  :: Person -> Int
-- name :: Person -> String
```

It's like `struct` in most language, with field accessor

---

# Data - mix them

We want both

```haskell
-- Define a type named Shape
data Shape
  = Circle { radius :: Int }
  | Rectangle { width :: Int , height :: Int }

-- And you'll get following functions automatically

-- Circle    :: Int -> Shape
-- Rectangle :: Int -> Int -> Shape

-- radius :: Circle -> Int
-- width  :: Rectangle -> Int
-- height :: Rectangle -> Int
```

---

# Data - generic

We don't have billion-dollar mistake

```haskell
-- Define a type named Maybe
data Maybe a = Nothing | Just a

-- And you'll get following functions automatically

-- Nothing :: Maybe a
-- Just    :: a -> Maybe a

Just "do it" :: Maybe String
```

It's like `null pointer` in most language, but you **CAN'T** dereference it randomly

---

# Pattern matching

now, we need to destruct our data

---

# Pattern matching

The enum type

```haskell
data MyBool = MyTrue | MyFalse

boolToString :: MyBool -> String
boolToString myBool = case myBool of
  MyTrue  -> "my true"
  MyFalse -> "my false"
```

We use `case <expr> of` to *destruct* our data

Then *transform* them into other type of data

---

# Pattern matching

The struct type

```haskell
data Person = Person Int String

personToString :: Person -> String
personToString p = case p of
  Person age name -> name
```
```haskell
data Maybe a = Nothing | Just a

maybeStringToString :: Maybe String -> String
maybeStringToString ms = case ms of
  Just s  -> s
  Nothing -> ""
```

---

# Too boring? Check this out!

---

# Pattern matching

Can you tell which case is missing?

```haskell
someLogic :: (Bool, Bool, Bool) -> String
someLogic bs = case bs of
  (True,    _,     _) -> "a"
  (   _, True,  True) -> "b"
  (   _,    _, False) -> "c"
```

```cpp
string some_logic(bool a, bool b, bool c) {
  if (a) { return "a"; }
  if (b && c) { return "b"; }
  if (!c) { return "c"; }
  // 
}
```

---

# Generic
