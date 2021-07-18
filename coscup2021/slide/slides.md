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
  - monadic I/O
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
- Haskell is lazy (evaluation) by default
  - I won't focus on this part in this talk
- Haskellers are lazy
  - they don't like repetition
- focus on input, output and types
  - try not to think about variable mutation here

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

how to make it more generic? Simple! Make some **type variables**!

```haskell
-- String becomes 'a'
-- Int becomes 'b'
map :: (a -> b) -> ([a] -> [b])
```

Now `a` and `b` could be **any** types. Of course `a` could be the same type as `b`.

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

<!--
這裏看看型別
-->

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

<!--
這裏看看數值
-->

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

---

# REPL

You can see result directly

```shell
> ghci
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

# Functions

Haskell loves functions, uses lots of function, and uses them very well

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

Not Japaness style, not indian style, Haskell style!

---

# Currying

```haskell
-- follows the type, they're all valid expressions
addThreeInt       :: Int -> Int -> Int -> Int
addThreeInt 1     :: Int -> Int -> Int
addThreeInt 1 2   :: Int -> Int
addThreeInt 1 2 3 :: Int
```

it means that `addThreeInt 1 2` is valild and returns a **function** with type `Int -> Int`!

you don't need to write code below like many other languages

```haskell
-- even though it's still valid in Haskell
(\n -> addThreeInt 1 2 n) 3
```

```javascript
// JavaScript, so many parentheses, is that LispScript?
(n => addThreeInt(1, 2, n)) (3) 
```

---

# Currying

Try to change your point of view, so magicall

```haskell
addThreeStuff :: Int -> Bool -> String -> Int
```

is actually (`->` is right associative)

```haskell
addThreeStuff :: Int -> (Bool -> (String -> Int))
```

so

```haskell
addThreeStuff :: Int -> (Bool -> (String -> Int))
addThreeStuff 1       :: Bool -> (String -> Int)
addThreeStuff 1 False          :: String -> Int
addThreeStuff 1 False "bye"              :: Int
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

It's not that useful at a glance, but it'll be annoying quickly when you have to write down function argument every time.

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

# Type inference

Statically typed does not need to be verbose

---

# Type inference

Start from Map

```haskell
data Map k a

empty  :: Map k a 
insert :: Ord k => k -> a -> Map k a -> Map k a
(&) :: a -> (a -> b) -> b
```

```haskell
-- The type of expression below is Map Bool String
empty
  & insert True "yes"
  & insert False "no"
```

Why there's no `Bool`, `Map` or `String` in our code and it still compiled !

<!--
靜態型別的語言不是應該型別寫地到處都是嗎
-->

---

# Type inference

Static enough

```haskell
data Map k a

empty  :: Map k a 
insert :: Ord k => k -> a -> Map k a -> Map k a
```

```haskell
empty
  & insert True "yes"
  & insert False "no"
```

- `insert` in our code takes `Bool` and `String` as arguments
- `insert :: Bool -> String -> Map Bool String -> Map Bool String`
- so the compiler knows `empty` should return `Map Bool String`
- and the compiler knows everything it needs

---

# Type inference

Lessons

- if a programing language is well designed, it could be robust yet clean
- statically typed doesn't means verbose and ceremony code
  - that's caused by your bad design, not by static type
- the most popluar solution is not always the optimal one
  - Bandwagon effect

<!--
從眾效應
-->

---

# Pattern matching

now we have data, time to destruct them

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

```haskell
someLogic :: (Bool, Bool, Bool) -> String
someLogic bs = case bs of
  (True,    _,     _) -> "a"
  (   _, True,  True) -> "b"
  (   _,    _, False) -> "c"
```

`_` means the hold could be either `True` or `False`

Can you tell which case is missing?

---

# Pattern matching

```haskell
someLogic :: (Bool, Bool, Bool) -> String
someLogic bs = case bs of
  (True,    _,     _) -> "a"
  (   _, True,  True) -> "b"
  (   _,    _, False) -> "c"
```

anyhow, the Haskell compiler could find it out instantly

```
Main.hs:2:16: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: (False, False, True)
  |
2 | someLogic bs = case bs of
  |                ^^^^^^^^^^...
```

---

# Pattern matching

It's a blessing to be able to write program like table and let compiler has your back

```haskell
someLogic :: (Bool, Bool) -> String
someLogic bs = case bs of
  (True, True)   -> "a"
  (True, False)  -> "b"
  (False, False) -> "c"
  (False, True)  -> "d"
```

```cpp
string some_logic(bool a, bool b) {
  if (a) {
    if (b) { return "a"; }
    else { return "b"; }
  } else {
    if (b) { return "c"; }
    else { return "d"; }
  }
}
```

---

# IO, side effects

Wake up, we have a **RealWorld** to burn

---

# IO

Hello World

```haskell
putStrLn :: String -> IO ()

-- Every Haskell program start from here
main :: IO ()
main = putStrLn "Hello World"

-- Running this program and you'll get
-- "Hello World" on your console
```

pretty short, huh

---

# IO

Hello World, explanation

```haskell
main :: IO ()
main = putStrLn "Hello World"
```

- `()` is empty tuple, it's like `void` in some language
  - you could call it **unit** in Haskell
- `IO a` is a generic data type
  - `a` could be anything
- `IO ()` means: do some side effects and produce a unit
- writing a Haskell program is like: chain lots of `IO` together to produce the final `IO ()` and run it

---

# IO

How to connect them?

```haskell
putStrLn :: String -> IO ()
getLine :: IO String
(<>) :: String -> String -> String

main :: IO ()
main = ???
```

- `IO String` means: do some side effects and produce a String

But how could us access the `String` from `getLine`, preppend it with "Hello, " then print it on screen?

---

# IO

Time for ">>="

```haskell
putStrLn :: String -> IO ()
getLine :: IO String
(<>) :: String -> String -> String

-- > > =
(>>=) :: IO a -> (a -> IO b) -> IO b

-- if we let "a" as String, "b" as ()...
(>>=) :: IO String -> (String -> IO ()) -> IO ()
```

we could now connect them together

```haskell
getLine >>= \name -> putStrLn ("Hello, " <> name) :: IO ()
```

<!--
讀你的名字，跟你說你好
-->

---

# >>=, now you know the reason

<logos-haskell-icon class="text-6xl" />

<!--
like H, like lambda, like >>=
-->

---

# IO

Greet to many of my friends (no joke, it's valid code)

```haskell
putStrLn :: String -> IO ()
getLine :: IO String
(<>) :: String -> String -> String
(>>=) :: IO a -> (a -> IO b) -> IO b

main :: IO ()
main =
  getLine >>= \firstFriend ->
    getLine >>= \secondFriend ->
      getLine >>= \thirdFriend ->
        putStrLn ("Hello 1, " <> firstFriend) >>= \() ->
          putStrLn ("Hello 2, " <> secondFriend) >>= \() ->
            putStrLn ("Hello 3, " <> thirdFriend)
```

who on earth would write code like this!!!

---

# LispScript - Promise

Not lisp, but like lisp

```javascript
var getUUID = () =>
  fetch('https://httpbin.org/uuid')
    .then(r => r.json()).then(j => j.uuid)
    
var showOnConsole = str => new Promise(resolve => {
  console.log(str); resolve();
})

var main = () =>
  getUUID().then(uuidA =>
    getUUID().then(uuidB =>
      getUUID().then(uuidC =>
        showOnConsole("Hello 1, " + uuidA).then(() =>
          showOnConsole("Hello 2, " + uuidB).then(() =>
            showOnConsole("Hello 3, " + uuidC)
  ))))) // << See, it is Lisp
```


<!--
恭喜！如果你會寫 LispScript，拿你就會寫 Haskell 了
-->

---

# IO - do notation

The very first "syntax sugar"

```haskell
putStrLn :: String -> IO ()
getLine :: IO String
(<>) :: String -> String -> String
(>>=) :: IO a -> (a -> IO b) -> IO b

main :: IO ()
main = do
  firstFriend <- getLine
  secondFriend <- getLine
  thirdFriend <- getLine
  putStrLn ("Hello 1, " <> firstFriend)
  putStrLn ("Hello 2, " <> secondFriend)
  putStrLn ("Hello 3, " <> thirdFriend)
```

who on earth would transform code into this!!!

<!--
有種感覺，你只能在 IO 裏面解開 IO
並且製造一行一行執行的幻覺
-->

---

# JavaScript - Async/Await

```javascript
var getUUID = () =>
  fetch('https://httpbin.org/uuid')
    .then(r => r.json()).then(j => j.uuid)

var showOnConsole = str => new Promise(resolve => {
  console.log(str); resolve();
})

var main = async () => {
  let uuidA = await getUUID()
  let uuidB = await getUUID()
  let uuidC = await getUUID()
  await showOnConsole("Hello 1, " + uuidA)
  await showOnConsole("Hello 2, " + uuidB)
  await showOnConsole("Hello 3, " + uuidC)
}
```

---

# History is fun

|                               | Haskell           | JavaScript | diff     |
| ------------- |:-------------:| -----:|---:|
| 1.0                           | Haskell 1.0, 1990 | ES1, 1997  | 7  years |
| >>=, Promise                  | Haskell 1.3, 1996 | ES6, 2015  | 19 years |
| do notation, async/await      | Haskell 1.3, 1996 | ES8, 2017  | 21 years |

---

# IO, error handling

Something went wrong and you have to do something

---

# IO - error handling

Exception

In Haskell, every `IO a` might throw exception during execution.

The exception mechanism here is no different from other popular programing language

- you can throw desired excpetion when needed
- exception would propagate automatically
- you can catch specific exception and try to recover it, stop the propagation

---

# Breaking news

Here comes a new data type!

```haskell
data Either a b = Left a | Right b
```

Examples: 

- `Either a b` is either `a` or `b`
- `Either Apple Orange` is either `Apple` or `Orange`
- `Either IOError FileHandle` is either `IOError` or `FileHandle`

<!--
介紹這個，等等會用到

看似很簡單對吧，但是多數的流行語言卻沒有辦法好好表達這種 AB 擇一的資料結構
-->

---

# IO - error handling

try, catch, finally

we need only functions, no extra keywords required

```haskell
try :: Exception e => IO a -> IO (Either e a) 
catch :: Exception e => IO a -> (e -> IO a) -> IO a
finally :: IO a -> IO b -> IO a
```

more useful **functions**

```haskell
handle :: Exception e => (e -> IO a) -> IO a -> IO a 
onException :: IO a -> IO b -> IO a 
```

<!--
讓我們來一一看看，可以怎麽使用他們
-->

---

# IO - error handling

See try in action

```haskell
try :: Exception e => IO a -> IO (Either e a) 

readFile :: FilePath -> IO String

usingTry = do
  result <- try (readFile "/tmp/secret") :: IO (Either IOError String)
  case result of
    Left err -> putStrLn "oh no IO error"
    Right content -> putStrLn content
```

`try` let you use `case ... of` to handle error and correct error repectly

---

# IO - error handling

See catch in action

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a

readFile :: FilePath -> IO String
isDoesNotExistError :: IOError -> Bool
pure :: a -> IO a

usingCatch =
  readFile "/tmp/secret"
  `catch` \err ->
    if isDoesNotExistError err
      then pure "file does not exist"
      else pure "random IO error"
```

`catch` let you specify how to recover from certain type of error

---

# IO - error handling

See handle in action

```haskell
catch :: Exception e => IO a -> (e -> IO a) -> IO a
handle :: Exception e => (e -> IO a) -> IO a -> IO a 

usingHandle =
  handle (\err ->
  if isDoesNotExistError err
    then pure "file does not exist"
    else pure "random IO error")
  (readFile "/tmp/secret")
```

`handle` is the "argument-reversed" version of `catch`, they share same behaviour

---

# IO - error handling

See finally in action

```haskell
finally :: IO a -> IO b -> IO a

readFile :: FilePath -> IO String
putStrLn :: String -> IO ()

usingFinally =
  readFile "/tmp/secret" -- IO a
  `finally`
  putStrLn "bye"         -- IO b
```

`finally` runs the `IO b` after `IO a` even if an exception was raised from `IO a`

---

# IO - error handling

See onException in action

```haskell
onException :: IO a -> IO b -> IO a

readFile :: FilePath -> IO String
putStrLn :: String -> IO ()

usingOnException =
  readFile "/tmp/secret" -- IO a
  `onException`
  putStrLn "bye"         -- IO b
```

like `finally`, but only runs `IO b` when an exception was raised from `IO a`

---

# IO - error handling

Lessons

- it's possible to use only function to declare semantics of program
- have only the ability to `catch exception` is far away from **usable**
  - Java has `finally`, but C++ doesn't
- has the ability to construct new semantics is important
  - `finally` is made with `onException`, `onException` is made with `catch`, `catch` is the primitive operation
- statement/keyword is less flexible than function
  - C++ can not create `finally` statement, since it's **statement**
  - Java can not create `onException` statement, since it's **statement**
  
---

# Concurrency

Let two tasks/threads run at the same time **conceptually**

---

# Concurrency

Two

Run two `IO` at the same time, conceptually

```haskell
concurrently :: IO a -> IO b -> IO (a, b) 
```

In most case, you don't need to know the implementation details. That's what we call **abstraction**, that's why functions are useful.

---

# Concurrency

If we can run two, we can run more tnat two

Run multiple `IO` at the same time conceptually, and amazingly simple

```haskell
mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
forConcurrently :: [a] -> (a -> IO b) -> IO [b]
```

```haskell
queryUrl:: Url -> IO HTML

mapConcurrently queryUrl ["pageA", "pageB", "pageC"] :: IO [HTML]

-- for-loop style
forforConcurrently ["pageA", "pageB", "pageC"] queryUrl :: IO [HTML]
```

---

# Concurrency

Simple yet powerful

```haskell
mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
forConcurrently :: [a] -> (a -> IO b) -> IO [b]

queryUrl:: Url -> IO HTML

mapConcurrently queryUrl ["pageA", "pageB", "pageC"] :: IO [HTML]
forforConcurrently ["pageA", "pageB", "pageC"] queryUrl :: IO [HTML]
```


You won't see something like "WaitGroup" to count jobs, or using "Channel" to send message here.

Haskell is SO GOOD at abstraction, haskellers are too lazy to write same stuff (ceremony) again and again and again ans again.

---

# Concurrency

sequential, concurrent, you name it

```haskell
-- Concurrent
mapConcurrently :: (a -> IO b) -> [a] -> IO [b]
forConcurrently :: [a] -> (a -> IO b) -> IO [b]

-- Sequential, same type but different semantics
mapM            :: (a -> IO b) -> [a] -> IO [b]
for             :: [a] -> (a -> IO b) -> IO [b]
```

```haskell
-- Concurrent
mapConcurrently queryUrl ["pageA", "pageB", "pageC"] :: IO [HTML]
forforConcurrently ["pageA", "pageB", "pageC"] queryUrl :: IO [HTML]

-- Sequential
mapM queryUrl ["pageA", "pageB", "pageC"] :: IO [HTML]
for ["pageA", "pageB", "pageC"] queryUrl :: IO [HTML]
```

---

# Cancellation

Stop the job please

---

# Cancellation

First, we create job

```haskell
forkIO :: IO () -> IO ThreadId
```

```haskell
main = do
  tidA <- forkIO (putStrLn "hello from thread")
  tidB <- forkIO (putStrLn "hello from another thread")
  --- other stuff
```

In Haskell, "thread" is lightweight on both thread creation and context switching, not like OS-thread. It's call **green thraed** sometimes.

Sounds familiar? It's like concurrent model of Go!

---

# Cancellation

Second, we stop/kill it

```haskell
killThread :: ThreadId -> IO () 
throwTo :: Exception e => ThreadId -> e -> IO () 
```

```haskell
longJobA :: IO ()
longJobB :: IO ()

main = do
  tidA <- forkIO longJobA
  tidB <- forkIO longJobB
  --- on some condition
  killThread tiA
  throwTo ThisIsBadException tidB
```

How to stop a thread? Throw exception on it!

This is called "asynchronous exception"

---

# Cancellation

Clean up

Actually, we'v mentioned them

```haskell
finally :: IO a -> IO b -> IO a
onException :: IO a -> IO b -> IO a 
```

Asynchronous exception is just like exception, if you already specified cleanup process for normal exception, (at most case) you don't need extra work.

(well, there are some detail designs about asynchronous exception but for the purpose of this talk I won't mention them here)

---

# Cancellation

Conclusion

- use `forkIO` to fork `IO ()` to get `ThreadId`
- use `killThread`, `throwTo` and `throwTo` to notify thread
- cancellation is built in in the language, every `IO` is cancellable
  - you don't need something like "Context" to propagate the stop signal
  - you don't need two version of API: normal one and cancellable one
- handling cancellation is (almost) like handling exception

---

# Some comparisons

I'm sorry but we're GOing to compare Haskell with Go

---

# Some comparisons

Specification:

- sequential, no cancellation
  - running 10 jobs **sequentially**, collect their result then print them on screen
- concurrent, no cancellation
  - running 10 jobs **concurrently**, collect their result then print them on screen
- concurrent, with cancellation
  - running 10 jobs **concurrently**, collect their result then print them on screen
  - set timeout, if 10 jobs run too slow, cancel all the running jobs, discard all results and print "oh no too slow" string on screen
  
---

# Let's Go first!

---

# Some comparisons

Go, sequential, no cancellation

```go
package main

import "fmt"

func jobs(i int) string { ... }

func main() {
    var results []string
    for i := 1; i <= 10; i++ {
        result := jobs(i)
        results = append(results, result)
    }

    for _, v := range results {
        fmt.Println(v)
    }
}
```

---

# Some comparisons

Go, concurrent, no cancellation

```go
package main

import (
	"fmt"
	"sync"
)

func jobs(i int, wg *sync.WaitGroup, results chan<- string)
{ ... }
// to next page...
```

oh my it's too long

---

```go
func main() {
	var wg sync.WaitGroup
	resultChan := make(chan string)

	for i := 1; i <= 10; i++ {
		wg.Add(1)
		go jobs(i, &wg, resultChan)
	}

	go func() {
		wg.Wait()
		close(resultChan)
	}()

	var results []string
	for r := range resultChan {
		results = append(results, r)
	}

	for _, v := range results {
		fmt.Println(v)
	}
}
```

---

# Some comparisons

Go, concurrent, with cancellation

```go
package main

import (
	"context"
	"fmt"
	"sync"
	"time"
)

func jobs(i int, wg *sync.WaitGroup,
	results chan<- string, ctx context.Context)
{ ... }
// to next page...
```

oh my, longer

---

```go
func main() {
	var wg sync.WaitGroup
	resultChan := make(chan string)
	ctx, cancel := context.WithTimeout(context.Background(),
		100*time.Millisecond)
	defer cancel()

	for i := 1; i <= 10; i++ {
		wg.Add(1)
		go jobs(i, &wg, resultChan, ctx)
	}

	go func() {
		wg.Wait()
		close(resultChan)
	}()
	// to next page...
```

---

```go
	select {
	case <-ctx.Done(): // Timed out
		fmt.Println("oh no too slow")

	default:
		var results []string
		for r := range resultChan {
			results = append(results, r)
		}

		for _, v := range results {
			fmt.Println(v)
		}
	}
}
```

oh long gopher

---

# Let's Haskell a bit!

---

# Some comparisons

Haskell, sequential, no cancellation

```haskell
module Main where

import Data.Foldable
import Data.Traversable

jobs :: Int -> IO String

main :: IO ()
main = do
  results <- for             [1..10] jobs
  for_ results putStrLn
```

---

# Some comparisons

Haskell, concurrent, no cancellation

```haskell
module Main where

import Data.Foldable
import Control.Concurrent.Async

jobs :: Int -> IO String

main :: IO ()
main = do
  results <- forConcurrently [1..10] jobs
  for_ results putStrLn
```

---

# Some comparisons

Haskell, concurrent, with cancellation

```haskell
timeout :: Int -> IO a -> IO (Maybe a) -- in nano second
```

```haskell
module Main where

import Data.Foldable
import Control.Concurrent.Async
import System.Timeout

jobs :: Int -> IO String

main :: IO ()
main = do
  maybeResults <- timeout (1000 * 1000) (forConcurrently [1..10] jobs)
  case maybeResults of
    Just results -> for_ results putStrLn
    Nothing -> putStrLn "oh no too slow"
```

---

# Some comparisons

What happens?

the **DEFAULT** in language design plays an important role.

it's like Vim v.s. IDE:

you can achieve IDE experience in Vim, but require more effort

it's less flexible while using IDE, but you got almost what you need out of the box


also, generics is SUPER-DUPER-MUST-HAVE feature all around the world to make you code more usable, clean, elegant and enjoyable to read

---

# Conclusions

---

# Why I believe Haskell is worth learning?

- it's VERY different from other programing language
- due to the flexible design, you could only find a certain useful technique in Haskell
  - like properties based testing
  - deterministic parallel programming
- it creates opportunities for you to think about programing language, human, and life
  - why other language don't do that?
  - why OOP is the mainstream?
  - is it possible for two humans to fully understand each others?
  - can we avoid misunderstand during communication?
  - where's the limit of language?
  - would language affect how you think?
  
---

# Learning resources

---

# Learning resources

- Functional Thursday (Chinese)
  - https://www.facebook.com/FunctionalThursday/
  - people there are happy to answer questions

- Haskell subreddit (English)
  - https://www.reddit.com/r/haskell/
  - there is **Monthly Hask Anything** thread for newcomer to ask question of any level
    
- Channel of Philipp Hagenlocher (English)
  - https://www.youtube.com/channel/UC3xdLFFsqG701QAyGJIPT1g/featured
  - great video tutorial
  
---

# Learning resources

- Learn You a Haskell (English)
  - http://learnyouahaskell.com/
  - book that teaching Haskell
  - I think it's not the best material to learn Haskell from scratch, but you can use it to learn specific topic
  
---
layout: cover
---

# Thank you for joining!

Hope you start thinking weird question after this talk

