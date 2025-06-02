# QuickQuickCheck

quickquickcheck is a tool for Haskell developers that enables an annotation-like system for running property-based tests using the QuickCheck library. By placing special annotations in comments above your function declarations, quickquickcheck automatically discovers these annotations and runs the specified QuickCheck properties on the corresponding functions.

## Features

- **Annotation-based Testing**: Define which QuickCheck properties to run using simple comment annotations above your functions.
- **Automatic Discovery**: Scans your Haskell source files for annotated functions and runs the desired QuickCheck tests.
- **Seamless Integration**: No need to modify your function signatures or add boilerplate code—just annotate and test.

## Example Usage with Annotation Styles# quickquickcheck

quickquickcheck is a tool for Haskell developers that enables an annotation-like system for running property-based tests using the QuickCheck library. By placing special annotations in comments above your function declarations, quickquickcheck automatically discovers these annotations and runs the specified QuickCheck properties on the corresponding functions.

## Features

- **Annotation-based Testing**: Define which QuickCheck properties to run using simple comment annotations above your functions.
- **Automatic Discovery**: Scans your Haskell source files for annotated functions and runs the desired QuickCheck tests.
- **Seamless Integration**: No need to modify your function signatures or add boilerplate code—just annotate and test.

## Example Usage with Annotation Styles

With `quickquickcheck`, you can use annotation comments to specify properties directly above your function definitions. The annotation uses a mathematical property format, such as `f(x) == g(x)`, to describe the expected behavior. Here are some example styles of annotations you can use:

- **Inequality**: `-- @quickcheck increment(x) > x`
- **Equality**: `-- @quickcheck double(x) == x + x`
- **Property on lists**: `-- @quickcheck listLength(xs) >= 0`
- **Commutativity**: `-- @quickcheck addCommutative(x, y) == addCommutative(y, x)`
- **Associativity**: `-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))`

### Example 1: Increment Function

```haskell
-- @quickcheck increment(x) > x
increment :: Int -> Int
increment x = x + 1
```

### Example 2: Double Function

```haskell
-- @quickcheck double(x) == x + x
double :: Int -> Int
double x = x * 2
```

### Example 3: List Length Function

```haskell
-- @quickcheck listLength(xs) >= 0
listLength :: [a] -> Int
listLength xs = length xs
```

### Example 4: Commutative Addition

```haskell
-- @quickcheck addCommutative(x, y) == addCommutative(y, x)
addCommutative :: Int -> Int -> Int
addCommutative x y = x + y
```

### Example 5: Associative Addition

```haskell
-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))
addAssociative :: Int -> Int -> Int -> Int
addAssociative x y z = x + (y + z)
```

And many more...

With `quickquickcheck`, you can use annotation comments to specify properties directly above your function definitions. The annotation uses a mathematical property format, such as `f(x) == g(x)`, to describe the expected behavior. Here are some example styles of annotations you can use:

- **Inequality**: `-- @quickcheck increment(x) > x`
- **Equality**: `-- @quickcheck double(x) == x + x`
- **Property on lists**: `-- @quickcheck listLength(xs) >= 0`
- **Commutativity**: `-- @quickcheck addCommutative(x, y) == addCommutative(y, x)`
- **Associativity**: `-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))`

### Example 1: Increment Function

```haskell
-- @quickcheck increment(x) > x
increment :: Int -> Int
increment x = x + 1
```

### Example 2: Double Function

```haskell
-- @quickcheck double(x) == x + x
double :: Int -> Int
double x = x * 2
```

### Example 3: List Length Function

```haskell
-- @quickcheck listLength(xs) >= 0
listLength :: [a] -> Int
listLength xs = length xs
```

### Example 4: Commutative Addition

```haskell
-- @quickcheck addCommutative(x, y) == addCommutative(y, x)
addCommutative :: Int -> Int -> Int
addCommutative x y = x + y
```

### Example 5: Associative Addition

```haskell
-- @quickcheck addAssociative(x, # QuickQuickCheck

quickquickcheck is a tool for Haskell developers that enables an annotation-like system for running property-based tests using the QuickCheck library. By placing special annotations in comments above your function declarations, quickquickcheck automatically discovers these annotations and runs the specified QuickCheck properties on the corresponding functions.

## Features

- **Annotation-based Testing**: Define which QuickCheck properties to run using simple comment annotations above your functions.
- **Automatic Discovery**: Scans your Haskell source files for annotated functions and runs the desired QuickCheck tests.
- **Seamless Integration**: No need to modify your function signatures or add boilerplate code—just annotate and test.

## Example Usage with Annotation Styles# quickquickcheck

quickquickcheck is a tool for Haskell developers that enables an annotation-like system for running property-based tests using the QuickCheck library. By placing special annotations in comments above your function declarations, quickquickcheck automatically discovers these annotations and runs the specified QuickCheck properties on the corresponding functions.

## Features

- **Annotation-based Testing**: Define which QuickCheck properties to run using simple comment annotations above your functions.
- **Automatic Discovery**: Scans your Haskell source files for annotated functions and runs the desired QuickCheck tests.
- **Seamless Integration**: No need to modify your function signatures or add boilerplate code—just annotate and test.

## Example Usage with Annotation Styles

With `quickquickcheck`, you can use annotation comments to specify properties directly above your function definitions. The annotation uses a mathematical property format, such as `f(x) == g(x)`, to describe the expected behavior. Here are some example styles of annotations you can use:

- **Inequality**: `-- @quickcheck increment(x) > x`
- **Equality**: `-- @quickcheck double(x) == x + x`
- **Property on lists**: `-- @quickcheck listLength(xs) >= 0`
- **Commutativity**: `-- @quickcheck addCommutative(x, y) == addCommutative(y, x)`
- **Associativity**: `-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))`

### Example 1: Increment Function

```haskell
-- @quickcheck increment(x) > x
increment :: Int -> Int
increment x = x + 1
```

### Example 2: Double Function

```haskell
-- @quickcheck double(x) == x + x
double :: Int -> Int
double x = x * 2
```

### Example 3: List Length Function

```haskell
-- @quickcheck listLength(xs) >= 0
listLength :: [a] -> Int
listLength xs = length xs
```

### Example 4: Commutative Addition

```haskell
-- @quickcheck addCommutative(x, y) == addCommutative(y, x)
addCommutative :: Int -> Int -> Int
addCommutative x y = x + y
```

### Example 5: Associative Addition

```haskell
-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))
addAssociative :: Int -> Int -> Int -> Int
addAssociative x y z = x + (y + z)
```

And many more...

With `quickquickcheck`, you can use annotation comments to specify properties directly above your function definitions. The annotation uses a mathematical property format, such as `f(x) == g(x)`, to describe the expected behavior. Here are some example styles of annotations you can use:

- **Inequality**: `-- @quickcheck increment(x) > x`
- **Equality**: `-- @quickcheck double(x) == x + x`
- **Property on lists**: `-- @quickcheck listLength(xs) >= 0`
- **Commutativity**: `-- @quickcheck addCommutative(x, y) == addCommutative(y, x)`
- **Associativity**: `-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))`

### Example 1: Increment Function

```haskell
-- @quickcheck increment(x) > x
increment :: Int -> Int
increment x = x + 1
```

### Example 2: Double Function

```haskell
-- @quickcheck double(x) == x + x
double :: Int -> Int
double x = x * 2
```

### Example 3: List Length Function

```haskell
-- @quickcheck listLength(xs) >= 0
listLength :: [a] -> Int
listLength xs = length xs
```

### Example 4: Commutative Addition

```haskell
-- @quickcheck addCommutative(x, y) == addCommutative(y, x)
addCommutative :: Int -> Int -> Int
addCommutative x y = x + y
```

### Example 5: Associative Addition

```haskell
-- @quickcheck addAssociative(x, y, z) == addAssociative(x, addAssociative(y, z))
addAssociative :: Int -> Int -> Int -> Int
addAssociative x y z = x + (y + z)
```

And many more...y, z) == addAssociative(x, addAssociative(y, z))
addAssociative :: Int -> Int -> Int -> Int
addAssociative x y z = x + (y + z)
```

And many more...