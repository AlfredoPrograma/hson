> Learning project implementing a JSON parser using **monadic parser combinators** in Haskell.

## Overview

This is a simple JSON parser. Basically, it receives a raw JSON string representation and parses it into a `JSONValue` representation within the language, so it’s possible to interact with its fields programmatically.

## Project Structure

* `Parser`:

  * `Monad.hs` -> Contains method implementations that prove the `Parser` type is actually a `Monad` instance.
  * `Combinators.hs` -> Contains basic parser combinator helpers for common structure patterns.
* `JSON`:

  * `Parser.hs` -> Contains all the JSON element parsers and also the recursive JSON generation based on the JSON syntax rules described [here](https://www.crockford.com/mckeeman.html).
* `Main.hs` -> Just the main entry point. It reads the file from the path provided as an argument and then parses the JSON if it is valid.

## Usage

Nothing too fancy. Just make sure you have `ghc` installed, preferably version `9.6.7`.

Then build the code by running:

```
ghc ./Main.hs
```

This will generate the `Main` binary. Run it by passing a JSON file as an argument. There is a `sample.json` file in the repo, so you can just run:

```
./Main ./sample.json
```

It will print the parsed output as a `JSONValue` representation in Haskell.

## Example

Input:

```json
{
  "name": "Alice",
  "admin": false,
  "age": 25,
  "friends": ["Maria", "Pedro"]
}
```

Output:

```
JSONObject (
  fromList [
    ("name",JSONString "Alice"),
    ("admin",JSONBoolean False),
    ("age",JSONNumber 25.0),
    ("friends",JSONArray [
      JSONString "Maria", 
      JSONString "Pedro"
    ])
)
```

## Learning Goals

I started learning Haskell and got a bit tired of just implementing dummy functions and running them in `ghci`. So I found an extremely interesting whitepaper about [Monadic Parser Combinators](https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf), and I decided to implement a simple JSON parser based on the monadic parsers concept.

Not gonna lie, it was kind of hard to understand at the beginning because of the need to understand Monads and their properties. Most of the learning time was spent there. However, once the Monad properties were implemented in my `Parser` type, development became really smooth. The declarative style actually made parsing JSON elements much easier.

So yeah, my learning goals were:

* Discover how to use Haskell outside of `ghci` and simple dummy functions.
* Understand Monads (and therefore Applicatives and Functors) and their applications.
* Learn how to provide a programmatic representation of a raw string based on a given syntax.

## Future Improvements

Obviously this is not the most useful project, but there are some improvements I would like to make in the future:

* Support escaped characters within strings.
* Add unit tests.
* Maybe turn it into a library.
