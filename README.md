# Stream-pattern

This project is just a simplistic regexp like language to match a sequence of elements in a stream.

My personal goal was to keep the implementation of the combinator and matching algorithm simple and clean - but this comes with some limitations.
This implementation hasn't been tested against big volume of data either.


## Usage

To use the matching language, you will need to import `Pattern._` as follow :
```scala
org.cc.stream.pattern.Pattern._
```

This will allow you to create patterns to match sequence of elements in streams.
<br>
A pattern is just an alias for a function:

```scala
  type Input[A] = Seq[A]
  type Matched[A] = Option[List[A]]
  type Pattern[A] = (Input[A], Matched[A]) => (Input[A], Matched[A])
```

You can create a pattern as follow:
```scala
val p: Pattern = eql(Set('A','Y','Z')) ~> elt('B') ~> elt('C')
```


## Examples
Here are some example of patterns you can create (comments are describing the pattern using regular expressions )

```scala
// A followed by B followed by C : ABC
elt('A') ~> elt('B') ~> elt('C')

// AB*C
elt('A') ~> many(elt('B')) ~> elt('C')

// AB{2,}C
elt('A') ~> atLeast(2,elt('B')) ~> elt('C')

// A.+C
elt('A') ~> any ~> elt('C')

// AB{3}C
elt('A') ~> exactly(3,of='B') ~> elt('C')


// Using nested patterns using |

// A((B*C) | D)
elt('A') ~> ((many('B') ~> 'C') | elt('D'))
// A(X|Y|Z)*C
elt('A') ~> many(elt('X') | (elt('Y') | elt('Z'))) ~> elt('C')
```

_Note : for simplicity, examples above are using characters, but it could be anything._


To get a stream of matches, you can use the following :

```scala
val p = many(elt('A')) ~> elt('B') ~> elt('C')
// Matching
val matched = matching(p)("AAAABC".toStream)
matched.foreach(println) // will output all possible match in a stream : AAAABC, AAABC, AABC, ABC, BC
```


## Limitations

- At the moment, the matching implementation doesn't work with infinite streams.
- The greedy implementation of the combinator `many()` make squences like `AB*BC` unmatchable. `ABB*C` would work.
