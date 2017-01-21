package org.cc.stream.pattern

import org.scalatest._

class PatternTest extends FlatSpec {

  import Pattern._

  val Failure = Stream.Empty

  "eql() combinator" should "describe any matching element" in {
    val p = eql(Set('A','Y','Z')) ~> elt('B') ~> elt('C')
    // Match
    checkAll("ABC") { input => assert(ss('A','B','C') === matching(p)(input)) }
    checkAll("YBC") { input => assert(ss('Y','B','C') === matching(p)(input)) }
    checkAll("ZBC") { input => assert(ss('Z','B','C') === matching(p)(input)) }
    // Failure
    assert(Failure === matching(p)("WBC".toStream))
  }

  "~> combinator" should "describe an element followed by another element" in {
    val p = elt('A') ~> elt('B') ~> elt('C')
    checkAll("ABC") { input => assert(ss('A','B','C') === matching(p)(input)) }
  }

  "A pattern" should "be matched several times if it appears several times" in {
    val p = elt('A') ~> elt('B') ~> elt('C')
    checkAll("ABCABC") { input => assert(ss('A','B','C') ++ ss('A','B','C') === matching(p)(input)) }
  }

  "or() combinator" should "match either the first part or the second part of the 'or'." in {
    val p = elt('A') ~> ((many('B') ~> 'C') | elt('D'))
    // First case of the or
    checkAll("AC") { input => assert(ss('A','C') === matching(p)(input)) }
    checkAll("ABC") { input => assert(ss('A','B','C') === matching(p)(input)) }
    checkAll("ABBC") { input => assert(ss('A','B','B','C') === matching(p)(input)) }
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input)) }
    checkAll("ABBBBC") { input => assert(ss('A','B','B','B','B','C') === matching(p)(input)) }
    // Second case of the or
    checkAll("AD") { input => assert(ss('A','D') === matching(p)(input)) }
  }

  "many() combinator" should "should match zero or more elements" in {
    val p = elt('A') ~> many('B') ~> elt('C')
    checkAll("AC") { input => assert(ss('A','C') === matching(p)(input)) }
    checkAll("ABC") { input => assert(ss('A','B','C') === matching(p)(input)) }
    checkAll("ABBC") { input => assert(ss('A','B','B','C') === matching(p)(input)) }
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input)) }
  }

  "exactly() combinator" should "match exactly n elements" in {
    val p = elt('A') ~> exactly(3,of='B') ~> elt('C')
    // Matching
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input)) }
    // Failures
    checkAll("AC") { input => assert(Failure === matching(p)(input)) }
    checkAll("ABC") { input => assert(Failure === matching(p)(input)) }
    checkAll("ABBC") { input => assert(Failure ===  matching(p)(input)) }
    checkAll("ABBBBC") { input => assert(Failure ===  matching(p)(input)) }
    checkAll("ABBBBBC") { input => assert(Failure ===  matching(p)(input)) }
  }

  "atLeast() combinator" should "match n or more elements" in {
    val p = elt('A') ~> atLeast(3,of='B') ~> elt('C')
    // Matching
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input)) }
    checkAll("ABBBBC") { input => assert(ss('A','B','B','B','B','C') ===  matching(p)(input)) }
    checkAll("ABBBBBC") { input => assert(ss('A','B','B','B','B','B','C') ===  matching(p)(input)) }
    // Failure
    checkAll("AC") { input => assert(Failure === matching(p)(input)) }
    checkAll("ABC") { input => assert(Failure === matching(p)(input)) }
    checkAll("ABBC") { input => assert(Failure ===  matching(p)(input)) }
  }

  "atMost() combinator" should "match n or less elements" in {
    val p = elt('A') ~> atMost(3,of='B') ~> elt('C')
    checkAll("AC") { input => assert(ss('A','C') === matching(p)(input)) }
    checkAll("ABC") { input => assert(ss('A','B','C') === matching(p)(input)) }
    checkAll("ABBC") { input => assert(ss('A','B','B','C') === matching(p)(input)) }
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input)) }
    checkAll("ABBBBC") { input => assert(Failure === matching(p)(input)) }
    checkAll("ABBBBC") { input => assert(Failure === matching(p)(input)) }
  }

  "Nested patterns" should "be supported" in {
    val p = elt('A') ~> many(elt('A') ~> ('B')) ~> elt('C')
    // Matching
    assert(ss('A','C') === matching(p)("AC".toStream))
    assert(ss('A','A','B','C') === matching(p)("AABC".toStream))
    assert(ss('A','A','B','A','B','C') === matching(p)("AABABC".toStream))
    assert(ss('A','A','B','A','B','A','B','C') === matching(p)("AABABABC".toStream))
    // Not matching
    assert(Failure === matching(p)("ABC".toStream))
  }

  "Matching a sliding pattern from a the start of a stream" should "be possible" in {
    val p = many(elt('A')) ~> elt('B') ~> elt('C')
    // Matching
    checkAll("BC") { input => assert(ss('B','C') === matching(p)(input.toStream)) }
    assert(ss('A','B','C') ++ ss('B','C') === matching(p)("ABC".toStream))
    assert(ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)("AABC".toStream))
    assert(ss('A','A','A','A','B','C') ++ ss('A','A','A','B','C') ++ ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)("AAAABC".toStream))
  }


  // FIXME add tests to challenge grredy behaviour of many(), atMost and at Least()
  // Will the matching be able to match against a pattern like this : "A(B*)BC" which is equivalent to "A(B+)C"

  // Helpers

  def ss[A](a:A*) = Stream(Some(a.toList))

  // Generates various combination of the input string
  def checkAll(input: String)(f: (Stream[Char]) => Unit): Unit = {
    f(input.toStream)        // just the input
    f(("---"+input).toStream)  // at the end
    f((input+"---").toStream)  // at the begining
    f(("---" + input + "---").toStream) // in the middle
  }

}
