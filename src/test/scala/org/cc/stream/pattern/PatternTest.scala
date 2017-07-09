package org.cc.stream.pattern

import org.scalatest._

class PatternTest extends FlatSpec {

  import Pattern._
  import TestHelper._

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

  "any()" should "match any element" in {
    val p = elt('A') ~> any ~> elt('C')
    checkAll("ABC") { input => assert(ss('A','B','C')=== matching(p)(input)) }
    checkAll("AXC") { input => assert(ss('A','X','C')=== matching(p)(input)) }
    checkAll("AYC") { input => assert(ss('A','Y','C')=== matching(p)(input)) }
    checkAll("AZC") { input => assert(ss('A','Z','C')=== matching(p)(input)) }
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
    val p = elt('A') ~> many(elt('A') ~> elt('B')) ~> elt('C')
    // Matching
    checkAll("AC") { input => assert(ss('A','C') === matching(p)(input)) }
    checkAll("AABC") { input => assert(ss('A','A','B','C') === matching(p)(input)) }
    checkAll("AABABC") { input => assert(ss('A','A','B','A','B','C') === matching(p)(input)) }
    checkAll("AABABABC") { input => assert(ss('A','A','B','A','B','A','B','C') === matching(p)(input)) }
    // Not matching
    checkAll("ABC") { input => assert(Failure === matching(p)(input)) }
  }

  "Nested patterns" should "be supported at an arbitrary depth" in {
    val p = elt('A') ~> many(elt('X') | (elt('Y') | elt('Z'))) ~> elt('C')
    // Matching
    checkAll("AC") { input => assert(ss('A','C') === matching(p)(input)) }
    checkAll("AXXC") { input => assert(ss('A','X','X','C') === matching(p)(input)) }
    checkAll("AXZYC") { input => assert(ss('A','X','Z','Y','C') === matching(p)(input)) }
    checkAll("AXXYYZZC") { input => assert(ss('A','X','X','Y','Y','Z','Z','C') === matching(p)(input)) }
    // Not matching
    assert(Failure === matching(p)("ABC".toStream))
    assert(Failure === matching(p)("AXYZXYZXYZBC".toStream))
  }

  "Matching a sliding pattern from a the start of a stream" should "be possible" in {
    val p = many(elt('A')) ~> elt('B') ~> elt('C')
    // Matching
    checkAll("BC") { input => assert(ss('B','C') === matching(p)(input.toStream)) }
    checkAll("ABC") { input => assert(ss('A','B','C') ++ ss('B','C') === matching(p)(input)) }
    checkAll("AABC") { input => assert(ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)(input)) }
    checkAll("AAAABC") { input => assert(ss('A','A','A','A','B','C') ++ ss('A','A','A','B','C') ++ ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)(input)) }
  }

}
