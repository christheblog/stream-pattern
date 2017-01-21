package org.cc.stream.pattern

import org.scalatest._

class PatternTest extends FlatSpec {

  import Pattern._

  val Failure = Stream.Empty

  "eql() combinator" should "describe any matching element" in {
    val p = eql(Set('A','Y','Z')) ~> elt('B') ~> elt('C')
    assert(ss('A','B','C') === matching(p)("ABC".toStream))
    assert(ss('Y','B','C') === matching(p)("YBC".toStream))
    assert(ss('Z','B','C') === matching(p)("ZBC".toStream))
    assert(Failure === matching(p)("WBC".toStream))
  }

  "~> combinator" should "describe an element followed by another element" in {
    val p = elt('A') ~> elt('B') ~> elt('C')
    assert(ss('A','B','C') === matching(p)("ABC".toStream))
  }

  "~> combinator" should "be able to describe a simple char sequence" in {
    val p = elt('A') ~> elt('B') ~> elt('C')
    assert(ss('A','B','C') === matching(p)("ABC".toStream))
  }

  "A pattern" should "be matched several times" in {
    val p = elt('A') ~> elt('B') ~> elt('C')
    assert(ss('A','B','C') ++ ss('A','B','C') === matching(p)("ABCABCDEFG".toStream))
  }

  "or() combinator" should "match either the first part or the second part of the 'or'." in {
    val p = elt('A') ~> ((many('B') ~> 'C') | elt('D'))
    // First case of the or
    assert(ss('A','C') === matching(p)("AC".toStream))
    assert(ss('A','B','C') === matching(p)("ABC".toStream))
    assert(ss('A','B','B','C') === matching(p)("ABBC".toStream))
    assert(ss('A','B','B','B','C') === matching(p)("ABBBC".toStream))
    assert(ss('A','B','B','B','C') === matching(p)("ABBBCD".toStream))
    // Second case of the or
    assert(ss('A','D') === matching(p)("AD".toStream))
  }

  "many() combinator" should "should match zero or more elements" in {
    val p = elt('A') ~> many('B') ~> elt('C')
    assert(ss('A','C') === matching(p)("AC".toStream))
    assert(ss('A','B','C') === matching(p)("ABC".toStream))
    assert(ss('A','B','B','C') === matching(p)("ABBC".toStream))
    assert(ss('A','B','B','B','C') === matching(p)("ABBBC".toStream))
  }

  "Embedded patterns" should "be supported" in {
    val p = elt('A') ~> many(elt('A') ~> ('B')) ~> elt('C')
    // Matching
    assert(ss('A','C') === matching(p)("AC".toStream))
    assert(ss('A','A','B','C') === matching(p)("AABC".toStream))
    assert(ss('A','A','B','A','B','C') === matching(p)("AABABC".toStream))
    assert(ss('A','A','B','A','B','A','B','C') === matching(p)("AABABABC".toStream))
    assert(ss('A','A','B','A','B','A','B','C') === matching(p)("-----AABABABC".toStream))
    assert(ss('A','A','B','A','B','A','B','C') === matching(p)("AABABABC-----".toStream))
    assert(ss('A','A','B','A','B','A','B','C') === matching(p)("--AABABABC---".toStream))
    // Not matching
    assert(Failure === matching(p)("ABC".toStream))
  }

//  "atLeast() combinator" should "match n or more elements" in {
//    val p = elt('A') ~> atLeast(3,of='B') ~> elt('C')
//    assert(ss('A','C') === matching(p)("AC".toStream))
//    assert(ss('A','B','C') === matching(p)("ABC".toStream))
//    assert(ss('A','B','B','C') === matching(p)("ABBC".toStream))
//    assert(ss('A','B','B','B','C') === matching(p)("ABBBC".toStream))
//  }
//
//  "atMost() combinator" should "match n or less elements" in {
//    val p = elt('A') ~> atMost(3,of='B') ~> elt('C')
//    assert(ss('A','C') === matching(p)("AC".toStream))
//    assert(ss('A','B','C') === matching(p)("ABC".toStream))
//    assert(ss('A','B','B','C') === matching(p)("ABBC".toStream))
//    assert(ss('A','B','B','B','C') === matching(p)("ABBBC".toStream))
//    assert(Failure === matching(p)("ABBBBC".toStream))
//    assert(Failure === matching(p)("ABBBBBC".toStream))
//  }

  "Matching a sliding pattern from a the start of a stream" should "be possible" in {
    val p = many(elt('A')) ~> elt('B') ~> elt('C')
    // Matching
    assert(ss('B','C') === matching(p)("BC".toStream))
    assert(ss('A','B','C') ++ ss('B','C') === matching(p)("ABC".toStream))
    assert(ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)("AABC".toStream))
    assert(ss('A','A','A','A','B','C') ++ ss('A','A','A','B','C') ++ ss('A','A','B','C') ++ ss('A','B','C') ++ ss('B','C') === matching(p)("AAAABC".toStream))
  }

  "Matching a sliding pattern from a the end of a stream" should "be possible" in {
    val p = elt('A') ~> elt('B') ~> many(elt('C'))
    // Matching
    assert(ss('A','B','C','C','C') ++ ss('A','B','C','C') ++ ss('A','B','C') ++ ss('A','B','C') === matching(p)("ABCCC--".toStream))
  }

  "Matching a sliding pattern from a the middle of a stream" should "be possible" in {
    val p = elt('A') ~> elt('B') ~> many(elt('C'))
    // Matching
    assert(ss('A','B','C','C','C') ++ ss('A','B','C','C') ++ ss('A','B','C') ++ ss('A','B','C') === matching(p)("----ABCCC----".toStream))
  }

  def ss[A](a:A*) = Stream(Some(a.toList))

}
