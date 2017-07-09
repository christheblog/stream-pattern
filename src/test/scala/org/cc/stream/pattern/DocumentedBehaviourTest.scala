package org.cc.stream.pattern

import org.scalatest.FlatSpec


// Documenting corner cases and implementation specific limitations here
class DocumentedBehaviourTest extends FlatSpec {

  import Pattern._
  import TestHelper._


  // Will the matching be able to match against a pattern like this : "A(B*)BC" which is equivalent to "A(B+)C"
  "Matching A(B*)BC" should "fail because of the greedy implementation of many()" in {
    val p = elt('A') ~> many(elt('B')) ~> elt('B') ~> elt('C')
    // Not Matching
    assert(Failure === matching(p)("ABBBC".toStream))
  }

  it should "work if we use equivalent pattern A(B+)C instead" in {
    val p = elt('A') ~> atLeast(1,elt('B')) ~> elt('C')
    // Matching
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input.toStream)) }
  }

  it should "work if we use equivalent pattern AB(B*)C instead" in {
    val p = elt('A') ~> elt('B') ~> many(elt('B')) ~> elt('C')
    // Matching
    checkAll("ABBBC") { input => assert(ss('A','B','B','B','C') === matching(p)(input.toStream)) }
  }

}
