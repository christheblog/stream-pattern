package org.cc.stream.pattern


object TestHelper {

  val Failure = Stream.Empty

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
