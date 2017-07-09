package org.cc.stream.pattern


object Pattern {

  type Input[A] = Seq[A]
  type Matched[A] = Option[List[A]]
  type Pattern[A] = (Input[A], Matched[A]) => (Input[A], Matched[A])


  implicit def toPattern[A](a: A): Pattern[A] = elt(a)
  implicit def toPattern[A](a: PatternOps[A]): Pattern[A] = a.pattern
  implicit class PatternOps[A](val pattern: Pattern[A]) { self =>
    def ~>(p: Pattern[A]): Pattern[A] = andThen(self,p)
    def |(p: Pattern[A]): Pattern[A] = or(self,p)
    def *(n: Int): Pattern[A] = exactly(n,of=pattern)
  }


  def success[A](i: (Input[A],Matched[A])) = !i._2.isEmpty
  def fail[A](i: (Input[A],Matched[A])) = !success(i)


  // Combinators

  def eql[A](p: A => Boolean): Pattern[A] =
    (i: Input[A], acc: Matched[A]) =>
      i.headOption.filter(p).map { h =>  (i.tail, acc.map(h::_)) } getOrElse (i,None)

  def elt[A](a: A) = eql[A](_==a)

  // Wildcard for any element of A
  def any[A] = eql[A](_ => true)

  def not[A](p: Pattern[A]) =
    (i: Input[A], acc: Matched[A]) =>
      if(!success(p(i,acc))) (i.tail, acc.map(i.head::_))

  def or[A](p1: Pattern[A], p2: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      val (r1, m1) = p1(i,acc)
      lazy val (r2, m2) = p2(i,acc)
      if(success((r1, m1))) (r1, m1)
      else if (success((r2, m2))) (r2, m2)
      else (i,None)
    }

  def andThen[A](p1: Pattern[A], p2: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      val (r1, m1) = p1(i,acc)
      lazy val (r2, m2) = p2(r1,m1)
      if(fail((r1, m1))) (i,None)
      else if (fail((r2, m2))) (i,None)
      else (r2, m2)
    }

  // Greedy, matching 0 to an infinite number of occurences
  def many[A](p: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      val (remainder,matched) = p(i,acc)
      if(matched.isEmpty) (i,acc)
      else many(p)(remainder,matched)
    }

  def exactly[A](n: Int, of: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      if(n==0) (i,acc)
      else {
        val (remainder, matched) = of(i, acc)
        if (matched.isEmpty) (i, None)
        else exactly(n-1, of)(remainder, matched)
      }
    }

  // A minimum of n occurences of the pattern
  def atLeast[A](n: Int, of: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      // Using many to ensure greedy matching of n
      val (remainder,matched) = many(of)(i,Some(List.empty[A]))
      if(matched.exists(_.size >= n)) (remainder, for(m <- matched;prev<-acc) yield m ++ prev)
      else (i,None)
    }

  // Between 0 and n occurences of the pattern
  def atMost[A](n: Int, of: Pattern[A]): Pattern[A] =
    (i: Input[A], acc: Matched[A]) => {
      if(n==0) (i,acc)
      else {
        val (remainder, matched) = of(i, acc)
        if (matched.isEmpty) (i, acc)
        else atMost(n-1, of)(remainder, matched)
      }
    }

  // Matches a given pattern against an input stream
  // FIXME this implementation does NOT work with infinite streams
  def matching[A](p: Pattern[A])(against: Stream[A]): Stream[Matched[A]] = {
    against.tails.foldRight(Stream.empty[Matched[A]]) { case (input,acc) =>
      val (remainder, matched) = p(input, Some(List.empty[A]))
        if(matched.isEmpty) acc else matched.map(_.reverse) #:: acc
    }
  }

}
