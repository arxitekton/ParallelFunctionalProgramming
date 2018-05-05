package com.ucu.pfp.sets

object lab2 extends App {

  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  // 2. Basic operation

  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  def union(s:Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)
  def symmetricDiff(s: Set, t: Set): Set = (x: Int) => (contains(s, x) && !contains(t, x)) || (!contains(s, x) && contains(t, x))

  // def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && contains(p, x)
  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p.asInstanceOf[Set])


  // Requests and transformation of sets

  val interval = (-1000, 1000)

  def forall(s: Set, p: Int => Boolean): Boolean = {

    def iter(a: Int): Boolean = {
      if (a > interval._2) true
      else if (contains(s, a) && !contains(p, a)) false // or previously val diff, than there used contains(diff,a)
      else iter(a + 1)
    }

    iter(interval._1)
  }

  // not all in s satisfies !p -> some do not satisfy !p -> some satisfy p
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x: Int) => !p(x))
  // for any y, if there exists x in s that satisfies the condition f(x) equals y [transformation], then y is in result set
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, (x: Int) => f(x) == y)


  // todo test

  val union_12 = union(singletonSet(1), singletonSet(2))

  println("is union12 contains 1: " + contains(union_12, 1))
  println("is union12 contains 2: " + contains(union_12, 2))
  println("is union12 contains 3: " + contains(union_12, 3))

  // test union
  val union_23 = union(singletonSet(2), singletonSet(3))
  // test intersect
  val intersect_12_23 = intersect(union_12, union_23)
  // test diff
  val diff_12_23 = diff(union_12, union_23)
  val symmetricDiff_12_23 = symmetricDiff(union_12, union_23)

  println("is intersect12_23 contains 1: " + contains(intersect_12_23, 1))
  println("is intersect12_23 contains 2: " + contains(intersect_12_23, 2))
  println("is intersect12_23 contains 3: " + contains(intersect_12_23, 3))

  println("is diff12_23 contains 1: " + contains(diff_12_23, 1))
  println("is diff12_23 contains 2: " + contains(diff_12_23, 2))
  println("is diff12_23 contains 3: " + contains(diff_12_23, 3))

  println("is symmetricDiff12_23 contains 1: " + contains(symmetricDiff_12_23, 1))
  println("is symmetricDiff12_23 contains 2: " + contains(symmetricDiff_12_23, 2))
  println("is symmetricDiff12_23 contains 3: " + contains(symmetricDiff_12_23, 3))

  // test filter
  val filter_12_2 = filter(union_12, singletonSet(2))

  println("is filter_12_2 contains 1: " + contains(filter_12_2, 1))
  println("is filter_12_2 contains 2: " + contains(filter_12_2, 2))
  println("is filter_12_2 contains 3: " + contains(filter_12_2, 3))


  val filter_12_23 = filter(union_12, union_23)

  println("is filter_12_23 contains 1: " + contains(filter_12_23, 1))
  println("is filter_12_23 contains 2: " + contains(filter_12_23, 2))
  println("is filter_12_23 contains 3 " + contains(filter_12_23, 3))

  // test exists
  println(exists(union_12, singletonSet(1)))
  println(exists(union_12, singletonSet(3)))

}
