package com.ucu.pfp.wordcounter


object StreamWorkshop {

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Cons(h, t) => Some(h())
      case Empty => None
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => List()
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if (n > 0) =>
        Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) if (n > 0) =>
          t().drop(n - 1)
        case _ => this
      }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B]) {
      case (a, acc) => Stream.cons(f(a), acc)
    }

    def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A]) {
      case (a, acc) if f(a) => Stream.cons(a, acc)
      case (a, acc) => acc
    }

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](args: ( () => A   )*): Stream[A] = {
      if (args.isEmpty) empty
      else cons(args.head(), apply(args.tail : _*))
    }

  }

  def main(args: Array[String]): Unit = {
    val firstStream = Stream(
      () => { 2 },
      () => { 3 },
      () => { 4 },
      () => { 5 }
    )


    println(firstStream.toList)

    println(firstStream.take(3).toList)

    println(firstStream.drop(2).toList)

    println(firstStream.forAll(_ > 0))
    println(firstStream.forAll(_ > 5))

    println(firstStream.map(_ * 10).toList)

    println(firstStream.filter(_ % 2 == 0).toList)

  }

}