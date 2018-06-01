package com.ucu.pfp.wordcounter

import org.scalameter._

import scala.io.Source

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  def foldMap[T, Y](inseq: IndexedSeq[T], m: Monoid[Y])(f: T => Y) : Y =
    inseq.foldLeft(m.zero) {
      case(t, y) => m.op(t, f(y))
    }

  def foldMapPar[T, Y](inseq: IndexedSeq[T], m: Monoid[Y])(f: T => Y) : Y =
    inseq.par.foldLeft(m.zero) {
    case(t, y) => m.op(t, f(y))
  }


  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (a: Part, b: Part) => Part(a.lStub, a.words + (if ((a.rStub + b.lStub).isEmpty) 0 else 1) + b.words, b.rStub)
      case (a: Part, b: Stub) => Part(a.lStub, a.words, a.rStub + b.chars)
      case (a: Stub, b: Part) => Part(a.chars + b.lStub, b.words, b.rStub)
      case (a: Stub, b: Stub) => Stub(a.chars + b.chars)
    }
    def zero: WC = Stub("")
  }

  def countWordsSeq(str: String): Int = {
    val wc = foldMap(str.toIndexedSeq, wcMonoid)(c =>
      if (c == ' ' || c == ','  || c == '.') Part("", 0, "")
      else Stub(c.toString))

    wc match {
      case Part (l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }
  }

  def countWordsPar(str: String): Int = {
    val wc = foldMapPar(str.toIndexedSeq, wcMonoid)(c =>
      if (c == ' ' || c == ','  || c == '.') Part("", 0, "")
      else Stub(c.toString))

    wc match {
      case Part (l, w, r) => w + (if (l.isEmpty) 0 else 1) + (if (r.isEmpty) 0 else 1)
      case Stub(c) => if (c.isEmpty) 0 else 1
    }
  }

  def main(args: Array[String]): Unit = {

    val filename = "src/main/scala/com/ucu/pfp/wordcounter/some.txt"
    val fileContents = Source.fromFile(filename).getLines.mkString

    val countWordsSeq_count =  countWordsSeq(fileContents)
    val countWordsPar_count =  countWordsPar(fileContents)

    println(s"countWordsSeq count:   ${countWordsSeq_count}")
    println(s"countWordsPar count:     ${countWordsPar_count}")

    if (countWordsSeq_count != countWordsPar_count)
      println("MISMATCH")
    else
      println("MATCH")

    val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 200,
      Key.exec.benchRuns -> 50,
      Key.verbose -> false) withWarmer (new Warmer.Default)


    val seqTime = standardConfig.measure(countWordsSeq(fileContents))
    val parTime = standardConfig.measure(countWordsPar(fileContents))

    println(s"sequential time:  $seqTime")
    println(s"parallel time:    $parTime")


  }
}