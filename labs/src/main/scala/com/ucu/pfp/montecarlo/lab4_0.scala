package com.ucu.pfp.montecarlo

import org.scalameter._
import parallel.parallel._

import scala.util.Random

object lab4_0 extends App {

  val i = 1000000

  def mcCount(iter: Int): Int = {

    val randomX = new Random
    val randomY = new Random

    var hits = 0
    for (i <- 0 until iter) {

      val x = randomX.nextDouble
      val y = randomY.nextDouble

      if (x*x + y*y < 1) hits += 1
    }

    hits
  }


  def sequentialMonteCarloPi(iter: Int): Double = 4.0 * mcCount(iter) / iter


  def parallelMonteCarloPi(iter: Int): Double = {

    val ((pi1, pi2), (pi3, pi4)) = parallel( parallel(mcCount(iter/4), mcCount(iter/4)),
                                             parallel(mcCount(iter/4), mcCount(iter - 3*(iter/4)))
    )

    4.0 * (pi1 + pi2 + pi3 + pi4) / iter
  }


  println("Sequential: " + sequentialMonteCarloPi(i))
  println("Parallel: " + parallelMonteCarloPi(i))


  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 500,
    Key.exec.benchRuns -> 50,
    Key.verbose -> false) withWarmer (new Warmer.Default)

  val sequentialMonteCarloPiTime = standardConfig measure {
    sequentialMonteCarloPi(i)
  }

  val parallelMonteCarloPiTime = standardConfig measure {
    parallelMonteCarloPi(i)
  }


  println(s"sequentialMonteCarloPiTime: $sequentialMonteCarloPiTime")
  println(s"parallelMonteCarloPiTime: $parallelMonteCarloPiTime")

}
