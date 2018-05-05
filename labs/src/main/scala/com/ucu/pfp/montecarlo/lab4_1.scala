package com.ucu.pfp.montecarlo

import org.scalameter._
import parallel.parallel._

object lab4_1 extends App {

  val fct = (x: Double) => 2 * x - 1.0
  val integral = (x: Double, c: Double) => x*x - x + c

  val from = 1.0
  val to = 2.0
  val numPoints = 100000

  val integrator = new MonteCarloIntegrator(fct, numPoints)

  val predicted = integrator.integrate(from, to)
  val expected = integral(to, 0.0) - integral(from, 0.0)

  println(s"predicted: $predicted")
  println(s"expected: $expected")


  def sequentialIntegration(f:Double => Double, from: Double, to: Double, numPoints: Int): Double = {
    val integrator = new MonteCarloIntegrator(f, numPoints)
    integrator.integrate(from, to)
  }

  def parallelIntegration(f:Double => Double, from: Double, to: Double, numPoints: Int): Double = {
    val integrator = new MonteCarloIntegrator(f, numPoints/4)
    val ((p1, p2), (p3, p4)) = parallel( parallel(integrator.integrate(from, to), integrator.integrate(from, to)),
      parallel(integrator.integrate(from, to), integrator.integrate(from, to))
    )
    (p1 + p2 + p3 + p4) / 4
  }

  println("Sequential: " + sequentialIntegration(fct, from, to, numPoints))

  println("Parallel: " + parallelIntegration(fct, from, to, numPoints))


  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 500,
    Key.exec.benchRuns -> 50,
    Key.verbose -> false) withWarmer (new Warmer.Default)

  val sequentialIntegrationTime = standardConfig measure {
    sequentialIntegration(fct, from, to, numPoints)
  }

  val parallelIntegrationTime = standardConfig measure {
    parallelIntegration(fct, from, to, numPoints)
  }



  println(s"sequentialIntegrationTime: $sequentialIntegrationTime")
  println(s"parallelIntegrationTime: $parallelIntegrationTime")

}
