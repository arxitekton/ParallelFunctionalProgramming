package com.ucu.pfp.montecarlo

import scala.util.Random

class MonteCarloIntegrator(f: Double => Double, numPoints: Int) {

  def integrate(from: Double, to: Double): Double = {
    // Get the minimum and maximum values for the function
    val (min, max) = getBounds(from, to)
    val width = to - from
    val height = if (min >= 0.0) max else max - min
    // compute the enclosing area (rectangle)
    val outerArea = width * height
    val randomx = new Random(System.currentTimeMillis)
    val randomy = new Random(System.currentTimeMillis + 42L)

    // Monte Carlo simulator for the  function
    def randomSquare: Double = {
      val numInsideArea = Range(0, numPoints)./:(0)(
        (s, n) => {
          val ptx = randomx.nextDouble * width + from
          val pty = randomy.nextDouble * height
          // update the seeds
          randomx.setSeed(randomy.nextLong)
          randomy.setSeed(randomx.nextLong)

          s + (if (pty > 0.0 && pty < f(ptx)) 1
          else if (pty < 0.0 && pty > f(ptx)) -1
          else 0)
        }
      )
      numInsideArea.toDouble * outerArea / numPoints
    }
    randomSquare
  }

  // Compute the bounds for the y values of the function
  private def getBounds(from: Double, to: Double): (Double, Double) = {
    def updateBounds(y: Double, minMax: (Double,Double)): Int = {
      var flag = 0x00
      if (y < minMax._1) flag += 0x01
      if (y > minMax._2) flag += 0x02
      flag
    }
    // extract the properties for the integration step
    val numSteps = Math.sqrt(numPoints).floor.toInt
    val stepSize = (to - from) / numSteps

    (0 to numSteps)./:((Double.MaxValue, -Double.MaxValue))(
      (minMax, n) => {
        val y = f(n * stepSize + from)
        updateBounds(y, minMax) match {
          case 0x01 => (y, minMax._2)
          case 0x02 => (minMax._1, y)
          case 0x03 => (y, y)
          case _ => minMax
        }
      }
    )
  }
}
