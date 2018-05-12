package com.ucu.pfp.closestpair

import org.scalameter._
import parallel.parallel._

import scala.collection.mutable.ListBuffer
import scala.util.Random


object PrlClosestPair {

  case class Point(x: Double, y: Double){
    def distance(p: Point) = math.hypot(x-p.x, y-p.y)

    override def toString = "(" + x + ", " + y + ")"
  }

  case class Pair(point1: Point, point2: Point) {
    val distance: Double = point1 distance point2

    override def toString = {
      point1 + "-" + point2 + " : " + distance
    }
  }

  def sortByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  def sortByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }

  def divideAndConquer(points: List[Point]): Pair = {
    val pointsSortedByX = sortByX(points)
    val pointsSortedByY = sortByY(points)

    divideAndConquer(pointsSortedByX, pointsSortedByY)
  }

  def bruteForce(points: List[Point]): Pair = {
    val numPoints = points.size

    if (numPoints < 2)
      return null

    var pair = Pair(points(0), points(1))

    if (numPoints > 2) {
      for (i <- 0 until numPoints - 1) {
        val point1 = points(i)
        for (j <- i + 1 until numPoints) {
          val point2 = points(j)
          val distance = point1 distance point2
          if (distance < pair.distance)
            pair = Pair(point1, point2)
        }
      }
    }

    return pair
  }


  private def divideAndConquer(pointsSortedByX: List[Point], pointsSortedByY: List[Point]): Pair = {

    val numPoints = pointsSortedByX.size

    if(numPoints <= 3) {
      return bruteForce(pointsSortedByX)
    }

    val dividingIndex = numPoints >>> 1
    val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
    val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

    // left part
    var leftTempList = leftOfCenter.map(x => x)
    leftTempList = sortByY(leftTempList)
    var closestPair = divideAndConquer(leftOfCenter, leftTempList)

    // right part
    var rightTempList = rightOfCenter.map(x => x)
    rightTempList = sortByY(rightTempList)
    val closestPairRight = divideAndConquer(rightOfCenter, rightTempList)

    // compare
    if (closestPairRight.distance < closestPair.distance)
      closestPair = closestPairRight

    var tempList = List[Point]()
    val shortestDistance = closestPair.distance
    val centerX = rightOfCenter(0).x

    for (point <- pointsSortedByY) {
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList = tempList :+ point
    }

    closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
    closestPair
  }

  def parallelDivideAndConquer(points: List[Point]): Pair = {
    val pointsSortedByX = sortByX(points)
    val pointsSortedByY = sortByY(points)

    parallelDivideAndConquer(pointsSortedByX, pointsSortedByY)
  }

  private def parallelDivideAndConquer(pointsSortedByX: List[Point], pointsSortedByY: List[Point]): Pair = {
    val numPoints = pointsSortedByX.size
    if(numPoints <= 3) {
      return bruteForce(pointsSortedByX)
    }

    val dividingIndex = numPoints >>> 1
    val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
    val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

    var leftTempList = leftOfCenter.map(x => x)
    leftTempList = sortByY(leftTempList)
    //var closestPair = parallelDivideAndConquer(leftOfCenter, leftTempList)

    var rightTempList = rightOfCenter.map(x => x)
    rightTempList = sortByY(rightTempList)
    //val closestPairRight = parallelDivideAndConquer(rightOfCenter, rightTempList)

    var (closestPairLeft, closestPairRight) =  parallel(divideAndConquer(leftOfCenter, leftTempList), divideAndConquer(rightOfCenter, rightTempList))

    // compare
    var closestPair = if (closestPairRight.distance < closestPairLeft.distance) closestPairRight else closestPairLeft

    var tempList = List[Point]()
    val shortestDistance = closestPair.distance
    val centerX = rightOfCenter(0).x

    for (point <- pointsSortedByY) {
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList = tempList :+ point
    }

    closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
    closestPair
  }

  private def shortestDistanceF(tempList: List[Point], shortestDistance: Double, closestPair: Pair ): Pair = {
    var shortest = shortestDistance
    var bestResult = closestPair
    for (i <- 0 until tempList.size) {
      val point1 = tempList(i)
      for (j <- i + 1 until tempList.size) {
        val point2 = tempList(j)

        val distance = point1 distance point2
        if (distance < closestPair.distance) {
          bestResult = Pair(point1, point2)
          shortest = distance
        }
      }
    }

    bestResult
  }

  def main(args: Array[String]) {
    val numPoints = 1024

    val points = ListBuffer[Point]()
    val r = new Random()
    for (i <- 0 until numPoints) {
      points.+=:(new Point(r.nextDouble(), r.nextDouble()))
    }
    println("Generated " + numPoints + " random points")


    val closestPair = bruteForce(points.toList)
    println("\nBruteForce: " + closestPair)

    val dqClosestPair = divideAndConquer(points.toList)
    println("Divide and conquer: " + dqClosestPair)

    val pdqClosestPair = parallelDivideAndConquer(points.toList)
    println("Parallel divide and conquer: " + pdqClosestPair)


    if (closestPair.distance == dqClosestPair.distance && dqClosestPair.distance == pdqClosestPair.distance)
      println("\nMATCH")
    else
      println("\nSOMETHING WRONG")



    val standardConfig = config(
      Key.exec.minWarmupRuns -> 20,
      Key.exec.maxWarmupRuns -> 200,
      Key.exec.benchRuns -> 50,
      Key.verbose -> false) withWarmer (new Warmer.Default)

    val bruteForceTime = standardConfig measure {
      bruteForce(points.toList)
    }

    val sequentialDivideAndConquerTime = standardConfig measure {
      divideAndConquer(points.toList)
    }

    val parallelDivideAndConquerTime = standardConfig measure {
      parallelDivideAndConquer(points.toList)
    }

    println(s"\nbruteForceTime: $bruteForceTime")
    println(s"sequentialDivideAndConquerTime: $sequentialDivideAndConquerTime")
    println(s"parallelDivideAndConquerTime: $parallelDivideAndConquerTime")

  }
}