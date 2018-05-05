package com.ucu.pfp.tinylang_old

object lab3 extends App {

  println("Test")
  println(Number(42).show)
  println(Sum(Number(1), Number(2)).show)
  println(Prod(Number(2), Number(3)).show)


  val expression = Sum(
    Prod(Number(2), Number(3)),
    Prod(Number(4), Number(5))
  )

  println(new Machine(Exp(expression), Map[String, Expr]()).run)

  val expr_2 = Prod(Sum(Var("x"), Number(2)),  Sum(Number(4), Var("y")))

  val map_2 = Map("x" -> Number(1),"y" -> Number(3))

  println(new Machine(Exp(expr_2), map_2).run)

}
