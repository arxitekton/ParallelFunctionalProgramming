package com.ucu.pfp.tinylang_old

trait Expr {
  def isReducible: Boolean = true
  def reduce(env: Map[String, Expr]): Expr = this
  def reduced(env: Map[String, Expr]): Expr = {
    def go(exp: Expr): Expr = exp match {
      case e: Expr if !(e.isReducible) => e
      case e: Expr => go(e.reduce(env))
    }
    go(this)
  }
  def eval(env: Map[String, Expr]): Int = this.reduce(env).eval(env)
  def show: String = toString
}

object Expr {
  def isReducible(expression: Expr): Boolean = expression match {
    case _: Number => false
    case _: Expr => true
  }
}

case class Number(x: Int) extends Expr {
  override def isReducible: Boolean = false
  override def eval(env: Map[String, Expr]): Int = x
  override def toString: String = x.toString
}

case class Sum(x: Expr, y: Expr) extends Expr {
  override def reduce(env: Map[String, Expr]): Expr = this match {
    case Sum(lOp: Expr, rOp: Expr) if lOp.isReducible => Sum(lOp.reduce(env), rOp)
    case Sum(lOp: Expr, rOp: Expr) if rOp.isReducible => Sum(lOp, rOp.reduce(env))
    case _: Expr => Number(x.eval(env) + y.eval(env))
  }
  override def toString: String = s"(${x} + ${y})"
}

case class Prod(x: Expr, y: Expr) extends Expr {
  override def reduce(env: Map[String, Expr]): Expr = this match {
    case Prod(lOp: Expr, rOp: Expr) if lOp.isReducible => Prod(lOp.reduce(env), rOp)
    case Prod(lOp: Expr, rOp: Expr) if rOp.isReducible => Prod(lOp, rOp.reduce(env))
    case _: Expr =>  Number(x.eval(env) * y.eval(env))
  }
  override def toString: String = s"${x} * ${y}"
}

case class Bool(b: Boolean) extends Expr {
  override def reduce(env: Map[String, Expr]): Expr = if (b) TRUE() else FALSE()
  override def toString: String = if (b) TRUE().toString else FALSE().toString
}

case class TRUE() extends Expr {
  override def isReducible: Boolean = false
  override def toString: String = "true"
}

case class FALSE() extends Expr {
  override def isReducible: Boolean = false
  override def toString: String = "false"
}

case class LessThan(lOp: Expr, rOp: Expr) extends Expr {
  override def toString: String = s"${lOp} < ${rOp}"
  override def reduce(env: Map[String, Expr]): Expr = this match {
    case LessThan(lOp: Expr, rOp: Expr) if lOp.isReducible => LessThan(lOp.reduce(env), rOp)
    case LessThan(lOp: Expr, rOp: Expr) if rOp.isReducible => LessThan(lOp, rOp.reduce(env))
    case LessThan(lOp: Expr, rOp: Expr) => Bool(lOp.eval(env) < rOp.eval(env))
  }
}

case class GreaterThan(lOp: Expr, rOp: Expr) extends Expr {
  override def toString: String = s"${lOp} > ${rOp}"
  override def reduce(env: Map[String, Expr]): Expr = this match {
    case GreaterThan(lOp: Expr, rOp: Expr) if lOp.isReducible => GreaterThan(lOp.reduce(env), rOp)
    case GreaterThan(lOp: Expr, rOp: Expr) if rOp.isReducible => GreaterThan(lOp, rOp.reduce(env))
    case GreaterThan(lOp: Expr, rOp: Expr) => Bool(lOp.eval(env) > rOp.eval(env))
  }
}

case class Var(name: String) extends Expr {
  override def toString: String = name
  override def reduce(env: Map[String, Expr]) = env.get(name).orNull
}