package com.ucu.pfp.tinylang_old

trait Statement {
  def isReducible: Boolean = true
  def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr])
}
case class DoNothing() extends Statement {
  override def toString: String = "DoNothing"
  override def isReducible: Boolean = false
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = (this, env)
}
case class Assign(v: Var, expression: Expr) extends Statement {
  override def toString: String = s"${v} = ${expression}"
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = expression match {
    case e: Expr if !(e.isReducible) => (DoNothing(), env + (v.name -> e))
    case e: Expr => (Assign(v, e.reduce(env)), env)
  }
}
case class Exp(expression: Expr) extends Statement {
  override def toString: String = s"${expression}"
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = expression match {
    case e: Expr if !(e.isReducible) => (Exp(e), env)
    case e: Expr => (Exp(e.reduce(env)), env)
  }
  override def isReducible: Boolean = expression.isReducible
}
case class Seq(statements: List[Statement]) extends Statement {
  override def toString: String = statements.mkString("[\n", "\n", "\n]")
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = statements match {
    case Nil => (DoNothing(), env)
    case x :: Nil if x.isReducible => {
      val next = x.reduce(env)
      (Seq(next._1 :: Nil), next._2)
    }
    case x :: Nil => (DoNothing(), env)
    case x :: xs if x.isReducible => {
      val next = x.reduce(env)
      (Seq(next._1 :: xs), next._2)
    }
    case _ :: xs => (Seq(xs), env)
  }
  override def isReducible: Boolean = if (statements.size == 0) false else true
}
case class IfElse(condition: Expr, consequence: Statement, alternative: Statement) extends Statement {
  override def toString: String = s"if(${condition}) { ${consequence} } else { ${alternative} }"
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = condition.reduced(env) match {
    case TRUE() => (consequence, env)
    case FALSE() => (alternative, env)
    case _ => (null, null)
  }
}
case class WhileLoop(condition: Expr, body: Statement) extends Statement {
  override def toString: String = s"while (${condition}) { ${body} }"
  override def reduce(env: Map[String, Expr]): (Statement, Map[String, Expr]) = condition.reduced(env) match {
    case FALSE() => (DoNothing(), env)
    case TRUE() => {
      val next = body.reduce(env)
      (Seq(List(
        next._1,
        WhileLoop(condition, body)
      )), next._2)
    }
  }
}