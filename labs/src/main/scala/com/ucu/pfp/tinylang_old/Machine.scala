package com.ucu.pfp.tinylang_old

case class Machine(statement: Statement, env: Map[String, Expr]) {

  def reductionStep: Machine = {
    val next: (Statement, Map[String, Expr]) = statement.reduce(env)
    Machine(next._1, next._2)
  }

  override def toString: String = s"Machine('${statement.toString}', '${env}')"

  def run: List[(Statement, Map[String, Expr])] = {
    def go(machine: Machine): List[(Statement, Map[String, Expr])] = machine match {
      case Machine(s, e) if !(s.isReducible)  => (s, e) :: Nil
      case _ => {
        val nextMachine: Machine = machine.reductionStep
        (machine.statement, machine.env) :: go(Machine(nextMachine.statement, nextMachine.env))
      }
    }
    go(this)
  }

}