package AAQZ

sealed trait ExprC


// case class Add(left: Expr, right: Expr) extends Expr
// case class Sub(left: Expr, right: Expr) extends Expr
// case class Mul(left: Expr, right: Expr) extends Expr
// case class Div(left: Expr, right: Expr) extends Expr

// case classes for all ExprC types
final case class NumC(n : Double) extends ExprC
final case class StrC(s : String) extends ExprC
final case class IdC(s : Symbol) extends ExprC
final case class AppC(fundef : ExprC, args : Seq[ExprC]) extends ExprC
final case class LamC(args : Seq[Symbol], body : ExprC) extends ExprC
final case class IfC(ifCond : ExprC, ifThen : ExprC, ifElse : ExprC) extends ExprC