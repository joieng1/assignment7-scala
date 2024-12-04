package AAQZ
sealed trait Expr
case class Num(value: Double) extends Expr
case class Var(name: String) extends Expr
case class Add(left: Expr, right: Expr) extends Expr
case class Sub(left: Expr, right: Expr) extends Expr
case class Mul(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Funct(name: String, args: Expr, body: Expr) extends Expr



object Interpreter {

  type Env = Map[String, Double]

  def eval(expr: Expr, env: Env = Map.empty): Double = expr match {
    case Num(value) => value

    case Var(name) =>
      env.getOrElse(name, throw new RuntimeException(s"AAQZ Undefined variable: $name"))

    case Add(left, right) =>
      eval(left, env) + eval(right, env)

    case Sub(left, right) =>
      eval(left, env) - eval(right, env)

    case Mul(left, right) =>
      eval(left, env) * eval(right, env)

    case Div(left, right) =>
      val denominator = eval(right, env)
      if (denominator == 0) throw new ArithmeticException("Division by zero")
      eval(left, env) / denominator

    case Funct(name, value, body) =>
      val valueEvaluated = eval(value, env)
      eval(body, env + (name -> valueEvaluated))
  }

  def run(expr: Expr): Double = eval(expr)
}

object Main extends App {
  import Interpreter._

  val expr = Funct(
    "x",
    Num(10),
    Add(
      Var("x"),
      Funct("y", Num(20), Mul(Var("x"), Var("y")))
    )
  )

  println(run(expr))
}
