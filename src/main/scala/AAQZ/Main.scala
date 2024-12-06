package AAQZ
import AAQZ.Value
import AAQZ.Env
import AAQZ.ExprC
import scala.collection.MapView.Id
//import org.scalatest.funsuite.AnyFunSuite

object Interpreter {
  def eval(expr: ExprC, env: Env): Value = expr match {
    case NumC(n)                     => NumV(n)
    case StrC(s)                     => StrV(s)
    case LamC(args, body)            => CloV(args, body, env)
    case IfC(ifCond, ifThen, ifElse) => interp_if(ifCond, ifThen, ifElse, env)
    case IdC(s)                      => env.lookup(s)
    case AppC(fundef, args) => {
      val f_value = eval(fundef, env)
      f_value match {
        case PrimV(op) => {
          // binop should be performed on two arguments
          if (args.length == 2) {
            if (op == Symbol("equal?")) {
              interp_equal(op, args.head, args.tail.head, env)
            } else {
              interp_binop(op, args.head, args.tail.head, env)
            }
          }
          // unless raising error, in which case only one
          else if (args.length == 1) {
            if (op == Symbol("error")) {

              /** TODO: include serialize? */
              throw new Exception("AAQZ user-error")
            } else {
              throw new Exception("AAQZ invalid binop")
            }
          } else {
            throw new Exception("AAQZ invalid argument length for binop")
          }
        }
        case CloV(params, body, closEnv) => {
          val vals = args.map(arg => eval(arg, env))
          if (vals.length == params.length) {
            // zip creates tuples (param, val), map is used to convert tuples to bindings
            val newBindings = params
              .zip(vals)
              .map { case (param, value) =>
                Binding(param, value)
              }
              .toList
            eval(body, closEnv.extendEnv(newBindings))
          } else {
            throw new Exception(
              "AAQZ CloV must have same number of arguments and symbols"
            )
          }
        }
        case _ => {
          throw new Exception("AAQZ attempted to apply a non-function value")
        }
      }
    }
  }

  /** interp helper function in case of if. consumes three ExprC (ifCond,
    * ifThen, ifElse) and Env if interpreting ifCond results in true BoolV,
    * return ifThen. if false BoolV, return ifElse if interpreting ifCond does
    * not result in BoolV, raise an erro
    */
  def interp_if(
      ifCond: ExprC,
      ifThen: ExprC,
      ifElse: ExprC,
      env: Env
  ): Value = {
    val ifCondVal = eval(ifCond, env)
    if (ifCondVal == BoolV(true)) {
      eval(ifThen, env)
    } else if (ifCondVal == BoolV(false)) {
      eval(ifElse, env)
    } else {
      throw new Exception("AAQZ: if-clause does not produce a BoolV")
    }
  }

  /** TODO: what are the errors here? */
  def interp_equal(op: Symbol, left: ExprC, right: ExprC, env: Env): Value = {
    val l = eval(left, env)
    val r = eval(right, env)
    if (l.isInstanceOf[CloV] || r.isInstanceOf[CloV]) {
      BoolV(false)
    } else if (l.isInstanceOf[PrimV] || r.isInstanceOf[PrimV]) {
      BoolV(false)
    } else if (l == r) {
      BoolV(true)
    } else {
      BoolV(false)
    }
  }

  /** interp helper function in case of binary operation consumes operand, left
    * and right operators, and environment returns outcome of Operation as a
    * Value
    */
  def interp_binop(op: Symbol, left: ExprC, right: ExprC, env: Env): Value = {
    val l = eval(left, env)
    val r = eval(right, env)
    if (op == Symbol("error")) {
      throw new Exception("AAQZ cannot perform 'error on two arguments")
    } else {
      (l, r) match {
        case (NumV(n1), NumV(n2)) => {
          op match {
            case Symbol("+") => NumV(n1 + n2)
            case Symbol("-") => NumV(n1 - n2)
            case Symbol("*") => NumV(n1 * n2)
            case Symbol("/") => {
              if (n2 == 0) {
                throw new Exception("AAQZ cannot divide by 0")
              } else {
                NumV(n1 / n2)
              }
            }
            case Symbol("<=") => {
              if (n1 <= n2) {
                BoolV(true)
              } else {
                BoolV(false)
              }
            }
            case _ => throw new Exception("AAQZ binop not supported")
          }
        }
        case _ =>
          throw new Exception("AAQZ cannot perform binop on non-numbers")

      }

    }
  }
}

// not needed because we will not be running main
// object Main extends App {
//   import Interpreter._
// }
