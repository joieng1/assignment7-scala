package AAQZ
import AAQZ.Value
import AAQZ.Env
import AAQZ.ExprC
import scala.collection.MapView.Id
//import org.scalatest.funsuite.AnyFunSuite

// final case class NumC(n : Double) extends ExprC
// final case class StrC(s : String) extends ExprC
// final case class IdC(s : Symbol) extends ExprC
// final case class AppC(fundef : ExprC, args : List[ExprC]) extends ExprC
// final case class LamC(arg : List[Symbol], body : ExprC) extends ExprC
// final case class IfC(ifCond : ExprC, ifThen : ExprC, ifElse : ExprC) extends ExprC

object Interpreter {
  def eval(expr: ExprC, env: Env): Value = expr match {
    case NumC(n) => NumV(n)
    
    case StrC(s) => StrV(s)

    case LamC(args, body) => CloV(args, body, env)

    case IfC(ifCond, ifThen, ifElse) => interp_if(ifCond, ifThen, ifElse, env)
    
    case IdC(s) => env.lookup(s)

    case AppC(fundef, args) => {
      val f_value = eval(fundef, env)
      f_value match {
        case PrimV(op) => {
          // binop should be performed on two arguments 
          if (args.length == 2) {
            if (op == Symbol("equal?")) {
              interp_equal(op, args(0), args(1), env)
            }
            else {
              interp_binop(op, args(0), args(1), env)
            }
          }
          // unless raising error, in which case only one 
          else if (args.length == 1) {
            if (op == Symbol("error")) {
              /** TODO: include serialize? */
              throw new Exception("AAQZ user-error")
            }
            else {
              throw new Exception("AAQZ invalid binop")
            }
          }
          else {
            throw new Exception("AAQZ invalid argument length for binop")
          }
        }
      }
    }
  }
   
  /** interp helper function in case of if. 
  consumes three ExprC (ifCond, ifThen, ifElse) and Env
  if interpreting ifCond results in true BoolV, return ifThen. if false BoolV, return ifElse
  if interpreting ifCond does not result in BoolV, raise an erro*/
  def interp_if(ifCond: ExprC, ifThen: ExprC, ifElse: ExprC, env: Env): Value = {
    val ifCondVal = eval(ifCond, env)
    if (ifCondVal == BoolV(true)) {
      eval(ifThen, env)
    }
    else if (ifCondVal == BoolV(false)) {
      eval(ifElse, env)
    }
    else {
      throw new Exception("AAQZ: if-clause does not produce a BoolV")
    }
  }

  /** TODO: what are the errors here? */
  def interp_equal(op: Symbol, left: ExprC, right: ExprC, env: Env): Value = {
    val l = eval(left, env)
    val r = eval(right, env)
    if (l.isInstanceOf[CloV] || r.isInstanceOf[CloV]) {
      throw new Exception("AAQZ")
    } 
    else if (l.isInstanceOf[PrimV] || r.isInstanceOf[PrimV]) {
      throw new Exception("AAQZ")
    }
    else if (l == r) {
      BoolV(true)
    }
    else {
      BoolV(false)
    }
  }

  // build this function and what not ...
  def interp_binop(op: Symbol, left: ExprC, right: ExprC, env: Env): Value = {
    BoolV(true)
  }

  // have to provide expr, env so that this runs ..
  // def run(expr: ExprC): Double = eval(expr, env)
}

// TODO: what this doing?
object Main extends App {
  import Interpreter._

}