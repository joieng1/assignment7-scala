// For more information on writing tests, see
import AAQZ.Env
import AAQZ.Binding
import AAQZ.PrimV
import AAQZ.Interpreter
import AAQZ.NumC
import AAQZ.NumV
import AAQZ.AppC
import AAQZ.IdC
import AAQZ.LamC
import AAQZ.IfC
// https://scalameta.org/munit/docs/getting-started.html
class InterpTest extends munit.FunSuite {
  test("Interp with addition") {
    val env = new Env()
    val expr = AppC(IdC(Symbol("+")), Seq(NumC(20), NumC(10)))
    val result = Interpreter.eval(expr, env)
    assertEquals(result, NumV(30))
  }
  test("Interp with division") {
    val env = new Env()
    val expr = AppC(IdC(Symbol("/")), Seq(NumC(20), NumC(10)))
    val result = Interpreter.eval(expr, env)
    assertEquals(result, NumV(2))
  }
  test("Interp with basic add 2 num function") {
    val env = new Env()
    val expr = AppC(
      LamC(
        Seq(Symbol("x"), Symbol("y")),
        AppC(IdC(Symbol("+")), Seq(IdC(Symbol("x")), IdC(Symbol("y"))))
      ),
      Seq(NumC(20), NumC(10))
    )
    val result = Interpreter.eval(expr, env)
    assertEquals(result, NumV(30))
  }

  test("test factorial function") {
    val expr = AppC(
      LamC(
        List(Symbol("fact")),
        AppC(IdC(Symbol("fact")), List(IdC(Symbol("fact")), NumC(3)))
      ),
      List(
        LamC(
          List(Symbol("self"), Symbol("n")),
          IfC(
            AppC(IdC(Symbol("<=")), List(IdC(Symbol("n")), NumC(0))),
            NumC(1),
            AppC(
              IdC(Symbol("*")),
              List(
                IdC(Symbol("n")),
                AppC(
                  IdC(Symbol("self")),
                  List(
                    IdC(Symbol("self")),
                    AppC(IdC(Symbol("-")), List(IdC(Symbol("n")), NumC(1)))
                  )
                )
              )
            )
          )
        )
      )
    )

    val env = new Env()
    val result = Interpreter.eval(expr, env)
    assert(result == NumV(6))
  }
}
