import org.scalatest.funsuite.AnyFunSuite
import AAQZ.ExprC
import AAQZ.Value
import AAQZ.Env

// class CubeCalculatorTest extends AnyFunSuite:
//    test("CubeCalculator.cube") {
//      assert(CubeCalculator.cube(3) === 27)
//    }

class EnvTest extends AnyFunSuite {
  test("Env Lookup") {
    val testEnv = new Env()
    testEnv.extendEnv(Binding(Symbol("+"), PrimV(Symbol("+"))))
    assert(testEnv.lookup(Symbol("+")) === (PrimV Symbol("+")))
  }
}