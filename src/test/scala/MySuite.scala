// For more information on writing tests, see
import AAQZ.Env
import AAQZ.Binding
import AAQZ.PrimV
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("Env Lookup") {
    val testEnv = new Env()
    assertEquals(testEnv.lookup(Symbol("-")), PrimV(Symbol("-")))
  }
}
