package AAQZ

import AAQZ.Value

final case class Binding(id: Symbol, value: Value)

// define env Class
class Env(
    private val bindings: List[Binding] = List(
      Binding(Symbol("+"), new PrimV(Symbol("+"))),
      Binding(Symbol("-"), new PrimV(Symbol("-"))),
      Binding(Symbol("*"), new PrimV(Symbol("*"))),
      Binding(Symbol("/"), new PrimV(Symbol("/"))),
      Binding(Symbol("true"), new BoolV(true)),
      Binding(Symbol("false"), new BoolV(false)),
      Binding(Symbol("<="), new PrimV(Symbol("<="))),
      Binding(Symbol("equal"), new PrimV(Symbol("equal"))),
      Binding(Symbol("error"), new PrimV(Symbol("error")))
    )
) {

  // add new binding to bindings
  def extendEnv(binding: Binding): Env = new Env(binding :: bindings)

// def lookup
  def lookup(targetId: Symbol): Value = {
    bindings.find(binding => binding.id == targetId) match {
      case Some(binding) => binding.value
      case None =>
        throw new NoSuchElementException(
          s"Binding with id $targetId cannot be found"
        )
    }
  }
}
