package AAQZ

import AAQZ.Value

final case class Binding(id : Symbol, value : Value)

// define env Class
class Env(private val bindings: List[Binding] = 
  List(Binding('+, new PrimV(Symbol("+"))),
       Binding('-, new PrimV(Symbol("-"))),
       Binding('*, new PrimV(Symbol("*"))),
       Binding('/, new PrimV(Symbol("/"))),
       Binding('true, new BoolV(true)),
       Binding('false, new BoolV(false)),
       Binding('<=, new PrimV(Symbol("<="))),
       Binding('equal, new PrimV(Symbol("equal"))),
       Binding('error, new PrimV(Symbol("error")))
       )) {
    
  // add new binding to bindings
  def extendEnv(binding : Binding) : Env = new Env(binding :: bindings)

// def lookup
  def lookup(targetId : Symbol) : Value = {
    bindings.find(binding => binding.id == targetId) match {
      case Some(binding) => binding.value
      case None => throw new NoSuchElementException(s"Binding with id $targetId cannot be found")
    }
  }
}