package AAQZ

import AAQZ.Value

final case class Binding(id : Symbol, value : Value)

// define env Class
class Env(private val bindings: List[Binding] = List()) {
  // add new binding to bindings
  def extendEnv(binding : Binding) : Env = new Env(binding :: bindings) 
  // def lookup
}