package AAQZ
sealed trait Value

// case classes for all Value types
final case class NumV(n : Double) extends Value
final case class BoolV(b : BoolV) extends Value
final case class StrV(s : String) extends Value
final case class CloV(params : List[Symbol], body : ExprC, env : Env) extends Value
final case class PrimV(op : Symbol) extends Value
final case class VoidV() extends Value


