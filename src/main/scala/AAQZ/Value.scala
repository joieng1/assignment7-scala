package AAQZ
sealed trait Value

// case classes for all Value types
final case class NumV(n: Double) extends Value
final case class BoolV(b: Boolean) extends Value
final case class StrV(s: String) extends Value
final case class CloV(params: Seq[Symbol], body: ExprC, env: Env) extends Value
final case class PrimV(op: Symbol) extends Value
