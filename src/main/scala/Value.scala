sealed trait Value

// case classes for all Value types
final case class NumV(n : Double)
final case class BoolV(b : BoolV)
final case class StrV(s : String)
final case class CloV(params : List[Symbol], body : ExprC, env : Env)
final case class PrimV(op : Symbol)
final case class VoidV()


