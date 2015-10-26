package lispex.parser

sealed trait SExpr

object SExpr {

  case class List(values: Seq[SExpr]) extends SExpr {
    def :+ (other: SExpr): List = List(values :+ other)

  }

  object List {
    def from(elems: SExpr*): List = List(elems.toSeq)
    def empty: List = List(Seq.empty)
  }

  case class Number(value: Int) extends SExpr
  case class Ident(value: String) extends SExpr
}
