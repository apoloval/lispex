package lispex.parser

sealed trait Token

object Token {
  case object LeftParens extends Token
  case object RightParens extends Token
  case class Number(value: Int) extends Token
  case class Ident(value: String) extends Token
}
