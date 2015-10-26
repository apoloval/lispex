package lispex.parser

trait Parser {
  def parse(input: Stream[Token]): Stream[Parser.Result[SExpr]]

  def parse(input: Token*): Stream[Parser.Result[SExpr]] = parse(input.toStream)
}

object DefaultParser extends Parser {

  override def parse(input: Stream[Token]): Stream[Parser.Result[SExpr]] = input.headOption match {
    case Some(Token.LeftParens) =>
      val (list, tail) = parseList(Seq.empty, input.tail)
      list #:: parse(tail)
    case Some(tk @ Token.RightParens) => Left(Parser.SyntaxError(tk)) #:: parse(input.tail)
    case Some(tk) => Right(parseAtomic(tk)) #:: parse(input.tail)
    case None => Stream.empty
  }

  private def parseList(accum: Seq[SExpr], input: Stream[Token]): (Parser.Result[SExpr.List], Stream[Token]) = {
    input.headOption match {
      case Some(Token.LeftParens) =>
        parseList(Seq.empty, input.tail) match {
          case (Right(list), tail) => parseList(accum :+ list, tail)
          case (Left(error), tail) => (Left(error), tail)
        }
      case Some(Token.RightParens) => (Right(SExpr.List(accum)), input.tail)
      case Some(tk) => parseList(accum :+ parseAtomic(tk), input.tail)
      case None => (Left(Parser.UnexpectedEof), Stream.empty)
    }
  }

  private def parseAtomic(input: Token): SExpr = input match {
    case Token.Number(n) => SExpr.Number(n)
    case Token.Ident(s) => SExpr.Ident(s)
  }

}

object Parser {
  sealed trait Error
  case object UnexpectedEof extends Error
  case class SyntaxError(token: Token) extends Error

  type Result[T] = Either[Error, T]
}
