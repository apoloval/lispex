package lispex.parser

trait Tokenizer {
  def tokenize(input: Stream[Char]): Stream[Token]

  def tokenize(input: String): Stream[Token] = tokenize(input.toStream)
}

object DefaultTokenizer extends Tokenizer {

  override def tokenize(input: Stream[Char]): Stream[Token] = {
    input.headOption match {
      case Some(c) if c.isWhitespace => tokenize(input.tail)
      case Some(c) if c.isDigit => tokenizeNumber(c.toString, input.tail)
      case Some('(') => Token.LeftParens #:: tokenize(input.tail)
      case Some(')') => Token.RightParens #:: tokenize(input.tail)
      case Some(c) => tokenizeIdent(c.toString, input.tail)
      case None => Stream.empty
    }
  }

  private def tokenizeIdent(accum: String, input: Stream[Char]): Stream[Token] = {
    input.headOption match {
      case Some(c) if c.isWhitespace => Token.Ident(accum) #:: tokenize(input)
      case Some('(') | Some(')') => Token.Ident(accum) #:: tokenize(input)
      case Some(c) => tokenizeIdent(accum + c, input.tail)
      case None => Token.Ident(accum) #:: Stream.empty
    }
  }

  private def tokenizeNumber(accum: String, input: Stream[Char]): Stream[Token] = {
    input.headOption match {
      case Some(c) if c.isDigit => tokenizeNumber(accum + c, input.tail)
      case Some(c) => Token.Number(accum.toInt) #:: tokenize(input)
      case None => Token.Number(accum.toInt) #:: Stream.empty
    }
  }
}
