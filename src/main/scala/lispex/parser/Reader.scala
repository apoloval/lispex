package lispex.parser

import scala.annotation.tailrec

class Reader(input: Stream[Char], onError: Parser.Error => Unit) extends Iterator[SExpr] {

  var expressions: Stream[Parser.Result[SExpr]] = DefaultParser.parse(DefaultTokenizer.tokenize(input))

  override def hasNext: Boolean = {
    skipErrors()
    expressions.nonEmpty
  }

  override def next(): SExpr = {
    val expr = expressions.head.right.get
    expressions = expressions.tail
    expr
  }

  @tailrec
  private def skipErrors(): Unit = {
    expressions.headOption match {
      case Some(Left(error)) =>
        onError(error)
        expressions = expressions.tail
        skipErrors()
      case _ => ()
    }
  }
}

object Reader {

  def apply(input: Stream[Char])(onError: Parser.Error => Unit): Reader = new Reader(input, onError)
}
