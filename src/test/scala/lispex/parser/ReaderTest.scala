package lispex.parser

import org.scalatest.{ShouldMatchers, FlatSpec}

class ReaderTest extends FlatSpec with ShouldMatchers {

  "Reader" should "read from input" in {
    val input = "(+ 123 abc)"
    Reader(input.toStream) { _ => }.toSeq shouldBe Seq(SExpr.List.from(
      SExpr.Ident("+"),
      SExpr.Number(123),
      SExpr.Ident("abc")
    ))
  }
}
