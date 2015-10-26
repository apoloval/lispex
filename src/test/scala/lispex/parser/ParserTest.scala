package lispex.parser

import org.scalatest.{ShouldMatchers, FlatSpec}

class ParserTest extends FlatSpec with ShouldMatchers {

  "Parser" should "parse atomics" in {
    DefaultParser.parse(Token.Number(42)) shouldBe Stream(Right(SExpr.Number(42)))
    DefaultParser.parse(Token.Ident("foobar")) shouldBe Stream(Right(SExpr.Ident("foobar")))
  }

  it should "parse lists" in {
    val prog = Stream(Token.LeftParens, Token.Number(123), Token.Number(456), Token.RightParens)
    DefaultParser.parse(prog) shouldBe Stream(Right(SExpr.List.from(SExpr.Number(123), SExpr.Number(456))))
  }

  it should "parse nested lists" in {
    val prog = Stream(
      Token.LeftParens,
        Token.Number(123),
        Token.LeftParens, Token.Number(456), Token.Number(789), Token.RightParens,
      Token.RightParens)
    DefaultParser.parse(prog) shouldBe Stream(Right(
      SExpr.List.from(
        SExpr.Number(123),
        SExpr.List.from(SExpr.Number(456), SExpr.Number(789)))))
  }

  it should "fail to parse unbalanced parens" in {
    DefaultParser.parse(Token.RightParens) shouldBe Stream(Left(Parser.SyntaxError(Token.RightParens)))
    DefaultParser.parse(Token.LeftParens, Token.RightParens, Token.RightParens) shouldBe Stream(
      Right(SExpr.List.empty), Left(Parser.SyntaxError(Token.RightParens)))
  }
}
