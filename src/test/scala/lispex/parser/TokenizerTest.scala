package lispex.parser

import org.scalatest.{FlatSpec, ShouldMatchers}

class TokenizerTest extends FlatSpec with ShouldMatchers {

  "Tokenizer" should "scan left parens" in {
    DefaultTokenizer.tokenize("(").toSeq shouldBe Seq(Token.LeftParens)
    DefaultTokenizer.tokenize("  \n \t (").toSeq shouldBe Seq(Token.LeftParens)
    DefaultTokenizer.tokenize("( \n \t ").toSeq shouldBe Seq(Token.LeftParens)
  }

  it should "scan right parens" in {
    DefaultTokenizer.tokenize(")").toSeq shouldBe Seq(Token.RightParens)
    DefaultTokenizer.tokenize("  \n \t )").toSeq shouldBe Seq(Token.RightParens)
    DefaultTokenizer.tokenize(") \n \t ").toSeq shouldBe Seq(Token.RightParens)
  }

  it should "scan identifiers" in {
    DefaultTokenizer.tokenize("foo").toSeq shouldBe Seq(Token.Ident("foo"))
    DefaultTokenizer.tokenize(" \n \t foo").toSeq shouldBe Seq(Token.Ident("foo"))
    DefaultTokenizer.tokenize("foo \n \t ").toSeq shouldBe Seq(Token.Ident("foo"))
    DefaultTokenizer.tokenize("+").toSeq shouldBe Seq(Token.Ident("+"))
  }

  it should "scan numbers" in {
    DefaultTokenizer.tokenize("123").toSeq shouldBe Seq(Token.Number(123))
    DefaultTokenizer.tokenize(" \n \t 123").toSeq shouldBe Seq(Token.Number(123))
    DefaultTokenizer.tokenize("123 \n \t ").toSeq shouldBe Seq(Token.Number(123))
  }

  it should "scan some text" in {
    DefaultTokenizer.tokenize(
      """(
        | add
        | 123   456
        |)
      """.stripMargin) shouldBe Seq(
      Token.LeftParens,
      Token.Ident("add"),
      Token.Number(123),
      Token.Number(456),
      Token.RightParens
    )
  }
}
