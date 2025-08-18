package org.planx.sh.parsing.shop2

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

trait SHOP2Tokens extends StdTokens {
  // Adapted from StdTokens
  case class IdLit(chars: String) extends Token {
    override def toString = chars
  }

  case class VarIdLit(chars: String) extends Token {
    override def toString = chars
  }

  case class AnonymousVarLit(chars: String) extends Token {
    override def toString = chars
  }

  case class FloatLit(chars: String) extends Token {
    override def toString = chars
  }

  case class IntLit(chars: String) extends Token {
    override def toString = chars
  }

  case class TaskLit(chars: String) extends Token {
    override def toString = chars
  }
}

class SHOP2Lexer extends StdLexical with SHOP2Tokens {


  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => Success(source.subSequence(offset, offset + matched.end).toString, in.drop(matched.end))
        case None => Failure("String matching regex `" + r + "' expected, but `" + in.first + "' found", in.drop(0))
      }
    }
  }

  reserved ++= List(
    "and",
    "imply",
    "not",
    "or",
    "forall",
    "defdomain",
    "defproblem",
    "def-problem-set",
    "eval",
    "call",
    "assign",
    "enforce",
    "setof"
  )

  delimiters ++= List(
    "(", ")", "{", "}", "[", "]",
    ":unordered", ":sort-by", ":first", ":method", ":operator", ":-", ":task", ":ordered", "immediate", ":protection", ":include"
  )

  def process(name: String): Token = if (reserved contains name) Keyword(name) else IdLit(name)

  override def token: Parser[Token] = (
    regex("""\?_[a-zA-Z][a-zA-Z0-9\-]*""".r) ^^ AnonymousVarLit // Anonymous variables: ?_variable
      | regex("""\?_""".r) ^^ AnonymousVarLit // Simple anonymous: ?_
      | regex("""\?[a-zA-Z][a-zA-Z0-9\-]*""".r) ^^ VarIdLit // Regular variables: ?variable
      | regex("""![a-zA-Z][a-zA-Z0-9\-]*""".r) ^^ TaskLit // Primitive tasks: !task
      | regex("""[a-zA-Z_][a-zA-Z0-9\-]*""".r) ^^ process // Constants/Functions/Predicates/Compound tasks
      | regex("""[+-]?\d+\.\d+""".r) ^^ FloatLit // Float numbers
      | regex("""[+-]?\d+""".r) ^^ IntLit // Integer numbers
      | super.token
      | EofCh ^^^ EOF
    )

  override def whitespace: Parser[Any] =
    rep(whitespaceChar | comment)

  override protected def comment: Parser[Any] =
    ';' ~> rep(chrExcept(EofCh, '\n', '\r')) ^^ (_ => ())
}
