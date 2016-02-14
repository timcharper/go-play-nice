package goparser

trait ParseHelpers {
  import fastparse.all._
  def doParse[T](p: Parser[T], s: String): T = {
    (p ~ End).parse(s) match {
      case Parsed.Success(m, _) =>
        m
      case f: Parsed.Failure =>
        throw new ParseError(f)
    }
  }
  val tpe = doParse(GoExpr.tpe, _: String)
}
