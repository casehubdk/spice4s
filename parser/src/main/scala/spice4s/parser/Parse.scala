package spice4s.parser

import cats.parse._
import cats.implicits._

object Parse {
  def parseWith[A](parser: Parser0[A])(data: String): Either[String, A] =
    parser.parseAll(data).leftMap(x => ParserUtil.errorMessage(data, x))
}
