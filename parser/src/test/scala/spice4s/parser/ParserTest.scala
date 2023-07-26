package spice4s.parser

import munit.FunSuite

class ParserTest extends FunSuite {
  schemas.zipWithIndex.foreach { case (schema, i) =>
    test(s"should be able to parse schema $i") {
      Parse.parseWith(SchemaParser.schema)(schema) match {
        case Left(err) => fail(err): Unit
        case Right(_)  => ()
      }
    }
  }

  lazy val schemas = List(
    """
/** user represents a user */
definition user {}

/** group represents a group **/
definition group {
    /** member is a member of a group, which can be a user or the membership of another group */
    relation member: user | group#member
}

/** document represents a document */
definition document {
    /** writer is a writer of the document */
    relation writer: user | group#member

    /** reader is a reader of the document */
    relation reader: user | group#member

    /** write indicates which user can write to the document */
    permission write = writer

    /** read indicates which user can read the document */
    permission read = reader + write
}
"""
  )
}
