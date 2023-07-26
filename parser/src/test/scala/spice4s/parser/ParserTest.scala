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
""",
"""
definition role {
	relation member: user | group#membership
	permission allowed = member
}

definition user {}

definition group {
	relation admin: user
	relation member: user
	permission membership = admin + member
}
""",
"""
definition platform {
	relation administrator: user
	permission super_admin = administrator
}

definition organization {
	relation platform: platform
	permission admin = platform->super_admin
}

definition resource {
	relation owner: user | organization
	permission admin = owner + owner->admin
}

definition user {}
""",
"""
definition user {}

definition portfolio {
	relation reader: user
	permission read = reader
}

definition folder {
	relation parent_portfolio: portfolio
	relation reader: user
	permission read = reader + parent_portfolio->read
}

definition document {
	relation parent_folder: folder
	relation reader: user

	/** read defines whether a user can read the document */
	permission read = reader + parent_folder->read
}
""",
"""
definition folder {
	relation parent: folder
	relation reader: user

	// Note that since `parent` refers to `folder` (which is this type), `parent->read` will call
	// this same permission, therefore following `read` for *that folder's* parent.
	permission read = reader + parent->read
}

definition user {}
"""
  )
}
