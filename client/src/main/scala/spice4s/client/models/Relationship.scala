package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.core

final case class Relationship(
    resource: ObjectReference,
    relation: Relation,
    subject: SubjectReference
) {
  def encode = core.Relationship.of(
    resource.encode.some,
    relation.value,
    subject.encode.some,
    None
  )
}
