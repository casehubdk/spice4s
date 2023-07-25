package spice4s.client.models

import spice4s.client.util._
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

object Relationship {
  def decode(x: core.Relationship): Decoded[Relationship] =
    (
      field("resource")(req(x.resource) andThen ObjectReference.decode),
      field("relation")(Relation.decode(x.relation) andThen req),
      field("subject")(req(x.subject) andThen SubjectReference.decode)
    ).mapN(Relationship.apply)
}
