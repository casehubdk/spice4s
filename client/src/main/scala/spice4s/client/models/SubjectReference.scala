package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.core

final case class SubjectReference(
    obj: ObjectReference,
    relation: Option[Relation]
) {
  def encode = core.SubjectReference.of(
    obj.encode.some,
    relation.foldMap(_.value)
  )
}

object SubjectReference {
  def decode(x: core.SubjectReference): Decoded[SubjectReference] =
    (
      field("object")(req(x.`object`) andThen ObjectReference.decode),
      field("relation")(Relation.decode(x.optionalRelation))
    ).mapN(SubjectReference.apply)
}
