package spice4s.client.models

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
