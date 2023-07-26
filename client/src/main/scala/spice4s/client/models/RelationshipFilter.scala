package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class RelationshipFilter(
    resourceType: Type,
    resourceId: Option[Id],
    relation: Option[Relation],
    subjectFilter: Option[SubjectFilter]
) {
  def encode = ps.RelationshipFilter.of(
    resourceType.value,
    resourceId.foldMap(_.value),
    relation.foldMap(_.value),
    subjectFilter.map(_.encode)
  )
}
