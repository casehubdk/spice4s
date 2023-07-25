package spice4s.client.models

import cats.implicits._
import cats.data._
import com.authzed.api.v1.{permission_service => ps}
import scalapb.validate._
import spice4s.client.util._

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
