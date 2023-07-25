package spice4s.client.models

import cats.implicits._
import cats.data._
import com.authzed.api.v1.{permission_service => ps}
import scalapb.validate._
import spice4s.client.util._

final case class SubjectFilter(
  subjectType: Type,
  subjectId: Option[Id],
  relationFilter: Option[Relation]
) {
  def encode = ps.SubjectFilter.of(
    subjectType.value,
    subjectId.foldMap(_.value),
    relationFilter.map(f => ps.SubjectFilter.RelationFilter.of(f.value))
  )
}
