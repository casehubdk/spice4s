package spice4s.client.models

import cats.data._
import scalapb.validate._
import spice4s.client.util._
import com.authzed.api.v1.permission_service.SubjectFilterValidator

// final case class SubjectFilter(
//   subjectType: SubjectType,
//   subjectId: Option[SubjectId],
//   relationFilter: Option[SubjectRelationFilter]
// )
