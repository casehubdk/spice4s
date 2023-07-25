package spice4s.client.models

import spice4s.client.util._

final case class SubjectFilter(
  subjectType: Type,
  subjectId: Option[Id],
  relationFilter: Option[Relation]
)
