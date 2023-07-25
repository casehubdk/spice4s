package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class LookupSubjectsRequest(
    consistency: Option[Consistency],
    objectReference: ObjectReference,
    permission: Relation,
    subjectObjectType: Type,
    subjectRelation: Option[Relation],
    limit: Option[Limit],
    cursor: Option[Cursor],
    wildcardOption: Option[LookupSubjectsRequest.WildcardOption]
) {
  def encode = ps.LookupSubjectsRequest.of(
    consistency.map(_.encode),
    objectReference.encode.some,
    permission.value,
    subjectObjectType.value,
    subjectRelation.foldMap(_.value),
    None,
    limit.foldMap(_.value),
    cursor.map(_.encode),
    wildcardOption.map(_.encode).getOrElse(ps.LookupSubjectsRequest.WildcardOption.WILDCARD_OPTION_UNSPECIFIED)
  )
}

object LookupSubjectsRequest {
  sealed trait WildcardOption {
    def encode: ps.LookupSubjectsRequest.WildcardOption
  }
  object WildcardOption {
    import ps.LookupSubjectsRequest.WildcardOption._
    case object Include extends WildcardOption {
      def encode = WILDCARD_OPTION_INCLUDE_WILDCARDS
    }
    case object Exclude extends WildcardOption {
      def encode = WILDCARD_OPTION_EXCLUDE_WILDCARDS
    }
  }
}
