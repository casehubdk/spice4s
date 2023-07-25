package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class Precondition(
    operation: Precondition.Operation,
    filter: RelationshipFilter
) {
  def encode = ps.Precondition.of(
    operation.encode,
    filter.encode.some
  )
}

object Precondition {
  sealed trait Operation {
    def encode: ps.Precondition.Operation
  }
  object Operation {
    import ps.Precondition.Operation._
    case object MustNotMatch extends Operation {
      def encode = OPERATION_MUST_NOT_MATCH
    }
    case object MustMatch extends Operation {
      def encode = OPERATION_MUST_MATCH
    }
  }
}
