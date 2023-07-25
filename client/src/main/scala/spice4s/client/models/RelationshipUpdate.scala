package spice4s.client.models

import cats.implicits._
import com.authzed.api.v1.core

final case class RelationshipUpdate(
    operation: RelationshipUpdate.Operation,
    relationship: Relationship
) {
  def encode = core.RelationshipUpdate.of(
    operation.encode,
    relationship.encode.some
  )
}

object RelationshipUpdate {
  sealed trait Operation {
    def encode: core.RelationshipUpdate.Operation
  }
  object Operation {
    import core.RelationshipUpdate.Operation._
    case object Create extends Operation {
      def encode = OPERATION_CREATE
    }
    case object Touch extends Operation {
      def encode = OPERATION_TOUCH
    }
    case object Delete extends Operation {
      def encode = OPERATION_DELETE
    }
  }
}
