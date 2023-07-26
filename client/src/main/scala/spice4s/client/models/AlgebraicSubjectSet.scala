package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.core

final case class AlgebraicSubjectSet(
    operation: AlgebraicSubjectSet.Operation,
    children: List[PermissionRelationshipTree]
)

object AlgebraicSubjectSet {
  sealed trait Operation
  object Operation {
    case object Union extends Operation
    case object Intersection extends Operation
    case object Exclusion extends Operation

    def decode(x: core.AlgebraicSubjectSet.Operation): Decoded[Operation] = {
      import core.AlgebraicSubjectSet.Operation._
      x match {
        case OPERATION_UNION                         => Union.validNec
        case OPERATION_INTERSECTION                  => Intersection.validNec
        case OPERATION_EXCLUSION                     => Exclusion.validNec
        case OPERATION_UNSPECIFIED | Unrecognized(_) => raise("operation was invalid")
      }
    }
  }

  def decode(x: core.AlgebraicSubjectSet): Decoded[AlgebraicSubjectSet] =
    (
      field("operation")(Operation.decode(x.operation)),
      field("children") {
        indexed {
          x.children.toList.map(PermissionRelationshipTree.decode)
        }
      }
    ).mapN(AlgebraicSubjectSet.apply)
}
