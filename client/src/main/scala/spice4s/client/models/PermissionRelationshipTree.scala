package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.core

final case class PermissionRelationshipTree(
    treeType: PermissionRelationshipTree.TreeType,
    expandedObject: ObjectReference,
    expandedRelation: String
)

object PermissionRelationshipTree {
  sealed trait TreeType
  object TreeType {
    final case class Intermediate(value: AlgebraicSubjectSet) extends TreeType
    final case class Leaf(value: DirectSubjectSet) extends TreeType

    def decode(x: core.PermissionRelationshipTree.TreeType): Decoded[TreeType] = {
      import core.PermissionRelationshipTree.{TreeType => T}
      x match {
        case T.Intermediate(value) => AlgebraicSubjectSet.decode(value).map(Intermediate(_))
        case T.Leaf(value)         => DirectSubjectSet.decode(value).map(Leaf(_))
        case T.Empty               => raise("tree type was invalid")
      }
    }
  }

  def decode(x: core.PermissionRelationshipTree): Decoded[PermissionRelationshipTree] =
    (
      field("tree_type")(TreeType.decode(x.treeType)),
      field("expanded_object")(req(x.expandedObject) andThen ObjectReference.decode),
      x.expandedRelation.validNec
    ).mapN(PermissionRelationshipTree.apply)
}