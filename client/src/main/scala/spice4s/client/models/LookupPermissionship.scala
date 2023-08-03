/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

// final case class PermissionRelationshipTree(
//     treeType: PermissionRelationshipTree.TreeType,
//     expandedObject: ObjectReference,
//     expandedRelation: String
// )

// object PermissionRelationshipTree {
//   sealed trait TreeType
//   object TreeType {
//     final case class Intermediate(value: AlgebraicSubjectSet) extends TreeType
//     final case class Leaf(value: DirectSubjectSet) extends TreeType

//     def decode(x: core.PermissionRelationshipTree.TreeType): Decoded[TreeType] = {
//       import core.PermissionRelationshipTree.{TreeType => T}
//       x match {
//         case T.Intermediate(value) => AlgebraicSubjectSet.decode(value).map(Intermediate(_))
//         case T.Leaf(value)         => DirectSubjectSet.decode(value).map(Leaf(_))
//         case T.Empty               => raise("tree type was invalid")
//       }
//     }
//   }

//   def decode(x: core.PermissionRelationshipTree): Decoded[PermissionRelationshipTree] =
//     (
//       field("tree_type")(TreeType.decode(x.treeType)),
//       field("expanded_object")(req(x.expandedObject) andThen ObjectReference.decode),
//       x.expandedRelation.validNec
//     ).mapN(PermissionRelationshipTree.apply)
// }

// final case class LookupResourcesResponse(
//   lookedUpAt: ZedToken,
//   resourceObjectId: String,
//   per

sealed trait LookupPermissionship

object LookupPermissionship {
  case object HasPermission extends LookupPermissionship
  case object ConditionalPermission extends LookupPermissionship

  def decodeOpt(x: ps.LookupPermissionship): Decoded[Option[LookupPermissionship]] = {
    import ps.{LookupPermissionship => PS}
    x match {
      case PS.LOOKUP_PERMISSIONSHIP_HAS_PERMISSION         => HasPermission.some.validNec
      case PS.LOOKUP_PERMISSIONSHIP_CONDITIONAL_PERMISSION => ConditionalPermission.some.validNec
      case PS.LOOKUP_PERMISSIONSHIP_UNSPECIFIED            => none.validNec
      case PS.Unrecognized(_)                              => raise("lookup permissionship was invalid")
    }
  }

  def decode(x: ps.LookupPermissionship): Decoded[LookupPermissionship] = decodeOpt(x) andThen req
}
