package spice4s.client.models

import spice4s.client.util._
import cats.implicits._
import com.authzed.api.v1.{permission_service => ps}

final case class ExpandPermissionTreeResponse(
    expandedAt: ZedToken,
    treeRoot: PermissionRelationshipTree
)

object ExpandPermissionTreeResponse {
  def decode(x: ps.ExpandPermissionTreeResponse): Decoded[ExpandPermissionTreeResponse] =
    (
      field("expanded_at")(req(x.expandedAt) andThen ZedToken.decode andThen req),
      field("tree_root")(req(x.treeRoot) andThen PermissionRelationshipTree.decode)
    ).mapN(ExpandPermissionTreeResponse.apply)
}
