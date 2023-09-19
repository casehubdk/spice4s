package spice4s.generator.core

import spice4s.client.models._

trait Spice4sResource {
  def constants: Spice4sResourceConstants[?]
  def id: Id
  def ref: ObjectReference = ObjectReference(
    constants.objectType,
    id
  )
}

trait Spice4sResourceConstants[A <: Spice4sResource] {
  def objectType: Type
}

trait Spice4sRelation {
  def relation: Relation
}

final case class PermissionRequest[Res <: Spice4sResource, Rel <: Spice4sRelation, Sub <: Spice4sResource](
    res: Res,
    rel: Rel,
    sub: Sub,
    subjectRelation: Option[Relation]
) {
  def request: CheckPermissionRequest = CheckPermissionRequest(
    None,
    res.ref,
    rel.relation,
    SubjectReference(sub.ref, subjectRelation)
  )
}

// trait Spice4sRelationReference[Res <: Spice4sResource, Rel <: Spice4sRelation, Sub <: Spice4sResource] {
//   def resConstants: Spice4sResourceConstants[Res]
//   def relation: Rel
//   def subConstants: Spice4sResourceConstants[Sub]
//   def apply(res: Res, sub: Sub, subjectRelation: Option[Relation]): PermissionRequest[Res, Rel, Sub] =
//     PermissionRequest(res, relation, sub, subjectRelation)
// }

// trait Spice4sUnionRelationReference[Res <: Spice4sResource, Rel <: Spice4sRelation, SubBase[_]] {
//   def resConstonts: Spice4sResourceConstants[Res]
//   def relation: Rel
//   def subConstants: List[Spice4sResourceConstants[?]]
//   def apply[A <: Spice4sResource](res: Res, sub: A, subjectRelation: Option[Relation])(implicit @unused ev: SubBase[A]) =
//     PermissionRequest(res, relation, sub, subjectRelation)
// }
