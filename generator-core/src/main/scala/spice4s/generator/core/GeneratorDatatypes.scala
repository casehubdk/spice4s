package spice4s.generator.core

import spice4s.client.models._
import scala.annotation.unused
import cats.data._

trait Spice4sResource {
  def companion: Spice4sCompanion[?]
  def id: Id
  def ref: ObjectReference = ObjectReference(
    companion.constants.objectType,
    id
  )
}

trait Spice4sCompanion[A <: Spice4sResource] {
  def constants: Spice4sConstants[A]
}

trait Spice4sConstants[A <: Spice4sResource] {
  def objectType: Type
}

trait Spice4sRelationType {
  def relation: Relation
}

final case class PermissionRequest[Res <: Spice4sResource, Rel <: Spice4sRelationType, Sub <: Spice4sResource](
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

trait Spice4sRelation[Res <: Spice4sResource, Rel <: Spice4sRelationType, Sub <: Spice4sResource] {
  def resource: Spice4sCompanion[Res]
  def relation: Rel
  def subResource: Spice4sCompanion[Sub]
  def apply(res: Res, sub: Sub, subjectRelation: Option[Relation]): PermissionRequest[Res, Rel, Sub] =
    PermissionRequest(res, relation, sub, subjectRelation)
  def check(res: Res)(sub: Sub): PermissionRequest[Res, Rel, Sub] =
    apply(res, sub, None)
  def checkSub(res: Res)(sub: Sub, subjectRelation: Relation): PermissionRequest[Res, Rel, Sub] =
    apply(res, sub, Some(subjectRelation))
}

trait Spice4sUnionRelation[
  Res <: Spice4sResource, 
  Rel <: Spice4sRelationType,
  Sub[x <: Spice4sResource] <: Spice4sCompanion[x]
] {
  def resource: Spice4sCompanion[Res]
  def relation: Rel
  def subs: NonEmptyList[Sub[?]]
  def apply[A <: Spice4sResource](res: Res, sub: A, subjectRelation: Option[Relation])(implicit @unused ev: Sub[A]) =
    PermissionRequest(res, relation, sub, subjectRelation)
  def check[A <: Spice4sResource](res: Res)(sub: A)(implicit ev: Sub[A]) =
    apply(res, sub, None)
  def checkSub[A <: Spice4sResource](res: Res)(sub: A, subjectRelation: Relation)(implicit ev: Sub[A]) =
    apply(res, sub, Some(subjectRelation))
}