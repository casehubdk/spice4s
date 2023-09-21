package spice4s.generator.core

import spice4s.client.models._
import scala.annotation.unused
import cats.data._

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

trait Spice4sResourceConstantsRef[A <: Spice4sResource] {
  def constants: Spice4sResourceConstants[A]
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

trait Spice4sRelationReference[Res <: Spice4sResource, Rel <: Spice4sRelation, Sub <: Spice4sResource] {
  def resConstants: Spice4sResourceConstants[Res]
  def relation: Rel
  def subConstants: Spice4sResourceConstants[Sub]
  def apply(res: Res, sub: Sub, subjectRelation: Option[Relation]): PermissionRequest[Res, Rel, Sub] =
    PermissionRequest(res, relation, sub, subjectRelation)
  def check(res: Res)(sub: Sub): PermissionRequest[Res, Rel, Sub] =
    apply(res, sub, None)
  def checkSub(res: Res)(sub: Sub, subjectRelation: Relation): PermissionRequest[Res, Rel, Sub] =
    apply(res, sub, Some(subjectRelation))
}

trait Spice4sUnionRelationReference[Res <: Spice4sResource, Rel <: Spice4sRelation, Sub[x <: Spice4sResource] <: Spice4sResourceConstantsRef[x]] {
  def resConstonts: Spice4sResourceConstants[Res]
  def relation: Rel
  def subs: NonEmptyList[Sub[?]]
  def apply[A <: Spice4sResource](res: Res, sub: A, subjectRelation: Option[Relation])(implicit @unused ev: Sub[A]) =
    PermissionRequest(res, relation, sub, subjectRelation)
  def check[A <: Spice4sResource](res: Res)(sub: A)(implicit ev: Sub[A]) =
    apply(res, sub, None)
  def checkSub[A <: Spice4sResource](res: Res)(sub: A, subjectRelation: Relation)(implicit ev: Sub[A]) =
    apply(res, sub, Some(subjectRelation))
}