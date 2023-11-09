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
  def sub(subjectRelation: Option[Relation]) =
    SubjectReference(ref, subjectRelation)
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

final case class RelationshipRequest[Res <: Spice4sResource, Rel <: Spice4sRelationType, Sub <: Spice4sResource](
    res: Res,
    rel: Rel,
    sub: Sub,
    subjectRelation: Option[Relation]
) {
  def checkPermission: CheckPermissionRequest = CheckPermissionRequest(
    None,
    res.ref,
    rel.relation,
    SubjectReference(sub.ref, subjectRelation)
  )

  def relationship: Relationship = Relationship(
    res.ref,
    rel.relation,
    sub.sub(subjectRelation)
  )

  def writeRelationship(op: RelationshipUpdate.Operation): WriteRelationshipsRequest =
    WriteRelationshipsRequest(
      NonEmptyList.one(RelationshipUpdate(op, relationship)),
      Nil
    )

  def touch: WriteRelationshipsRequest =
    writeRelationship(RelationshipUpdate.Operation.Touch)

  def create: WriteRelationshipsRequest =
    writeRelationship(RelationshipUpdate.Operation.Create)

  def delete: WriteRelationshipsRequest =
    writeRelationship(RelationshipUpdate.Operation.Delete)
}

trait Spice4sAnyRelation[Res <: Spice4sResource, Rel <: Spice4sRelationType] {
  def resource: Spice4sCompanion[Res]
  def relation: Rel
  def subjectRelation: Option[Relation]
  def isPermission: Boolean
  def subjects: NonEmptyList[Spice4sCompanion[? <: Spice4sResource]]
}

trait Spice4sRelation[Res <: Spice4sResource, Rel <: Spice4sRelationType, Sub <: Spice4sResource] extends Spice4sAnyRelation[Res, Rel] {
  def subResource: Spice4sCompanion[Sub]
  def subjects: NonEmptyList[Spice4sCompanion[? <: Spice4sResource]] = NonEmptyList.one(subResource)
  def apply(res: Res, sub: Sub): RelationshipRequest[Res, Rel, Sub] =
    RelationshipRequest(res, relation, sub, subjectRelation)
}

trait Spice4sUnionRelation[
    Res <: Spice4sResource,
    Rel <: Spice4sRelationType,
    Sub[x <: Spice4sResource] <: Spice4sCompanion[x]
] extends Spice4sAnyRelation[Res, Rel] {
  def subjects: NonEmptyList[Sub[? <: Spice4sResource]]
  def apply[A <: Spice4sResource](res: Res, sub: A)(implicit @unused ev: Sub[A]): RelationshipRequest[Res, Rel, A] =
    RelationshipRequest(res, relation, sub, subjectRelation)
}
