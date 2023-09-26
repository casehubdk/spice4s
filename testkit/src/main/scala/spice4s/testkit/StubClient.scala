package spice4s.testkit

import cats.implicits._
import spice4s.client.SpiceClient
import spice4s.client.models._
import fs2.Stream
import cats.effect._
import cats._

class StubClient[F[_]: Monad](
    state: Ref[F, StubClient.State]
) extends SpiceClient[F] {
  def filterRelations(x: RelationshipFilter) =
    state.get.map(_.relationships.filter { r =>
      r.resource.objectType == x.resourceType &&
      x.resourceId.forall(_ == r.resource.objectId) &&
      x.relation.forall(_ == r.relation) &&
      x.subjectFilter.forall { s =>
        s.subjectType == r.subject.obj.objectType &&
        s.subjectId.forall(_ == r.subject.obj.objectId) &&
        s.relationFilter == r.subject.relation
      }
    })

  override def readRelationships(x: ReadRelationshipsRequest): Stream[F, ReadRelationshipsResponse] =
    Stream.eval(filterRelations(x.relationshipFilter)).flatMap { ys =>
      Stream.emits(
        ys.map(
          ReadRelationshipsResponse(
            ZedToken.unsafeFromString("stub"),
            _,
            Cursor.unsafeFromString("stub")
          )
        )
      )
    }

  override def writeRelationships(x: WriteRelationshipsRequest): F[WriteRelationshipsResponse] =
    state
      .update { s =>
        x.updates.foldLeft(s) { case (current, ru) =>
          import RelationshipUpdate.Operation._
          ru.operation match {
            case Create =>
              if (current.relationships.contains(ru.relationship)) throw new Exception("Relationship already exists")
              else current.copy(relationships = ru.relationship :: current.relationships)
            case Touch  => current.copy(relationships = ru.relationship :: current.relationships.filterNot(_ == ru.relationship))
            case Delete => current.copy(relationships = current.relationships.filterNot(_ == ru.relationship))
          }
        }
      }
      .as(WriteRelationshipsResponse(ZedToken.unsafeFromString("stub")))

  override def deleteRelationships(x: DeleteRelationshipsRequest): F[DeleteRelationshipsResponse] =
    filterRelations(x.filter)
      .flatMap { xs =>
        xs.toNel.traverse_ { nel =>
          writeRelationships(
            WriteRelationshipsRequest(
              nel.map(RelationshipUpdate(RelationshipUpdate.Operation.Delete, _)),
              Nil
            )
          )
        }
      }
      .as(DeleteRelationshipsResponse(ZedToken.unsafeFromString("stub"), DeleteRelationshipsResponse.DeletionProgress.Complete))

  override def checkPermission(x: CheckPermissionRequest): F[CheckPermissionResponse] =
    state.get
      .map(_.relationships.contains(Relationship(x.resource, x.permission, x.subject)))
      .map {
        case true  => CheckPermissionResponse.Permissionship.HasPermission
        case false => CheckPermissionResponse.Permissionship.NoPermission
      }
      .map(CheckPermissionResponse(ZedToken.unsafeFromString("stub"), _))

  // this is probably fun to implement, but also time consuming
  override def expandPermissionTree(x: ExpandPermissionTreeRequest): F[ExpandPermissionTreeResponse] = ???

  override def lookupResources(x: LookupResourcesRequest): Stream[F, LookupResourcesResponse] =
    Stream
      .eval {
        filterRelations(
          RelationshipFilter(
            x.resourceObjectType,
            None,
            x.permission.some,
            Some(
              SubjectFilter(
                x.subjectReference.obj.objectType,
                x.subjectReference.obj.objectId.some,
                x.subjectReference.relation
              )
            )
          )
        )
      }
      .flatMap { xs =>
        Stream.emits {
          xs.map { r =>
            LookupResourcesResponse(
              ZedToken.unsafeFromString("stub"),
              r.resource.objectId.value,
              LookupPermissionship.HasPermission,
              Cursor.unsafeFromString("stub")
            )
          }
        }
      }

  override def lookupSubjects(x: LookupSubjectsRequest): Stream[F, LookupSubjectsResponse] =
    Stream
      .eval {
        filterRelations(
          RelationshipFilter(
            x.objectReference.objectType,
            x.objectReference.objectId.some,
            x.permission.some,
            Some(
              SubjectFilter(
                x.subjectObjectType,
                None,
                x.subjectRelation
              )
            )
          )
        )
      }
      .flatMap { xs =>
        Stream.emits {
          xs.map { r =>
            LookupSubjectsResponse(
              ZedToken.unsafeFromString("stub"),
              ResolvedSubject(r.subject.obj.objectId.value, LookupPermissionship.HasPermission),
              Nil,
              Cursor.unsafeFromString("stub")
            )
          }
        }
      }
}

object StubClient {
  case class State(relationships: List[Relationship])

  def apply[F[_]](implicit F: Concurrent[F]): F[StubClient[F]] =
    F.ref(State(Nil)).map(new StubClient(_))
}
