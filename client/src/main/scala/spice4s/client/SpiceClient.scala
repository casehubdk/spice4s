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

package spice4s.client

import cats.effect._
import cats.implicits._
import cats._
import com.authzed.api.v1.permission_service.PermissionsServiceFs2Grpc
import spice4s.client.models._
import spice4s.client.util._
import fs2.Stream
import io.grpc._

trait SpiceClient[F[_]] { self =>
  def readRelationships(x: ReadRelationshipsRequest): Stream[F, ReadRelationshipsResponse]

  def writeRelationships(x: WriteRelationshipsRequest): F[WriteRelationshipsResponse]

  def deleteRelationships(x: DeleteRelationshipsRequest): F[DeleteRelationshipsResponse]

  def checkPermission(x: CheckPermissionRequest): F[CheckPermissionResponse]

  def expandPermissionTree(x: ExpandPermissionTreeRequest): F[ExpandPermissionTreeResponse]

  def lookupResources(x: LookupResourcesRequest): Stream[F, LookupResourcesResponse]

  def lookupSubjects(x: LookupSubjectsRequest): Stream[F, LookupSubjectsResponse]

  def mapK[G[_]](fk: F ~> G): SpiceClient[G] = new SpiceClient[G] {
    override def readRelationships(x: ReadRelationshipsRequest): Stream[G, ReadRelationshipsResponse] =
      self.readRelationships(x).translate(fk)

    override def writeRelationships(x: WriteRelationshipsRequest): G[WriteRelationshipsResponse] =
      fk(self.writeRelationships(x))

    override def deleteRelationships(x: DeleteRelationshipsRequest): G[DeleteRelationshipsResponse] =
      fk(self.deleteRelationships(x))

    override def checkPermission(x: CheckPermissionRequest): G[CheckPermissionResponse] =
      fk(self.checkPermission(x))

    override def expandPermissionTree(x: ExpandPermissionTreeRequest): G[ExpandPermissionTreeResponse] =
      fk(self.expandPermissionTree(x))

    override def lookupResources(x: LookupResourcesRequest): Stream[G, LookupResourcesResponse] =
      self.lookupResources(x).translate(fk)

    override def lookupSubjects(x: LookupSubjectsRequest): Stream[G, LookupSubjectsResponse] =
      self.lookupSubjects(x).translate(fk)
  }
}

object SpiceClient {
  def fromClient[F[_]: MonadThrow](client: PermissionsServiceFs2Grpc[F, Unit]): SpiceClient[F] = {
    def decodeWith[A, B](f: A => Decoded[B]): A => F[B] = a => raiseIn[F, B](f(a))

    new SpiceClient[F] {
      def readRelationships(x: ReadRelationshipsRequest): Stream[F, ReadRelationshipsResponse] =
        client.readRelationships(x.encode, ()).evalMap(decodeWith(ReadRelationshipsResponse.decode))

      def writeRelationships(x: WriteRelationshipsRequest): F[WriteRelationshipsResponse] =
        client.writeRelationships(x.encode, ()).flatMap(decodeWith(WriteRelationshipsResponse.decode))

      def deleteRelationships(x: DeleteRelationshipsRequest): F[DeleteRelationshipsResponse] =
        client.deleteRelationships(x.encode, ()).flatMap(decodeWith(DeleteRelationshipsResponse.decode))

      def checkPermission(x: CheckPermissionRequest): F[CheckPermissionResponse] =
        client.checkPermission(x.encode, ()).flatMap(decodeWith(CheckPermissionResponse.decode))

      def expandPermissionTree(x: ExpandPermissionTreeRequest): F[ExpandPermissionTreeResponse] =
        client.expandPermissionTree(x.encode, ()).flatMap(decodeWith(ExpandPermissionTreeResponse.decode))

      def lookupResources(x: LookupResourcesRequest): Stream[F, LookupResourcesResponse] =
        client.lookupResources(x.encode, ()).evalMap(decodeWith(LookupResourcesResponse.decode))

      def lookupSubjects(x: LookupSubjectsRequest): Stream[F, LookupSubjectsResponse] =
        client.lookupSubjects(x.encode, ()).evalMap(decodeWith(LookupSubjectsResponse.decode))
    }
  }

  def fromChannel[F[_]: Async](channel: Channel): Resource[F, SpiceClient[F]] =
    PermissionsServiceFs2Grpc
      .clientResource[F, Unit](channel, _ => new Metadata())
      .map(fromClient[F])
}
