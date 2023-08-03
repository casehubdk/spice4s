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
import com.authzed.api.v1.core

final case class Relationship(
    resource: ObjectReference,
    relation: Relation,
    subject: SubjectReference
) {
  def encode = core.Relationship.of(
    resource.encode.some,
    relation.value,
    subject.encode.some,
    None
  )
}

object Relationship {
  def decode(x: core.Relationship): Decoded[Relationship] =
    (
      field("resource")(req(x.resource) andThen ObjectReference.decode),
      field("relation")(Relation.decode(x.relation) andThen req),
      field("subject")(req(x.subject) andThen SubjectReference.decode)
    ).mapN(Relationship.apply)
}
