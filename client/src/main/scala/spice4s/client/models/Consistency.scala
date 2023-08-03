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

import com.authzed.api.v1.{permission_service => ps}

sealed trait Consistency {
  def requirement: ps.Consistency.Requirement
  def encode: ps.Consistency = ps.Consistency.of(requirement)
}

object Consistency {
  case object MinimizeLatency extends Consistency {
    val requirement = ps.Consistency.Requirement.MinimizeLatency(true)
  }
  final case class AtLeastAsFresh(value: ZedToken) extends Consistency {
    val requirement = ps.Consistency.Requirement.AtLeastAsFresh(value.encode)
  }
  final case class AtExactSnapshot(value: ZedToken) extends Consistency {
    val requirement = ps.Consistency.Requirement.AtExactSnapshot(value.encode)
  }
  case object FullyConsistent extends Consistency {
    val requirement = ps.Consistency.Requirement.FullyConsistent(true)
  }
}
