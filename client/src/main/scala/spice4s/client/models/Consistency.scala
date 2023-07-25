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
