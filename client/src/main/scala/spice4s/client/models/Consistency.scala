package spice4s.client.models

sealed trait Consistency

object Consistency {
  case object MinimizeLatency extends Consistency
  final case class AtLeastAsFresh(value: ZedToken) extends Consistency
  final case class AtExactSnapshot(value: ZedToken) extends Consistency
  case object FullyConsistent extends Consistency
}
