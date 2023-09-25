package spice4s.encoder

import spice4s.client.models._
import cats.{Id => _, _}
import java.util.UUID

trait Spice4sEncoder[A] {
  def encode(a: A): Id
}

object Spice4sEncoder {
  def apply[A](implicit ev: Spice4sEncoder[A]): Spice4sEncoder[A] = ev

  def of[A](f: A => Id): Spice4sEncoder[A] = new Spice4sEncoder[A] {
    override def encode(a: A): Id = f(a)
  }

  implicit val idEncoder: Spice4sEncoder[Id] = of(identity)

  def unsafeFromString: Spice4sEncoder[String] =
    of[String](Id.unsafeFromString)

  implicit lazy val uuidEncoder: Spice4sEncoder[UUID] =
    of[UUID](x => unsafeFromString.encode(x.toString()))

  implicit lazy val intEncoder: Spice4sEncoder[Int] =
    of[Int](x => unsafeFromString.encode(x.toString()))

  implicit val contravariant: ContravariantSemigroupal[Spice4sEncoder] =
    new ContravariantSemigroupal[Spice4sEncoder] {
      override def product[A, B](fa: Spice4sEncoder[A], fb: Spice4sEncoder[B]): Spice4sEncoder[(A, B)] =
        of[(A, B)] { case (a, b) => unsafeFromString.encode(s"${fa.encode(a)}/${fb.encode(b)}") }

      override def contramap[A, B](fa: Spice4sEncoder[A])(f: B => A): Spice4sEncoder[B] =
        of[B](b => fa.encode(f(b)))
    }
}
