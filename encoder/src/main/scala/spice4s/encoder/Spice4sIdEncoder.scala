package spice4s.encoder

import spice4s.client.models._
import cats.{Id => _, _}
import java.util.UUID

trait Spice4sIdEncoder[A] extends Spice4sEncoder[A, Id] {
  def encode(a: A): Id
}

object Spice4sIdEncoder extends Spice4sEncoderOps[Id] {
  type Enc[A] = Spice4sIdEncoder[A]

  override def of[A](f: A => Id): Enc[A] = new Spice4sIdEncoder[A] {
    override def encode(a: A): Id = f(a)
  }

  override def unsafeFromString: Spice4sEncoder[String, Id] =
    of[String](Id.unsafeFromString)
}

trait Spice4sTypeEncoder[A] extends Spice4sEncoder[A, Type] {
  def encode(a: A): Type
}

object Spice4sTypeEncoder extends Spice4sEncoderOps[Type] {
  type Enc[A] = Spice4sTypeEncoder[A]

  override def of[A](f: A => Type): Enc[A] = new Spice4sTypeEncoder[A] {
    override def encode(a: A): Type = f(a)
  }

  override def unsafeFromString: Spice4sEncoder[String, Type] =
    of[String](Type.unsafeFromString)
}

trait Spice4sRelationEncoder[A] extends Spice4sEncoder[A, Relation] {
  def encode(a: A): Relation
}

object Spice4sRelationEncoder extends Spice4sEncoderOps[Relation] {
  type Enc[A] = Spice4sRelationEncoder[A]

  override def of[A](f: A => Relation): Enc[A] = new Spice4sRelationEncoder[A] {
    override def encode(a: A): Relation = f(a)
  }

  override def unsafeFromString: Spice4sEncoder[String, Relation] =
    of[String](Relation.unsafeFromString)
}

trait Spice4sEncoder[A, B] {
  def encode(a: A): B
}

trait Spice4sEncoderOps[B] {
  type Enc[A] <: Spice4sEncoder[A, B]

  def apply[A](implicit ev: Enc[A]): Enc[A] = ev

  def of[A](f: A => B): Enc[A]

  implicit val idEncoder: Spice4sEncoder[B, B] = of(identity)

  def unsafeFromString: Spice4sEncoder[String, B]

  implicit lazy val uuidEncoder: Enc[UUID] =
    of[UUID](x => unsafeFromString.encode(x.toString().replace("-", "_")))

  implicit lazy val intEncoder: Enc[Int] =
    of[Int](x => unsafeFromString.encode(x.toString()))

  implicit val contravariant: ContravariantSemigroupal[Enc] =
    new ContravariantSemigroupal[Enc] {
      override def product[A, B](fa: Enc[A], fb: Enc[B]): Enc[(A, B)] =
        of[(A, B)] { case (a, b) => unsafeFromString.encode(s"${fa.encode(a)}_${fb.encode(b)}") }

      override def contramap[A, B](fa: Enc[A])(f: B => A): Enc[B] =
        of[B](b => fa.encode(f(b)))
    }
}
