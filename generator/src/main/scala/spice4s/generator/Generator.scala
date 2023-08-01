package spice4s.generator

import cats._
import cats.mtl._
import cats.implicits._
import scala.meta._
import spice4s.parser.SchemaParser._
import cats.data._
import spice4s.client.models.Relation
import cats.parse.Caret

object Generator extends App {
  def resource: Defn.Trait = q"""
    trait Resource {
      def value: String
    }
  """

  def snake2camel(x: String) = {
    val xs = x.split("_")
    xs.headOption.mkString ++ xs.tail.toList.foldMap(_.capitalize)
  }

  def snake2Type(x: String): Type.Name =
    Type.Name(snake2camel(x).capitalize)

  def snake2Obj(x: String): Term.Name =
    Term.Name(snake2camel(x).capitalize)

  def definitionTypeReference(x: String): Type.Ref =
    x.split("/").toList match {
      case x :: Nil      => snake2Type(x)
      case x :: y :: Nil => Type.Select(snake2Obj(x), snake2Type(y))
      case _             => ???
    }

  def definitionValueReference(x: String): Term.Ref =
    x.split("/").toList match {
      case x :: Nil      => snake2Obj(x)
      case x :: y :: Nil => Term.Select(snake2Obj(x), snake2Obj(y))
      case _             => ???
    }

  // def resourceReferenceTrait(rr: ResourceReference, whoReferresToThis: NonEmptyList[String]) = {
  //   val w = whoReferresToThis.map(snake2Type).map(n => Init(n, Term.Name(""), Seq.empty)).toList
  //   q"sealed trait ${Type.Name(rr.resource)} extends ..$w"
  // }

  /*
   definition user {}

   definition horse {}

   definition org {
     relation member: user | horse
   }

   org:acme@member@user:kathryn
   */

  // object TypeclassExample {
  //   final case class User(value: String) extends Resource

  //   final case class Horse(value: String) extends Resource

  //   final case class Org(value: String) extends Resource {
  //     val memberRelation = Relation.unsafeFromString("member")

  //     def member[F[_], A](that: A)(implicit ev: Org.Member[A]): F[Boolean] = {
  //       ???
  //     }
  //   }
  //   object Org {
  //     protected trait Member[A]
  //     implicit object MemberUser extends Member[User]
  //     implicit object MemberHorse extends Member[Horse]
  //   }

  //   Org("hey") member User("hest")
  // }

  // trait Resource {
  //   def value: String
  // }

  // final case class User(value: String) extends Resource

  // final case class Org(value: String) extends Resource

  final case class RelationRef(resource: String, relation: String)
  final case class Error(message: String, position: Option[Caret])
  type RelationCache = Map[RelationRef, Set[String]]
  type Effect[A] = EitherT[State[RelationCache, *], NonEmptyChain[Error], A]
  val S = Stateful[Effect, RelationCache]
  val R = Raise[Effect, NonEmptyChain[Error]]
  val Effect = Monad[Effect]

  def raise[A](msg: String, rr: RelationRef, caret: Option[Caret] = None): Effect[A] =
    // // We set the cache to an empty result, such that the same error for a node is not generated twice
    // S.modify(_.updatedWith(rr)(_.getOrElse(Set.empty).some)) &>
    R.raise(NonEmptyChain.one(Error(msg, caret)))

  def relationDefTypeclass(rd: RelationDef, lookup: Map[String, Resource]) = {
    def goPermissionExpression(pe: PermissionExpression): Effect[Set[String]] = {
      pe
      ???
    }

    def goResourceReference(currentResource: String, rr: ResourceReference): Effect[Set[String]] =
      rr.relation match {
        case None | Some(ResourceRelationType.Wildcard) => Effect.pure(Set(rr.resource))
        case Some(ResourceRelationType.Relation(rel)) =>
          val key = RelationRef(rr.resource, rel)
          S.get
            .flatMap[NonEmptyChain[Error], Option[Set[String]]] { m =>
              m.get(key) match {
                case Some(xs) => Effect.pure(Some(xs))
                // We set the cache to a value such that even if we error, we don't generate the same error twice
                // Furthermore, if we discover ourselves in a cycle, we already have the result
                case None => S.modify(_ + (key -> Set(currentResource))) as none
              }
            }
            .flatMap {
              case Some(xs) => Effect.pure(xs)
              case None =>
                lookup.get(rr.resource) match {
                  case None => raise(s"resource '${rr.resource}' not found", key, Some(rr.caret))
                  case Some(res) =>
                    res.lookup.get(rel) match {
                      case Some(x: PermissionDef) => goPermissionExpression(x.expr)
                      case Some(x: RelationDef) =>
                        x.resources.toList
                          .parTraverse(goResourceReference(res.name, _))
                          .map(_.flatMap(_.toList).toSet)
                      case None =>
                        raise(s"relation '${rel}' not found in resource '${rr.resource}'", key, Some(rr.caret))
                    }
                }
            }
      }

    S.get

    // val uniqueTypes = rd.resources.parTraverse { rr =>
    // }
    q"sealed trait Hset"
  }

  def relationDefRelation(rd: RelationDef): Defn.Val = {
    val n = Term.Name(rd.name)
    val defName = Term.Name(n.value + "Relation")
    q"val ${Pat.Var(defName)} = Relation.unsafeFromString($n)"
  }

  // println(resourceReferenceTrait(ResourceReference("")))

  // println(definitionReference("hest/hest").syntax)

  // def hesteInits = List(init"Resource", init"Resource2(hest)")

  // def definition(
  //     name: String,
  //     exts: List[Init]
  //     // partOf: List[String],
  //     // relations: List[(String, Non

  // ): Defn.Class = q"""
  // final case class ${Type.Name(name)}(value: String) extends ..$exts with Resource {
  // }
  // """

  // def relation(rd: RelationDef) = q"""
  // def ${Type.Name(rd.name)}(that: )
  // """

  // println(definition("Hest", List(init"A.B")))

  // def object
}
