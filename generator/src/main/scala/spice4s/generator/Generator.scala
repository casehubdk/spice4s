package spice4s.generator

import cats.implicits._
import scala.meta._
import spice4s.parser.SchemaParser._
import cats.data.NonEmptyList

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

  def resourceReferenceExts(rr: ResourceReference) =  {
    rr.resource
  }

  def relationDefRelation(rd: RelationDef): Defn.Val = {
    val n = Term.Name(rd.name)
    val defName = Term.Name(n.value + "Relation")
    q"val ${Pat.Var(defName)} = Relation.unsafeFromString($n)"
  }

  println(resourceReferenceTrait(ResourceReference("")))

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
