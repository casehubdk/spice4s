package spice4s.generator

import cats.implicits._
import scala.meta._
import spice4s.parser.SchemaParser._

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

  def definitionReference(x: String): Type.Ref =
    x.split("/").toList match {
      case x :: Nil      => snake2Type(x)
      case x :: y :: Nil => Type.Select(snake2Obj(x), snake2Type(y))
      case _             => ???
    }

  def resourceReference(rr: ResourceReference) = {
    rr.resource
  }

  def relationDef(rd: RelationDef) = {
    
  }

  println(definitionReference("hest/hest").syntax)

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
