import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
//import math.Integral.Implicits.infixIntegralOps
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Relationship._

object Main {

  def main(args: Array[String]): Unit = {
    val a = Resource("A", Attribute("id"), Attribute("a1"), Attribute("a2"))
    val b = Resource("B", Attribute("id"), Attribute("b1"))
    val c = Resource("C", Attribute("id"), Attribute("c1"))
    val d = Resource("D", Attribute("id"), Attribute("d1"), Attribute("d1"))

    val g = Graph(
      (a ~+#> b)(ManyToOne), 
      (b ~+# c)(OneToOne),  
      (b ~+#> d)(ManyToOne))


    val yUml =
      g.edges
        .map(e => s"[${e._1.name}]${e.label
          match
            case ManyToOne => "*-1" 
            case OneToMany => "1-*"
            case OneToOne => "1-1"
            case _ => "-"}${if e.isDirected then ">" else ""}[${e._2.name}]")
        .mkString("\n")
  
    val sqlCreate =
      g.nodes
        .map(n => s"CREATE TABLE ${n.name}(${n.identity.name} serial primary key, ${n.attributes.map(a => a.name).mkString(", ")});")
        .mkString("\n")

    Console.println("yUml:")
    Console.println(yUml)
    Console.println("Sql Create:")
    Console.println(sqlCreate)

    {
      import scalax.collection.edge.Implicits.any2XEdgeAssoc
      
      val paths = List((g get b).pathTo(g get c).get.edges.toList, (g get b).pathTo(g get d).get.edges.toList)

      val p = f(g, paths)

      //find all unique edges leaving a

      Console.println(p)
    }
  }

  def f[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E], paths: List[List[g.EdgeT]]): String = 
    if paths.isEmpty then
      ""
    else
      paths
        .groupBy(p => p.headOption)
        .view
        .filterKeys(k => k.isDefined)
        .mapValues(v => v.tail)
        .map(e => "fk IN (SELECT id FROM " + e._1.get._2.name + " WHERE " + f(g, e._2) + ")")
        .mkString(" AND ") // f(g, e._2.tail))
  }

case class Resource(name: String, identity: Attribute, attributes: Attribute*)
case class Attribute(name: String)

enum Relationship {
  case ManyToOne, OneToMany, OneToOne
}