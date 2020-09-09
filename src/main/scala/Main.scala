import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import math.Integral.Implicits.infixIntegralOps
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions

object Main {

  def main(args: Array[String]): Unit = {
    val a = Resource("A", "id", "A.body")
    val b = Resource("B", "id", "B.body")
    val c = Resource("C", "id", "C.body")
    val d = Resource("D", "id", "D.body")

    val g = Graph(
      (a ~+#> b)(Relationship("*", "1")), 
      (b ~+# c)(Relationship("1", "1")),  
      (d ~+#> b)(Relationship("1", "*")))


    val yUml =
      g.edges
        .map(e => s"[${e._1.name}]${e.label
          match
            case r: Relationship => r.fromCardinality + "-" + r.toCardinality 
            case _ => "-"}${if e.isDirected then ">" else ""}[${e._2.name}]")
        .mkString("\n")
  
    val sqlCreate =
      g.nodes
        .map(n => s"CREATE TABLE ${n.name}(${n.identity} serial primary key);")
        .mkString("\n")

    Console.println("yUml:")
    Console.println(yUml)
    Console.println("Sql Create:")
    Console.println(sqlCreate)

  }
}

case class Resource[I, B](name: String, identity: I, body: B)
case class Relationship(fromCardinality: String, toCardinality: String)