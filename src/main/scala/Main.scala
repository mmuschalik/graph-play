import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Model.Relationship._
import Model._

object Main {

  def main(args: Array[String]): Unit = {
    val a = Resource("A", Attribute("id"), Attribute("a1"), Attribute("b_id"))
    val b = Resource("B", Attribute("id"), Attribute("b1"))
    val c = Resource("C", Attribute("id"), Attribute("c1"))
    val d = Resource("D", Attribute("id"), Attribute("d1"), Attribute("d1"))

    val g = Graph(
      (a ~+# b)(ManyToOne(Attribute("b_id"))), 
      (b ~+# c)(ManyToOne(Attribute("c_id"))),  
      (b ~+# d)(OneToMany(Attribute("b_id"))))


    val yUml = YUml.draw(g)
    val sqlCreate = SQL.create(g)

    Console.println("yUml:")
    Console.println(yUml)
    Console.println("Sql Create:")
    Console.println(sqlCreate)

    {
      import scalax.collection.edge.Implicits.any2XEdgeAssoc
      import SQL.{given _}

      val paths = List((g get a).pathTo(g get c).get.edges.toList, (g get a).pathTo(g get d).get.edges.toList)

      val p = SQL.query(g, paths)

      Console.println(p)
    }
  }
}