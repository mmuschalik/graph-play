package SQL

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Model.Relationship._
import Model._


def create[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E]): String = 
  g.nodes
  .map(n => s"CREATE TABLE ${n.name}(${n.identity.name} serial primary key, ${n.attributes.map(a => a.name).mkString(", ")});")
  .mkString("\n")

def query[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E], paths: List[List[g.EdgeT]]): String = 
  if paths.isEmpty then
    ""
  else 
    "WHERE " +
    paths
      .groupBy(p => p.headOption)
      .view
      .filterKeys(k => k.isDefined)
      .mapValues(v => v.map(_.tail))
      .map(e => s"${e._1.get.label
        match
          case m: ManyToOne => m.refAttribute.name
          case m: OneToMany => m.refAttribute.name
          case m: OneToOne => m.refAttribute.name
        } IN (SELECT ${e._1.get._2.identity.name} FROM ${e._1.get._2.name} ${query(g, e._2)})")
      .mkString(" AND ")
