package YUml

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Model.Relationship._
import Model._

def draw[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E]): String = 
  g.edges
  .map(e => s"[${e._1.name}]${e.label
    match
      case ManyToOne => "*-1" 
      case OneToMany => "1-*"
      case OneToOne => "1-1"
      case _ => "-"}${if e.isDirected then ">" else ""}[${e._2.name}]")
  .mkString("\n")