package YUml

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Model.Relationship._
import Model._

def draw[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E]): String = 
  val rels = g
      .edges
      .map(e => s"[${e._1.name}]${e.label
        match
          case ManyToOne(_) => "*-1" 
          case OneToMany(_) => "1-*"
          case OneToOne(_) => "1-1"
          case _ => "-"}${if e.isDirected then ">" else ""}[${e._2.name}]")
  
  val details = g
    .nodes
    .map(n => s"[${n.name}|${n.attributes.map(_.name).mkString(";")}]")
    
  (rels ++ details)
    .mkString("\n")