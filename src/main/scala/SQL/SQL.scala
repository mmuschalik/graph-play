package SQL

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits.any2XEdgeAssoc
import scala.language.implicitConversions
import Model.Relationship._
import Model._
import SQL.{given _}

def create[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E]): String = 
  g.nodes
  .map(n => s"CREATE TABLE ${n.name}(${n.identity.name} serial primary key, ${n.attributes.map(a => a.name).mkString(", ")});")
  .mkString("\n")

def query[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E], paths: List[List[g.EdgeT]]): String = 
  val t = paths
    .headOption
    .flatMap(p => p.headOption)
    .fold("")(_._1.name)

  val q = queryWhere(g, paths, Query(SelectList("*"), TableExpression(TableName(s"${t}"), None)))// s"SELECT ${t}.* FROM ${t} ")

  summon[Show[Query]].show(q)

def queryWhere[E[+X] <: EdgeLikeIn[X]](g: Graph[Resource, E], paths: List[List[g.EdgeT]], query: Query): Query = 
  if paths.isEmpty then
    query // fix
  else 
    val qs = paths
      .groupBy(p => p.headOption)
      .collect { case (Some(s), list) => (s, list) }
      .view
      .mapValues(v => v.map(_.tail))
      .map((edge, list) => (edge.label
        match
          case ManyToOne(m) => {
            val q = queryWhere(g, list, Query(SelectList("*"), TableExpression(TableName(s"${edge._2.name}"), None))) //.appendJoin(s" JOIN ${edge._2.name} ON (${edge._2.name}.${edge._2.identity.name} = ${edge._1.name}.${m.name})")
            (getJoin(query, q, JoinCondition(s"${edge._1.name}.${m.name}", s"${edge._2.name}.${edge._2.identity.name}")), getWhere(q))
          }
          case OneToMany(m) => 
            (None, Some(WhereClause(InClause(s"${edge._1.name}.${edge._2.identity.name}", queryWhere(g, list, Query(SelectList(s"${edge._2.name}.${m.name}"), TableExpression(TableName(s"${edge._2.name}"), None)))) :: Nil)))))
  
    val js = qs.flatMap(q => q._1.toList).toList
      match
        case Nil => query.tableExpression.tableReference
        case v :: Nil => v
        case l => l.reduce((a, b) => Join(a, b, b 
          match
            case Join(_, _, c) => c
            case _ => None)) // fix None
    
    val ws = listToOptionList(query.tableExpression.where.toList.flatMap(x => x.clauses) ++ qs.toList.flatMap(q => q._2.toList.flatMap(x => x.clauses)))
    
    Query(query.selectList, TableExpression(js, ws.map(WhereClause(_))))

def getJoin(left: Query, right: Query, condition: JoinCondition): Option[TableReference] = Some(Join(left.tableExpression.tableReference, right.tableExpression.tableReference, Some(condition)))

def getWhere(query: Query): Option[WhereClause] = query.tableExpression.where

def listToOptionList[A](list: List[A]) = if list.isEmpty then None else Some(list)


