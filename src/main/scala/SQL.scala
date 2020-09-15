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
            (getJoin(q), getWhere(q))
          }
          case OneToMany(m) => 
            (None, Some(WhereClause(InClause(s"${edge._1.name}.${edge._2.identity.name}", queryWhere(g, list, Query(SelectList(s"${edge._2.name}.${m.name}"), TableExpression(TableName(s"${edge._2.name}"), None)))) :: Nil)))))
  
    val js = qs.flatMap(q => q._1.toList).fold(query.tableExpression.tableReference)((a, b) => Join(a, b, b 
      match
        case Join(_, _, c) => c
        case _ => None)) // fix None
    
    val ws = WhereClause(query.tableExpression.where.toList.flatMap(x => x.clauses) ++ qs.toList.flatMap(q => q._2.toList.flatMap(x => x.clauses)))
    
    Query(query.selectList, TableExpression(js, Some(ws)))

def getJoin(query: Query): Option[TableReference] = Some(query.tableExpression.tableReference)

def getWhere(query: Query): Option[WhereClause] = query.tableExpression.where

case class Query(selectList: SelectList, tableExpression: TableExpression)
case class SelectList(list: String)
case class TableExpression(tableReference: TableReference, where: Option[WhereClause])

sealed trait TableReference
case class TableName(tableName: String) extends TableReference
case class Join(t1: TableReference, t2: TableReference, joinCondition: Option[JoinCondition]) extends TableReference

case class JoinCondition(left: String, right: String)
case class WhereClause(clauses: List[InClause])
case class InClause(column: String, query: Query)

trait Show[A] {
  def show(a: A): String
}

given tableReferenceShow as Show[TableReference]() {
  def show(a: TableReference): String = a
    match
      case TableName(n) => n
      case Join(t1, t2, c) => show(t1) + " JOIN " + show(t2) + c.fold("")(x => x.left + " = " + x.right)
}

given tableExpressionShow as Show[TableExpression] {
  def show(a: TableExpression): String = summon[Show[TableReference]].show(a.tableReference) + a.where.fold("")(x => " WHERE " + summon[Show[WhereClause]].show(x))
}

given whereClauseShow as Show[WhereClause] {
  def show(a: WhereClause): String = a.clauses.map(c => c.column + " IN (" + summon[Show[Query]].show(c.query) + ")").mkString(" AND ")
}

given queryShow as Show[Query] {
  def show(a: Query): String = "SELECT " + a.selectList.list + " FROM " + summon[Show[TableExpression]].show(a.tableExpression)
}