package SQL

case class Query(selectList: SelectList, tableExpression: TableExpression)
case class SelectList(list: String)
case class TableExpression(tableReference: TableReference, where: Option[WhereClause])

sealed trait TableReference
case class TableName(tableName: String) extends TableReference
case class Join(t1: TableReference, t2: TableReference, joinCondition: Option[JoinCondition]) extends TableReference

case class JoinCondition(left: String, right: String)
case class WhereClause(clauses: List[InClause])
case class InClause(column: String, query: Query)
