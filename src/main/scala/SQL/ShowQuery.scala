package SQL

trait Show[A] {
  def show(a: A): String
}

given tableReferenceShow as Show[TableReference]() {
  def show(a: TableReference): String = (listTableReferences(a) zip ("" :: listConditions(a))).map(_ + _).mkString(" JOIN ")

  def listTableReferences(a: TableReference): List[String] = a
    match
      case t: TableName => t.tableName :: Nil
      case Join(t1, t2, c) => listTableReferences(t1) ++  listTableReferences(t2)
        
  def listConditions(a: TableReference): List[String] = a
    match
      case t: TableName => Nil
      case Join(t1, t2, c) => listConditions(t1) ++ c.map(x => " ON (" + x.left + " = " + x.right + ")").toList ++  listConditions(t2)

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