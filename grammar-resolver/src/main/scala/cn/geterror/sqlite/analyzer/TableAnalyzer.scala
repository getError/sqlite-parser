package cn.geterror.sqlite.analyzer

import cn.geterror.sqlite.analyzer.ProjectionAnalyzerUtil.dealStar
import cn.geterror.sqlite.exception.SqlResolveException
import cn.geterror.sqlite.expr.Expr
import cn.geterror.sqlite.result._

class TableAnalyzer(factTable: Seq[FactTable]) {
  def resolveTable(table:Table):ResolvedTable = {
    val tableMap = factTable.map(i => (i.tableName, i)).toMap
    table.transformUp({
      case node:SimpleTable if tableMap.contains(node.tableName)=>ResolvedSimpleTable(node.alias,node.tableName,tableMap(node.tableName).fields)
      case node:JoinTable=>resolveJoinTable(node)
      case node:SubQuery=>resolveSubQuery(node)
    }).asInstanceOf[ResolvedTable]
  }
  def resolveProjection(fields:Seq[UnresolvedProjection],currentTables:Seq[ResolvedTable]):Seq[ResolvedProjection]={
    fields.flatMap(bindProjectionAndTable(_,unzipJoinTable(currentTables))).toIndexedSeq
  }

  def resolvedExpr(expr:Expr,currentTables:Seq[ResolvedTable])(implicit exprAnalyzer:ExprAnalyzer = new ExprAnalyzer(unzipJoinTable(currentTables), factTable)): Expr ={

    exprAnalyzer.resolvedExpr(expr)
  }

  def resolveJoinTable(table:JoinTable):ResolvedJoinTable={
    if(!table.children.forall(_.isResolved)){
      throw new SqlResolveException("表的子节点未完全解析")
    }
    val projections = table.children.map(_.asInstanceOf[ResolvedTable]).flatMap(t => t.fields.map({
      case f: FactFieldProjection => JoinedFieldProjection(t,f.alias, Seq(f.factTable), f.expr)
      case f: JoinedFieldProjection => JoinedFieldProjection(t,f.alias, f.factTables, f.expr)
      case f: ResolvedFieldProjection  => JoinedFieldProjection(t,f.alias, f.factTables, f.expr)
      case f: StarFieldProjection => JoinedFieldProjection(t,f.alias, f.factTables, f.expr)
    })).toIndexedSeq
    ResolvedJoinTable(table.alias,table.children.map(_.asInstanceOf[ResolvedTable]),table.joinType,projections)
  }

  // 展开Join表
  def unzipJoinTable(tables:Seq[ResolvedTable]):Seq[ResolvedTable]={
    tables.flatMap({
      case j: ResolvedJoinTable => j.children
      case t: ResolvedTable => Seq(t)
      case unresolvedTable=>throw new SqlResolveException(s"未解析的表$unresolvedTable")
    })
  }

  def resolvedParseResult(parserResult: ParserResult):ParserResult={
    val resolvedTables = Seq(parserResult.table match {
      case t: ResolvedTable => t
      case t: UnresolvedTable => resolveTable(t)
    })
    val expansionTables = unzipJoinTable(resolvedTables)
    val projections = parserResult.projections.flatMap(i => bindProjectionAndTable(i, expansionTables))

    val analyzer = new ExprAnalyzer(expansionTables, factTable)
    val resolvedWhere = parserResult.where.map(analyzer.resolvedExpr)
    val resolvedHaving= parserResult.having.map(analyzer.resolvedExpr)
    val resolvedGroup = parserResult.groupBy.map(analyzer.resolvedExpr)
    ParserResult(projections, resolvedTables.head, resolvedWhere, resolvedGroup, resolvedHaving)
  }

  def resolveSubQuery(subQuery:SubQuery):ResolvedSubQuery={
    val result = resolvedParseResult(subQuery.parserResult)
    ResolvedSubQuery(subQuery.alias,Seq(result.table),result)
  }

  def bindProjectionAndTable(projection:Projection,resolvedTables:Seq[ResolvedTable]):Seq[ResolvedProjection]={


    projection match {
      case star: UnresolvedStarProjection=>resolvedTables.flatMap(dealStar)
      case tableStar: UnresolvedTableStarProjection=>resolvedTables.flatMap(table=>table match {
        case t:ResolvedTable if t.alias.exists(tableStar.tableName.equals(_))=> dealStar(t)
        case t:FactTable if tableStar.tableName.equals(t.tableName)=> dealStar(t)(tableAlias = Option(t.tableName))
        case _=>Seq()
      })
      case f:UnresolvedFieldProjection => {
        val projectionAnalyzer = new ProjectionAnalyzer(resolvedTables, factTable)
        Seq(projectionAnalyzer.dealFieldProjection(f))
      }
    }
  }

}
