package cn.geterror.sqlite.parser

import cn.geterror.sqlite.result._
import cn.geterror.sqlite.exception.SqlParserException
import cn.geterror.sqlite.expr._
import cn.geterror.sqlite.parser.SQLiteParser._
import cn.geterror.sqlite.util.CollectionUtil._
import cn.geterror.sqlite.util.ParserUtil.getText

import java.util.stream.Collectors
import scala.language.postfixOps

class SqlitePlanBuilder extends SQLiteParserBaseVisitor[Any]{
  override def visitParse(ctx: ParseContext): ParserResult = {
    val sqlCtxList = ctx.sql_stmt_list
    if(sqlCtxList==null||sqlCtxList.size()!=1){
      throw new SqlParserException("仅解析单条SQL")
    }
    visitSql_stmt(sqlCtxList.get(0).sql_stmt(0))
  }

  override def visitSql_stmt(ctx: Sql_stmtContext): ParserResult = {
    if(ctx.select_stmt()==null){
      throw new SqlParserException("仅解析SELECT语句")
    }
    visitSelect_stmt(ctx.select_stmt())
  }

  override def visitSelect_stmt(ctx: Select_stmtContext): ParserResult = {
    //我们只关心select_core第一部分，其他部分由于是Union All的关系，不会影响解析结果
    visitSelect_core(toSeq(ctx.select_core()).head)

  }

  override def visitSelect_core(ctx: Select_coreContext): ParserResult = {

    // 从表里获得源表信息，从子查询里获得
    val tables = (toSeq(ctx.table_or_subquery()).map(visitTableOrSubQuery)++
      Option(ctx.join_clause()).toSeq.map(i=>JoinTable(None,visitJoin_clause(i)))).toIndexedSeq

    val table = if(tables.size==1){tables.head}else{JoinTable(None,tables)}

    // 获得查询字段
    val projections = toSeq(ctx.result_column()).map(visitResultColumn).toIndexedSeq
    // 获得where条件
    val whereExpr = Option(ctx.whereExpr).flatMap(e=>Option(visitExpr(e)))
    // 获得group by字段
    val groupExpr = toSeq(ctx.groupExpr).map(visitExpr).toIndexedSeq
    // 获得having字段
    val havingExpr = Option(ctx.havingExpr).flatMap(e=>Option(visitExpr(e)))
    ParserResult(projections,table,whereExpr,groupExpr,havingExpr)
  }

  def visitResultColumn(ctx: Result_columnContext): UnresolvedProjection = {
    ctx match {
      case ctx: StarColumnContext => visitStarColumn(ctx)
      case ctx: TableStarColumnContext => visitTableStarColumn(ctx)
      case ctx: ExprColumnContext => visitExprColumn(ctx)
      case _=> throw new SqlParserException("不支持的结果列类型")
    }
  }

  override def visitStarColumn(ctx: StarColumnContext): UnresolvedStarProjection = {
    UnresolvedStarProjection()
  }

  override def visitTableStarColumn(ctx: TableStarColumnContext): UnresolvedTableStarProjection = {
    UnresolvedTableStarProjection(ctx.table_name.getText)
  }

  override def visitExprColumn(ctx: ExprColumnContext): UnresolvedFieldProjection = {
    val expr = visitExpr(ctx.expr())
    val alias = Option(ctx.column_alias()).flatMap(a=>Option(a.getText))
    val fields:Seq[ColumnResultExpr] =
      ExprDriver.findTypeInExpr(expr, classOf[ColumnResultExpr]).seq
    UnresolvedFieldProjection(Option(expr),alias)
  }

  def visitExpr(ctx: ExprContext): Expr = {
    ctx.accept(this).asInstanceOf[Expr]
  }

  override def visitLiteralValue(ctx: LiteralValueContext): LiteralExpr = {
    LiteralExpr(ctx.getText)
  }

  override def visitBindParameter(ctx: BindParameterContext): Nothing = {
    throw new SqlParserException("暂不支持BindParameter")
  }

  override def visitColumnExpr(ctx: ColumnExprContext): ColumnExpr = {
    ColumnExpr(Option(ctx.schema_name()).flatMap(s=>Option(s.getText)),
      Option(ctx.table_name()).flatMap(s=>Option(s.getText)),ctx.column_name.getText)
  }

  override def visitUnaryOpExpr(ctx: UnaryOpExprContext): UnaryExpr = {
    UnaryExpr(ctx.unary_operator().getText,visitExpr(ctx.expr()))
  }

  override def visitPipeOpExpr(ctx: PipeOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.PIPE2().getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitMulDivModOpExpr(ctx: MulDivModOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitAddSubOpExpr(ctx: AddSubOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitCompOpExpr(ctx: CompOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitAssignOpExpr(ctx: AssignOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitAndOpExpr(ctx: AndOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitOrOpExpr(ctx: OrOpExprContext): BinaryExpr = {
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitFunctionExpr(ctx: FunctionExprContext): FunctionExpr = {
    if(ctx.filter_clause()!=null || ctx.over_clause()!=null){
      throw new SqlParserException("暂不支持开窗")
    }
    val distinct = ctx.DISTINCT_()==null
    if(ctx.STAR()!=null){
      FunctionExpr(ctx.function_name.getText,distinct,Seq(StarExpr()))
    }
    FunctionExpr(ctx.function_name.getText,distinct,toSeq(ctx.expr()).map(visitExpr))
  }

  override def visitGroupExpr(ctx: GroupExprContext): TupleExpr = {
    TupleExpr(toSeq(ctx.expr()).map(visitExpr))
  }

  override def visitCastExpr(ctx: CastExprContext): CastExpr = {
    CastExpr(visitExpr(ctx.expr()),ctx.type_name().getText)
  }
  override def visitCollateExpr(ctx: CollateExprContext): Expr = {
//    super.visitCollateExpr(ctx)
    throw new SqlParserException("暂不支持Collate")
  }

  override def visitLikeExpr(ctx: LikeExprContext): BinaryExpr ={
    if(ctx.NOT_()!=null){
      BinaryExpr("NOT "+ctx.children.get(2).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
    }
    BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
  }

  override def visitNullExpr(ctx: NullExprContext):BinaryExpr ={
    if(ctx.NOT_()!=null||ctx.NOTNULL_()!=null){
      BinaryExpr("NOT "+ctx.children.get(2).getText,visitExpr(ctx.expr),LiteralExpr("NULL"))
    }else{
      BinaryExpr("IS ",visitExpr(ctx.expr),LiteralExpr("NULL"))
    }
  }

  override def visitEqExpr(ctx: EqExprContext): BinaryExpr={
    if(ctx.NOT_()!=null){
      BinaryExpr(ctx.children.get(2).getText+" NOT",visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
    }else{
      BinaryExpr(ctx.children.get(1).getText,visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)))
    }
  }

  override def visitBetweenExpr(ctx: BetweenExprContext): TernaryExpr={
    if(ctx.NOT_()!=null){
      TernaryExpr("NOT "+ctx.children.get(2).getText,"AND",visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)),visitExpr(ctx.expr(2)))
    }else{
      TernaryExpr(ctx.children.get(1).getText,"AND",visitExpr(ctx.expr(0)),visitExpr(ctx.expr(1)),visitExpr(ctx.expr(2)))
    }
  }

  override def visitInExpr(ctx: InExprContext): Any = {
    if(ctx.table_function_name()!=null){
      throw new SqlParserException("暂不支持表函数")
    }
    val inStr = if(ctx.NOT_()!=null){"NOT IN"}else{"IN"}

    /**
     * IN schema.table
     */
    val inClause = if(ctx.table_name()!=null){
      val tableExpr = SchemaTableStarExpr(Option(ctx.schema_name()).flatMap(s => Option(s.getText)), Option(ctx.table_name()).flatMap(s => Option(s.getText)))
      BinaryExpr(inStr,visitExpr(ctx.expr(0)),tableExpr)
    }
    /**
     * IN (1,2,3)
     */
    else {
      val exprs:Seq[Expr] = if (ctx.expr.size() > 1) {
        toSeq(ctx.expr().subList(1, ctx.expr.size())).map(visitExpr).toIndexedSeq
      } else {
        Seq()
      }
      val subQueryExprs = if (ctx.select_stmt() != null) {
        Seq(SubQueryExpr(visitSelect_stmt(ctx.select_stmt())))
      } else {
        Seq()
      }
      TupleExpr(exprs++subQueryExprs)
    }
    BinaryExpr(inStr,visitExpr(ctx.expr(0)),inClause)
  }

  override def visitExistsExpr(ctx: ExistsExprContext): Any = {
    throw new SqlParserException("暂不支持EXISTS")
  }

  override def visitCaseExpr(ctx: CaseExprContext): Any = {
    val caseExpr = if(ctx.caseExpr!=null){Option(visitExpr(ctx.caseExpr))}else{Option.empty}
    val elseExpr = if(ctx.elseExpr!=null){Option(visitExpr(ctx.elseExpr))}else{Option.empty}
    val whenExprs = toSeq(ctx.whenExpr).map(visitExpr).toIndexedSeq
    val thenExprs = toSeq(ctx.thenExpr).map(visitExpr).toIndexedSeq
    val whens = whenExprs.zip(thenExprs)
    CaseExpr(caseExpr, whens.map(w=>WhenExpr(w._1,w._2)), elseExpr)
  }

  override def visitRaiseFunctionExpr(ctx: RaiseFunctionExprContext): Any = {
    throw new SqlParserException("暂不支持RAISE(傻逼考勤系统坏了，不想加班了，赣)")
  }

  def visitTableOrSubQuery(ctx: Table_or_subqueryContext): Table = {
    ctx match {
      case simpleTable: IndexedByTableContext =>
        visitIndexedByTable(simpleTable)
      case subQuery: SubQueryContext =>
        visitSubQuery(subQuery)
      case joinTable: JoinTablesContext =>
        visitJoinTables(joinTable)
    }
  }

  override def visitTable_function_name(ctx: SQLiteParser.Table_function_nameContext): Unit = {
    throw new SqlParserException("不支持表名函数解析")
  }

  override def visitIndexedByTable(ctx: SQLiteParser.IndexedByTableContext): Table = {
    val schemaName = getText(ctx.schema_name())
    val tableName = getText(ctx.table_name())
    val tableAlias = getText(ctx.table_alias())
    SimpleTable(Option(tableAlias),tableName)
  }

  override def visitJoinTables(ctx: SQLiteParser.JoinTablesContext): Table = {

    val dotTables = toSeq(ctx.table_or_subquery()).map(visitTableOrSubQuery).toIndexedSeq
    val joinTables = visitJoin_clause(ctx.join_clause())
    JoinTable(None,dotTables++joinTables)
  }

  override def visitJoin_clause(ctx: Join_clauseContext): Seq[Table] = {
    toSeq(ctx.table_or_subquery()).map(visitTableOrSubQuery).toIndexedSeq
  }

  override def visitSubQuery(ctx: SQLiteParser.SubQueryContext): Table = {
    val result = visitSelect_stmt(ctx.select_stmt())

    SubQuery(Option(ctx.table_alias().getText),Seq(result.table),result)
  }
}
