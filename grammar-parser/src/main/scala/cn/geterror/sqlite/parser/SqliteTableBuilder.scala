package cn.geterror.sqlite.parser

import cn.geterror.sqlite.exception.SqlParserException
import cn.geterror.sqlite.parser.SQLiteParser.{ParseContext, Sql_stmtContext}
import cn.geterror.sqlite.result.{FactFieldProjection, FactTable, Field, ParserResult, Projection}
import cn.geterror.sqlite.util.CollectionUtil.toSeq

class SqliteTableBuilder extends SQLiteParserBaseVisitor[Any]{
  override def visitParse(ctx: ParseContext): FactTable = {
    val sqlCtxList = ctx.sql_stmt_list
    if(sqlCtxList==null||sqlCtxList.size()!=1){
      throw new SqlParserException("仅解析单条SQL")
    }
    visitSql_stmt(sqlCtxList.get(0).sql_stmt(0))
  }

  override def visitSql_stmt(ctx: Sql_stmtContext): FactTable = {
    if(ctx.create_table_stmt()==null){
      throw new SqlParserException("仅解析建表语句")
    }
    visitCreate_table_stmt(ctx.create_table_stmt())
  }

  override def visitCreate_table_stmt(ctx: SQLiteParser.Create_table_stmtContext): FactTable = {
    val schema = Option(ctx.schema_name()).flatMap(x=>Option(x.getText))
    val fields = toSeq(ctx.column_def()).map(visitColumn_def).toIndexedSeq
    val tableName = ctx.table_name().getText
    val projections = fields.map(i => FactFieldProjection(tableName, i.name, i.fieldType))
    val table = FactTable(schema, tableName, projections)

    table

  }

  override def visitColumn_def(ctx: SQLiteParser.Column_defContext): Field = {
    val name = ctx.column_name().getText
    val dataType = ctx.type_name().getText
    Field(name, dataType)
  }
}
