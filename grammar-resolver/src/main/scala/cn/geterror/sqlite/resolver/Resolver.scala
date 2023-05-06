package cn.geterror.sqlite.resolver

import cn.geterror.sqlite.analyzer.TableAnalyzer
import cn.geterror.sqlite.parser._
import cn.geterror.sqlite.result.{FactTable, ParserResult, UnresolvedFieldProjection, UnresolvedProjection}
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.atn.PredictionMode

object Resolver {

  val queryBuilder:SqlitePlanBuilder = new SqlitePlanBuilder
  val tableBuilder:SqliteTableBuilder = new SqliteTableBuilder
  def resolve(querySql:String,tableSql:Seq[String]): ParserResult = {
    val query = parseQuery(querySql)
    val tables = tableSql.map(parseTable).toIndexedSeq

    val analyzer = new TableAnalyzer(tables)
    val result = analyzer.resolvedParseResult(query)
    result
  }

  def parseQuery(querySql:String): ParserResult = handleCommand(querySql){ parser=>
    queryBuilder.visitParse(parser.parse())
  }

  def parseTable(querySql: String): FactTable = handleCommand(querySql) { parser =>
    tableBuilder.visitParse(parser.parse())
  }

  def handleCommand[T](querySql:String)(toResult:SQLiteParser=>T): T = {
    val stream = new NoCaseStringStream(querySql)
    val lexer = new SQLiteLexer(stream)
    lexer.removeErrorListeners()

    val tokenStream = new CommonTokenStream(lexer)
    val parser = new SQLiteParser(tokenStream)
    parser.getInterpreter.setPredictionMode(PredictionMode.LL_EXACT_AMBIG_DETECTION)
    toResult(parser)
  }

  def main(args: Array[String]): Unit = {
    val result = resolve("select * from t", Seq("create table t(\nnum int(13),\nname varchar(255),\nage int(3),\nsex char(1),\nphonenumber char(11),\nemail varchar(255)\n);"))
    return
  }
}
