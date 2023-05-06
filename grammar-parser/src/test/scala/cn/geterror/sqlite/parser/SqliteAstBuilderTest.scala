package cn.geterror.sqlite.parser

import cn.geterror.sqlite.parser.SqliteAstBuilderTest.parser
import cn.geterror.sqlite.result.ParserResult
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

object SqliteAstBuilderTest extends App{
  private val builder = new SqlitePlanBuilder
  private val stream = new NoCaseStringStream("SELECT E+A.f  FROM A AS B JOIN C WHERE A.B = B.C")
  val lexer = new SQLiteLexer(stream)
  lexer.removeErrorListeners()

  val tokenStream = new CommonTokenStream(lexer)
  val parser = new SQLiteParser(tokenStream)
  parser.getInterpreter.setPredictionMode(PredictionMode.SLL)
  val result: ParserResult = builder.visitParse(parser.parse())
  result
}
