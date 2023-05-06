package cn.geterror.sqlite.exception

class SqlParserException (message: String, cause: Throwable) extends Exception(message, cause) {
  def this(msg:String){
    this(msg,null)
  }
}
