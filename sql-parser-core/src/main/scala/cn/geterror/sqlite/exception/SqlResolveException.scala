package cn.geterror.sqlite.exception

class SqlResolveException (message: String, cause: Throwable) extends Exception(message, cause) {
  def this(msg:String){
    this(msg,null)
  }
}
