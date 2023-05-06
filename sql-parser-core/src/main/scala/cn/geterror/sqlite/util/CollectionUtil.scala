package cn.geterror.sqlite.util

import scala.collection.JavaConverters
object CollectionUtil {
  def toSeq[T](list: java.util.List[T]): Seq[T] = {
    if (list == null) {
      null
    } else {
      JavaConverters.asScalaIteratorConverter(list.iterator).asScala.toSeq
    }
  }

}
