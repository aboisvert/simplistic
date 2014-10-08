package simplistic

import scala.collection._
import scala.collection.JavaConverters._
import com.amazonaws.services.simpledb.model.Attribute

object Utils {

  type Token = String

  /** Convert a list of Attributes into a Map[String, Set[String] */
  implicit def attrsToMap(attrs: java.util.List[Attribute]): Map[String, Set[String]] = {
    val map = new mutable.HashMap[String, mutable.Set[String]]()
    val iter = attrs.iterator
    while (iter.hasNext) {
      val e = iter.next()
      val set = map.getOrElseUpdate(e.getName, new mutable.HashSet())
      set += e.getValue
    }
    map
  }

  /** Generate an iterator from a token-based request generator */
  def tokenIterator[T](nextPage: Option[Token] => (java.util.List[T], Token)): Iterator[T] = {
    var nextToken: Option[Token] = None
    val iter = Iterator.continually {
      if (nextToken != Some(null)) {
        val (items, token) = nextPage(nextToken)
        nextToken = Some(token)
        items
      } else null
    } takeWhile { _ != null }
    iter flatMap { _.asScala }
  }

}