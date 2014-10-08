package simplistic

import scala.collection._
import scala.collection.JavaConverters._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.amazonaws.services.simpledb.model.Attribute

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class UtilsSuite extends WordSpec with ShouldMatchers {

  import Utils._

  "Utils.attrsToMap" should {

    "Convert attributes to Map[String, Set[String]]" in {
      attrsToMap(javaList(new Attribute("foo", "bar"))) should be === Map("foo" -> Set("bar"))

      attrsToMap(javaList(
        new Attribute("foo1", "bar1"),
        new Attribute("foo2", "bar2")
      )) should be === Map(
        "foo1" -> Set("bar1"),
        "foo2" -> Set("bar2")
      )

      attrsToMap(javaList(
        new Attribute("foo1", "bar1"),
        new Attribute("foo2", "bar2"),
        new Attribute("foo2", "bar3"),
        new Attribute("foo1", "bar4")
      )) should be === Map(
        "foo1" -> Set("bar1", "bar4"),
        "foo2" -> Set("bar2", "bar3")
      )
    }
  }


  "Utils.tokenIterator" should {

    "pass None as initial token" in {
      val iter = tokenIterator { token =>
        token should be === None
        (null, null)
      }
      iter.to[Seq] should be === Seq.empty
    }

    "pass back token after first loop" in {
      var loops = 0
      val iter = tokenIterator { token =>
        token match {
          case None        => (javaList(1,2), "one")
          case Some("one") => (javaList(3), null)
        }
      }
      iter.to[Seq] should be === Seq(1,2,3)
    }

    "flatten the results" in {
      val iter = tokenIterator[Int] { token =>
        token match {
          case None          => (javaList(1,2),   "one")
          case Some("one")   => (javaList(3,4,5), "two")
          case Some("two")   => (javaList(),      "three")
          case Some("three") => (javaList(6),     null)
        }
      }
      iter.to[Seq] should be === Seq(1,2,3,4,5,6)
    }
  }

  def javaList[T](ts: T*) = ts.asJava
}
