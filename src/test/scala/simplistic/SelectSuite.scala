package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SelectSuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  val testDomain = account.domain("test")

  implicit val consistency = UnspecifiedConsistency

  override def beforeEach() {
    super.beforeEach()
    testDomain.create()
  }

  "Domain.first" should {
    import Attributes._
    import Select._

    val attr = attribute("test")
    def addItem(s: String) { testDomain.unique += attr(s) }
    def first(s: String) = testDomain first (attr is s)

    "return Some(firstItem) when such item exists" in {
      addItem("foo")
      first("foo").isDefined should be === true
    }

    "return None when no such item exists" in {
      addItem("foo")
      first("bar").isDefined should be === false
    }

    "return Some(firstItem) when multiple such items exists" in {
      addItem("foo")
      addItem("foo")
      first("foo").isDefined should be === true
    }
  }


  "Domain.select" should {
    import Select._
    import TestDomainData._

    "simple select expression" in {
      setupData(testDomain)
      (testDomain select (visits > 16) map (user(_))).toSet should be === Set("jon", "jack")
    }

    "range select expression" in {
      setupData(testDomain)
      (testDomain select (visits > 16 and visits < 50) map (user(_))).next should be === "jon"
    }

    "range select with sorting" in {
      setupData(testDomain)
      (testDomain select ((visits > 1) and (visits < 50) orderBy visits desc) map (user(_))).toList should be === List("jon", "alice", "robin")
    }
  }

  "Domain.items" should {
    import Attributes._

    val attr = attribute("test")
    def addItem(s: String) { testDomain.unique += attr(s) }

    "return all items" in {
      addItem("a")
      addItem("b")
      addItem("c")

      val items = testDomain.items map { item => attr(item.attributes)}
      items.toSet should be === Set("a", "b", "c")
    }
  }
}

object TestDomainData {
  import Attributes._
  import Conversions._

  val user = attribute("user")
  val startDate = attribute("startDate", ISO8601Date)
  val visits = attribute("visits", PositiveInt)
  val tags = attribute("tags")

  def setupData(d: Domain) = {
    d.unique += (user("robin"), startDate(new java.util.Date()), visits(3))
    d.unique += (user("jon"), startDate(new java.util.Date()), visits(20))
    d.unique += (user("alice"), startDate(new java.util.Date()), visits(15))
    d.unique += (user("jack"), startDate(new java.util.Date()), visits(100))
  }
}
