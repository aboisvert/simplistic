package simplistic

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class DomainSuite extends WordSpec with ShouldMatchers with TestUtil.CleanBefore with TestUtil.StopAndStartServer {
  import TestUtil._

  val testDomain = account.domain("test")

  "Domain.create" should {

    "create a domain" in {
      testDomain.create()
      account.domains.map(_.name).to[Seq] should be === Seq("test")
    }

    "be idempotent" in {
      testDomain.create()
      testDomain.create()
      account.domains.map(_.name).to[Seq] should be === Seq("test")
    }

    "create different domains" in {
      account.domain("foo").create()
      account.domain("bar").create()
      account.domains.map(_.name).toSet should be === Set("foo", "bar")
    }

  }

  "Domain.delete" should {

    "delete a domain" in {
      testDomain.create()
      account.domains.map(_.name).toSet should be === Set("test")
      testDomain.delete()
      account.domains.map(_.name).toSet should be === Set.empty
    }
  }

  "Domain.unique" should {
    "return unique item ids" in {
      val u1 = testDomain.unique
      val u2 = testDomain.unique
      u1.name should not be === (u2.name)
    }
  }
}
