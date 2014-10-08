package simplistic

import org.scalatest.BeforeAndAfterEach
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Suite
import fakesdb.Jetty
import simplistic._
import com.amazonaws.services.simpledb._
import com.amazonaws.auth.BasicAWSCredentials

object TestUtil {
  val jetty = Jetty.apply(8181)

  val creds = new BasicAWSCredentials("accessKey", "secretKey")
  val sdb = new AmazonSimpleDBClient(creds)
  sdb.setEndpoint("http://localhost:8181/")

  val account = new SimpleAPI(sdb)

  def flush() {
    account.domain("_flush").create
  }

  trait CleanBefore extends BeforeAndAfterEach { self: Suite =>
    override def beforeEach() {
      flush()
    }
  }

  trait StopAndStartServer extends BeforeAndAfterAll { self: Suite =>
    override def beforeAll() {
      jetty.server.start()
    }
    override def afterAll() {
      jetty.server.stop()
    }
  }
}

