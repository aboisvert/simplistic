package simplistic

import java.util.Date

/** Serialization to/from String for SimpleDB data types */
object Conversions {

  trait Conversion[T] {
    def apply(t: T): String
    def unapply(s: String): Option[T]
  }

  object PassThrough extends Conversion[String] {
    override def apply(value: String) = value
    override def unapply(value: String) = Some(value)
  }

  object PositiveInt extends Conversion[Int] {
    import java.text.DecimalFormat

    private val format = new DecimalFormat("0" * Integer.MAX_VALUE.toString.length)

    override def apply(number: Int) = {
      if (number >= 0) format.format(number)
      else throw new IllegalArgumentException("field can only be positive but was "+number)
    }

    override def unapply(string: String): Option[Int] = {
      val number = format.parse(string)
      if (number == null) None
      else Some(number.intValue)
    }
  }

  object IntConversion extends Conversion[Int] {
    override def apply(number: Int) = number.toString
    override def unapply(string: String): Option[Int] = string match {
      case null => None
      case "" => None
      case intString => Some(java.lang.Integer.parseInt(intString))
    }
  }

  object LongConversion extends Conversion[Long] {
    override def apply(number: Long) = number.toString
    override def unapply(string: String): Option[Long] = string match {
      case null => None
      case "" => None
      case intString => Some(java.lang.Long.parseLong(intString))
    }
  }
  
  /**
   * Conversion for dates using a lexicographically comparable format specified by ISO8601.
   * @see http://en.wikipedia.org/wiki/ISO_8601
   * @see http://www.iso.org/iso/catalogue_detail?csnumber=40874
   */
  object ISO8601Date extends Conversion[Date] {
    import java.text.SimpleDateFormat
    import java.util.TimeZone

    def format = {
      val f = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
      f.setTimeZone(TimeZone.getTimeZone("UTC"))
      f
    }

    override def apply(date:  Date): String = format.format(date)
    override def unapply(string: String): Option[Date] = Option(format.parse(string))
  }

  object SHA1Base64 extends Conversion[String] {
    import java.security.MessageDigest
    import org.apache.commons.codec.binary.Base64

    private def base64Encode(bytes: Array[Byte]) = (new Base64).encode(bytes).toString
    private def digest(in: String): Array[Byte] = MessageDigest.getInstance("SHA-1").digest(in.getBytes)

    override def apply(in: String): String = base64Encode(digest(in))
    override def unapply(in: String) = None
  }
}
