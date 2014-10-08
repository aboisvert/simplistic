package simplistic

import Conversions._
import com.amazonaws.services.simpledb
import scala.collection._

object Attributes {

  trait Attribute[T] {
    val name: String
    val conversion: Conversion[T]

    /** When applied to a value, the conversion returns a name value pair of strings */
    def apply(value: T) = (name -> conversion(value))
  }

  /**
   * An attribute is a definition of a named value with a conversion to and from a string
   * representation.  It is useful when working with structures such as http request
   * parameters and simpledb queries.
   */
  case class MultiValuedAttribute[T](
    override val name: String,
    override val conversion: Conversion[T]
  ) extends Attribute[T] {
    /**
     * When applied to a map of names to sets of string values, the conversion returns a
     * list of values retrieved from the set and converted from strings back to their
     * original type.
     */
    def apply(result: scala.collection.Map[String, Set[String]]): List[T] =
      if (! result.contains(name)) List.empty
      else result(name).toList flatMap conversion.unapply

    def apply(result: java.util.List[simpledb.model.Attribute]): List[T] = apply(Utils.attrsToMap(result))

  }

  trait SingleValuedAttribute[T] extends Attribute[T] {
    def apply(expected: Option[T]): (String, Option[String]) = {
      (name -> (expected map conversion.apply))
    }
  }


  case class RequiredSingleValuedAttribute[T](
      override val name: String,
      override val conversion: Conversion[T]
  ) extends SingleValuedAttribute[T] {
    def apply(result: scala.collection.Map[String, Set[String]]): T = {
      if (! result.contains(name)) throw new MissingRequiredAttribute(name)

      result(name) head match {
        case conversion(value) => value
      }
    }
  }

  class MissingRequiredAttribute(val name: String) extends RuntimeException("Missing required attribute: " + name)

  case class OptionalSingleValuedAttribute[T](
      override val name: String,
      override val conversion: Conversion[T]
  ) extends SingleValuedAttribute[T] {
    def apply(result: scala.collection.Map[String, Set[String]]): Option[T] = {
      if (! result.contains(name)) None
      else result(name) head match {
        case conversion(value) => Some(value)
      }
    }
  }

  /** Create a simple attribute which performs no conversion on string values. */
  def multiValued(name: String) = MultiValuedAttribute(name, Conversions.PassThrough)

  /** Create a typed attribute with an associated conversion to and from that type. */
  def multiValued[T](name: String, conversion: Conversion[T]) = MultiValuedAttribute[T](name, conversion)

  /** Create an optional attribute that's not multi-valued */
  def optionalAttribute(name: String) = OptionalSingleValuedAttribute(name, Conversions.PassThrough)
  def optionalAttribute[T](name: String, conversion: Conversion[T]) = OptionalSingleValuedAttribute[T](name, conversion)

  /** Create a non-multi-valued attribute that's required */
  def attribute(name: String) = RequiredSingleValuedAttribute(name, Conversions.PassThrough)
  def attribute[T](name: String, conversion: Conversion[T]) = RequiredSingleValuedAttribute[T](name, conversion)


  trait AttributeToPutConditionBuilder[T] {
    def doesNotExist: PutConditions.DoesNotExist
    def stillEquals(value: T): PutConditions.Equals
  }

  implicit def attributeToPutCondition[T](attr: Attribute[T]) = new AttributeToPutConditionBuilder[T] {
    override def doesNotExist = PutConditions.DoesNotExist(attr.name)
    override def stillEquals(value: T) = PutConditions.Equals(attr.name, attr.conversion(value))
  }
}
