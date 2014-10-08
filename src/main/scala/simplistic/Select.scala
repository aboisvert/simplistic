package simplistic

import Attributes._
import Conversions._
import Quoting._

object Select {

  trait FromExpression {
    def queryString: String
  }

  trait LimitableExpression extends FromExpression {
    def limit(n: Int) = LimitedExpression(this, n)
  }

  case class LimitedExpression(e: LimitableExpression, n: Int) extends FromExpression {
    override def queryString = e.queryString + " limit " + n
  }

  trait SortedExpression extends LimitableExpression {}

  case class DescendingOrder[T](e: Expression, a: Attribute[T]) extends SortedExpression {
    def queryString = e.queryString + " order by " + quoteName(a.name) + " desc"
  }

  case class AscendingOrder[T](e: Expression, a: Attribute[T]) extends SortedExpression {
    def asc = this
    def desc = DescendingOrder(e, a)
    def queryString = e.queryString + " order by " + quoteName(a.name) + " asc"
  }

  trait Expression extends LimitableExpression {
    def orderBy[T](a: Attribute[T])  = AscendingOrder(this, a)
    def queryString: String
  }

  case object EmptyExpression extends Expression {
    def queryString = ""
  }

  case class Intersection(lhs: SelectExpression, rhs: SelectExpression)  extends SelectExpression {
    def queryString = lhs.queryString + " intersection " + rhs.queryString
  }

  case class Or(lhs: SelectExpression, rhs: SelectExpression) extends SelectExpression {
    def queryString = lhs.queryString + " or " + rhs.queryString
  }

  case class And(lhs: SelectExpression, rhs: SelectExpression) extends SelectExpression {
    def queryString = lhs.queryString + " and " + rhs.queryString
  }

  trait SelectExpression extends Expression {
    def intersection(e: SelectExpression) = Intersection(this, e)
    def or(e: SelectExpression) = Or(this, e)
    def and(e: SelectExpression) = And(this, e)
  }

  case class Not(e: SelectExpression) extends Expression {
    def queryString = "not " + e.queryString
  }

  def not(e: SelectExpression) = Not(e)

  trait Comparison extends SelectExpression

  case class BasicComparison[T](op: String, c: Comparable[T], value: T) extends Comparison {
    def queryString = quoteName(c.name) + " " + op + " " + quoteValue(c.conversion(value))
  }

  case class StringComparison[T](op: String, c: Comparable[T], value: String) extends Comparison {
    def queryString = quoteName(c.name) + " " + op + " " + quoteValue(value)
  }

  case class Between[T](c: Comparable[T], start: T, finish: T) extends Comparison {
    def queryString = (
      quoteName(c.name) + " between " + quoteValue(c.conversion(start)) + " and " + quoteValue(c.conversion(finish))
    )
  }

  case class BetweenLHS[T](c: Comparable[T], start: T) {
    def and(finish: T) = Between(c, start,finish)
  }

  case class In[T](c: Comparable[T], values: T*) extends Comparison {
    def terms = values map(t => quoteValue(c.conversion(t))) mkString ", "
    def queryString = "in(" + terms + ")"
  }

  case class Unary[T](op: String, c: Comparable[T]) extends Comparison {
    def queryString = quoteName(c.name) + " " + op
  }

  trait Comparable[T] {
    // requires
    def conversion: Conversion[T]
    def name: String

    // provides
    def comparison(op: String, value: T) = BasicComparison(op, this, value)
    def ===(value: T) = is(value)
    def is(value: T) = comparison("=", value)
    def !==(value: T) = isNot(value)
    def isNot(value: T) = comparison("!=", value)
    def >(value: T) = comparison(">", value)
    def >=(value: T) = comparison(">=", value)
    def <(value: T) = comparison("<", value)
    def <=(value: T) = comparison("<=", value)
    def like(value: String) = StringComparison("like", this, value)
    def notLike(value: String) = StringComparison("not like", this, value)
    def between(value: T) = BetweenLHS(this, value)
    def in(value: T*) = In[T](this, value: _*)
    def isNull = Unary("is null", this)
    def isNotNull = Unary("is not null", this)
  }

  class SelectAttribute[T](a: Attribute[T]) extends Comparable[T] {
    def conversion = a.conversion
    def name = a.name
  }

  implicit def toSelectAttribute[T](a: Attribute[T]): SelectAttribute[T] = new SelectAttribute(a)

  class Every[T](a: Attribute[T]) extends Comparable[T] {
    def conversion = a.conversion
    def name = "every("+quoteName(a.name)+")"
  }

  def every[T](a: Attribute[T]) = new Every(a)

  implicit def toSelectableDomain(d: Domain): SelectableDomain = new SelectableDomain(d)

  // Building elements of the query syntax
  private[Select] def names(attributes: Attribute[_]*) = (attributes map (_.name) mkString ", ") + " "

  private[Select] def whereClause(e: FromExpression) = "where " + e.queryString

  private[Select] def from(d: Domain) = "from " + quoteName(d.name) + " "

  class SelectableDomain(val d: Domain) {
    val all = "* "

    val itemName = "itemName() "

    def select(a: Attribute[_]*)(e: FromExpression)(implicit consistency: Consistency): Iterator[ItemSnapshot] = {
      d.api.select(names(a: _*) + from(d) + whereClause(e), d)
    }

    def select(e: FromExpression)(implicit consistency: Consistency): Iterator[ItemSnapshot] =
      d.api.select(all + from(d) + whereClause(e), d)

    def first(expression: LimitableExpression)(implicit consistency: Consistency): Option[ItemSnapshot] = {
      val iter = select(expression limit 1)
      if (iter.hasNext) Some(iter.next) else None
    }

    /**
     * Return the integer count of items within the domain that match the supplied expression.
     */
    def count(e: Expression)(implicit consistency: Consistency): Int =
      new CountSource(d.api, d, consistency).where(e)

    /**
     * Return a stream of items matching the supplied expression.  These are items without
     * attributes associated with then.
     */
    def items(e: Expression)(implicit consistency: Consistency): Iterator[Item] =
      d.api.items(itemName + from(d) + whereClause(e), d)

    def count(implicit consistency: Consistency): Int = count(EmptyExpression)
  }

  class SourceSelection(attributes: AttributeSelection, d: Domain, consistency: Consistency) {
    def where(e: FromExpression): Iterator[ItemSnapshot] = {
      d.api.select(attributes.queryString + from(d) + whereClause(e), d)(consistency)
    }
  }

  class CountSource(api: SimpleAPI, d: Domain, consistency: Consistency) {
    private val countValue = multiValued[Int]("Count", PositiveInt)

    val toCount = "count(*) "

    private def values(whereClause: String) =
      d.api.select(toCount + from(d) + whereClause, d)(consistency) flatMap { m => countValue(m) }

    def where(e: Expression): Int = {
      val e2 = e match {
        case EmptyExpression => ""
        case e: Expression => whereClause(e)
      }
      values(e2).sum
    }
  }

  class AttributeSelection(val api: SimpleAPI, val queryString: String, consistency: Consistency) {
    def from(d: Domain) = new SourceSelection(this, d, consistency)
    def from(name: String) = new SourceSelection(this, api.domain(name), consistency)
  }

  class CountSelection(val api: SimpleAPI, consistency: Consistency) {
    def from(d: Domain) = new CountSource(api, d, consistency)
    def from(name: String) = new CountSource(api, api.domain(name), consistency)
  }
}
