// Copyright 2008 Robin Barooah
// Copyright 2011-14 Alex Boisvert
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package simplistic

import Request.{AttributeOperation, AddValue, ReplaceValue}


import com.amazonaws.services.simpledb.AmazonSimpleDB
import com.amazonaws.services.simpledb.model._
import com.amazonaws.services.simpledb

import scala.collection._
import scala.collection.JavaConverters._
import scala.collection.MapProxy

/**
 * A map implementation which holds a snapshot of the properties and values of an item.
 * An item object which can be used for updates or to make further queries can be accessed
 * via the 'item' field.
 */
class ItemSnapshot(val item: Item, override val self: Map[String,Set[String]])
  extends MapProxy[String,Set[String]] with BatchOperations
{

  def this(item: Item, attrs: java.util.List[Attribute]) = {
    this(item, Utils.attrsToMap(attrs))
  }

  override def batch = item.batch

  /** Friendly toString for diagnostics  */
  override def toString = "ItemSnapshot(" + item + ", " + self + ")"
}

/**
 * A map implementation which holds a snapshot of the properties and values of an item
 * The 'name' field contains the name of the item.
 */
class ItemNameSnapshot(val name: String, override val self: Map[String,Set[String]])
  extends MapProxy[String,Set[String]] with BatchOperations
{
  override lazy val batch = new Batch(name)

  def this(name: String, attrs: java.util.List[Attribute]) = {
    this(name, Utils.attrsToMap(attrs))
  }

  /*** Friendly toString for diagnostics  */
  override def toString = "ItemSnapshot(" + name + ", " + self + ")"
}

/**
 * A class which serves as a proxy to a Domain within simpleDB.  This class holds no data
 * other than a reference to the domain name.  Calls to methods which access items from
 * within the domain will always result in network requests to SimpleDB.
 */
class Domain(val name: String)(implicit val sdb: AmazonSimpleDB) {
  lazy val api = new SimpleAPI(sdb)

  /**
   * Return a current snapshot of the metadata associated with this domain.
   *
   * This is the analog of the 'DomainMetadata' request.
   */
  def metadata: DomainMetadataResult = {
    sdb.domainMetadata(new DomainMetadataRequest().withDomainName(name))
  }

  /**
   * Delete this domain from the SimpleDB account.
   *
   * This is the analog of the 'DeleteDomain' request.
   */
  def delete() = sdb.deleteDomain(new DeleteDomainRequest(name))

  /**
   * Create a domain within SimpleDB corresponding to this object if one doesn't exist
   * already.
   *
   * This is the analog of the 'CreateDomain'
   */
  def create() = sdb.createDomain(new CreateDomainRequest(name))

  /** Return a reference to a theoretically unique item with a new UUID as it's name. */
  def unique = new Item(this, java.util.UUID.randomUUID.toString)

  /** Return a reference to an item with a given name within this domain. */
  def item(name: String) = new Item(this, name)

  /** Return a reference to an item given an ItemNameSnapshot (as returned from select) */
  def item(snapshot: ItemNameSnapshot) = new Item(this, snapshot.name)

  /**
   * Return an iterator containing all of the items within the domain.  One simpleDB request
   * will be performed initially, and subsequent queries will be performed as the iterator
   * is read if they are needed.  This query does not obtain any of the attributes but
   * returns Item objects that you can use to retrieve the attributes you desire.
   *
   * This the exact analog of using the 'Query' request without specifying a query
   * expression.
   */
  def items(implicit consistency: Consistency): Iterator[ItemSnapshot] = {
    api.select("itemName() from `%s`".format(name), this)
  }

  /**
   * Return an iterator containing all of the items within the domain with all of their
   * attributes.  As with most of the queries that return multiple results, an iterator
   * is returned and additional requests are made to SimpleDB only when needed.
   *
   * This is the analog of using the 'QueryWithAttributes' request without specifying a
   * query expression.
   */
  def itemsWithAttributes(implicit consistency: Consistency): Iterator[ItemSnapshot] = withAttributes(Set[String]())

  /**
   * Return an iterator containing the items matching a given query with all of their
   * attributes.  As with most of the queries that return multiple results, an iterator
   * is returned and additional requests are made to SimpleDB only when needed.
   *
   * This is the analog of using the 'QueryWithAttributes' request with a query expression
   * but no list of attributes.
   */
  def withAttributes(expression: String)(implicit consistency: Consistency): Iterator[ItemSnapshot] =
    withAttributes(Some(expression), Set[String]())

  /**
   * Return an iterator containing all of the items within a domain with a selected set of
   * their attributes.  As with most of the queries that return multiple results, an
   * iterator is returned and additional requests are made to SimpleDB only when needed.
   *
   * This is the analog of using the 'QueryWithAttributes' request without a query
   * expression but with a list of attributes.
   */
  def withAttributes(attributes: Set[String])(implicit consistency: Consistency): Iterator[ItemSnapshot] =
    withAttributes(None, attributes)

 /**
  * Return an iterator containing the items matching a given query with a selected set of
  * their attributes.  As with most of the queries that return multiple results, an
  * iterator is returned and additional requests are made to SimpleDB only when needed.
  *
  * This is the analog of using the 'QueryWithAttributes' request with a query
  * expression and a list of attributes.
  */
 def withAttributes(expression: String, attributes: Set[String])(implicit consistency: Consistency): Iterator[ItemSnapshot] =
   withAttributes(Some(expression), attributes)


 /**
  * Return an iterator containing the items matching an optional query with a selected set
  * of their attributes.  As with most of the queries that return multiple results, an
  * iterator is returned and additional requests are made to SimpleDB only when
  * needed.  If 'None' is supplied instead of the query string, all items are returned.
  *
  * This is the analog of using the 'QueryWithAttributes' request.
  */
 def withAttributes(expression: Option[String], attributes: Set[String])(implicit consistency: Consistency): Iterator[ItemSnapshot] = {
    expression match {
      case None => api.select("* from `%s`".format(name), this)
      case Some(where) => api.select("* from `%s` where %s".format(name, where), this)
    }
  }

  /**
   * Perform a batch of attribute modifications on multiple items within the same domain in
   * one operation.
   */
  def apply(batch: List[AttributeOperation]*) = {
    // combine the attributes into a single operation.
    val operations = (List[AttributeOperation]() /: batch) (_ ++ _) map {
      case add: AddValue =>
        new ReplaceableItem(add.item)
          .withAttributes(new ReplaceableAttribute(add.name, add.value, /* replace */ false))

      case replace: ReplaceValue =>
        new ReplaceableItem(replace.item)
          .withAttributes(new ReplaceableAttribute(replace.name, replace.value, /* replace */ true))

    }
    sdb.batchPutAttributes(new BatchPutAttributesRequest(name, operations.asJava))
  }

  override def toString = name
}

/**
 * A trait that defines batch operations for updating attributes on more than one item at a time.
 */
trait BatchOperations {
  /** Return an object that can be used to create batch operations. */
  def batch: Batch
}

/**
 * Class for creating batch operations that work on a particular item.
 */
class Batch(val itemName: String) {
  /** Add values to one or more attributes. */
  def +=(pairs: (String, String)*): List[AttributeOperation] =
    (for (pair <- pairs) yield { AddValue(itemName, pair._1, pair._2) }).toList

  /**
   * Set the value of one or more attributes.
   */
  def set(pairs: (String, String)*): List[AttributeOperation] =
    (for (pair <- pairs) yield { ReplaceValue(itemName, pair._1, pair._2) }).toList
}

/**
 * A class which serves as a proxy to an Item within simpleDB.  This class holds none of the
 * attributes of the item itself.  Calls to methods which read or write attributes to and
 * from the item will result in network requests to SimpleDB.
 */
class Item(val domain: Domain, val name: String)(implicit val sdb: AmazonSimpleDB)
  extends BatchOperations
{
  import PutConditions._
  import Attributes.Attribute

  /** Return a string associating this item with it's domain in the form "domain.item" */
  def path = domain + "." + name

  override def toString = name

  /** Read all of the attributes from this item. */
  def attributes(implicit consistency: Consistency) = {
    val attributes = sdb.getAttributes(
      new GetAttributesRequest(domain.name, name)
        .withConsistentRead(consistency.isConsistent)
      ).getAttributes()
    new ItemSnapshot(this, attributes)
  }

  def attributesOption(implicit consistency: Consistency) = {
    val attrs = attributes
    if (attrs.isEmpty) None else Some(attrs)
  }

  /** Read a selection of attributes from this item */
  def attributes(attributes: Set[String])(implicit consistency: Consistency) = {
    val attrs = sdb.getAttributes(
      new GetAttributesRequest(domain.name, name)
        .withConsistentRead(consistency.isConsistent)
        .withAttributeNames(attributes.toArray: _*)
      ).getAttributes()
    new ItemSnapshot(this, attrs)
  }

  /** Read a single attribute from this item. */
  def attribute(attributeName: String)(implicit consistency: Consistency): Set[String] = {
    val attrs = sdb.getAttributes(
      new GetAttributesRequest(domain.name, name)
        .withConsistentRead(consistency.isConsistent)
        .withAttributeNames(Array(attributeName): _*)
      ).getAttributes()
    attrs.asScala map (_.getValue) toSet
  }

  def attribute(attribute: Attribute[_])(implicit consistency: Consistency): Set[String] =
    this.attribute(attribute.name)

  private def putAttribute(pair: (String, String), replace: Boolean) = {
    sdb.putAttributes(new PutAttributesRequest()
      .withDomainName(domain.name)
      .withItemName(name)
      .withAttributes(new ReplaceableAttribute(pair._1, pair._2, replace))
    )
  }

  private def putAttribute(attributeName: String, values: Set[String], replace: Boolean) = {
    val attrs = values.toSeq.zipWithIndex map { case (value, index) =>
      new ReplaceableAttribute(attributeName, value, replace && index == 0)
    }
    sdb.putAttributes(new PutAttributesRequest()
      .withDomainName(domain.name)
      .withItemName(name)
      .withAttributes(attrs: _*)
    )
  }

  /**
   * Update the contents of this item with a map of attributes names and a set of values
   * and boolean for each one.  If the boolean is true, the existing values will be
   * replace with those in the set, otherwise the set of values will be added to the
   * existing ones.
   *
   * This is the analog of the 'PutAttributes' request.
   */
  def update(values: Map[String, (Set[String], Boolean)], conditional: PutCondition = NoCondition) = {
    val attrs = values.toSeq flatMap { case (attributeName, (values, replace)) =>
      values.toSeq.zipWithIndex map { case (value, index) =>
        new ReplaceableAttribute(attributeName, value, replace && index == 0)
      }
    }
    val condition = conditional match {
      case NoCondition => null
      case DoesNotExist(name)  => new UpdateCondition(name, null,  /* exists */ false)
      case Equals(name, value) => new UpdateCondition(name, value, /* exists */ true)
    }
    sdb.putAttributes(new PutAttributesRequest()
      .withDomainName(domain.name)
      .withItemName(name)
      .withAttributes(attrs: _*)
      .withExpected(condition)
    )

  }

  /** Add a single value to an attribute of this item. */
  def +=(pair: (String, String)) = putAttribute(pair, false)

  /** Add multiple values to this attribute by specifying a series of mappings. */
  def +=(pairs: (String, String)*) = update(combinePairs(false, pairs))

  def +=?(condition: PutCondition)(pairs: (String, String)*) = update(combinePairs(false, pairs), condition)

  /** Add multiple values to this attribute by specifying a sequence of mappings. */
  def addSeq(pairs: Seq[(String, String)]) = update(combinePairs(false, pairs))

  private def combinePairs(replace: Boolean, pairs: Seq[(String, String)]): Map[String, (Set[String], Boolean)] = {
    def combine(map: Map[String,(Set[String], Boolean)], pair: (String, String)): Map[String,(Set[String], Boolean)] =
      if (pair._2 != null) {
        if (map.contains(pair._1))
          map ++ Map(pair._1 -> (map(pair._1)._1 + pair._2 -> replace))
        else
          map ++ Map(pair._1 -> (Set[String](pair._2) -> replace))
      } else map

    (Map[String,(Set[String], Boolean)]() /: pairs) (combine(_, _))
  }

  /** Add multiple values to an attribute of this item. */
  def +=(name: String, values: Set[String]) = putAttribute(name, values, false)

  /**
   * Replace the value of an attribute in this item with a single value.  Any previously
   * existing values for this item will be deleted.
   */
  def set(pair: (String,String)) = putAttribute(pair, true)

  /**
   * Replace the value of an attribute in this item with a set of values.  Any previously
   * existing values for this item will be deleted.
   */
  def set(name: String, values: Set[String]) = putAttribute(name, values, true)

  /**
   * Replace the values of multiple attributes in this item with a series of mappings.
   */
  def set(pairs: (String,String)*) = update(combinePairs(true, pairs))

  /**
   * Conditionally replace the values of multiple attributes in this item with a series of mappings.
   */
  def setIf(condition: PutCondition)(pairs: (String,String)*) = update(combinePairs(true, pairs), condition)

  /**
   * Replace the values of multiple attributes in this item with a series of mappings.
   */
  def setSeq(pairs: Seq[(String, String)]) = update(combinePairs(true, pairs))

  /** Delete all of the attributes in this item. */
  def clear() = {
    sdb.deleteAttributes(new DeleteAttributesRequest(domain.name, name))
  }

  /** Delete the item -- equivalent to clear() */
  def delete() = clear()

  /** Delete a single attribute value pair in this item. */
  def -=(pair: (String, String)) = {
    sdb.deleteAttributes(new DeleteAttributesRequest(domain.name, name).withAttributes(new simpledb.model.Attribute(pair._1, pair._2)))
  }

  /** Delete a single attribute in this item. */
  def -=(attributeName: String): Unit = {
    sdb.deleteAttributes(new DeleteAttributesRequest(domain.name, name).withAttributes(new simpledb.model.Attribute(attributeName, null)))
  }

  def -=(attribute: Attribute[_]): Unit = {
    this.-=(attribute.name)
  }

  /** Supply an object that can be used to create batch operations. */
  lazy val batch = new Batch(name)
}

object PutConditions {
  sealed trait PutCondition
  case object NoCondition extends PutCondition /* default */
  case class DoesNotExist(name: String) extends PutCondition
  case class Equals(name: String, value: String) extends PutCondition
}

/**
 * This trait provides a simple API for accessing Amazon SimpleDB.  It is designed to be
 * mixed in to objects or classes that provide the necessary networking capabilities.
 *
 * @see SimpleDBAccount for a simple and complete implementation.
 */
class SimpleAPI(val sdb: AmazonSimpleDB)
{
  private[this] implicit val _ = sdb

  /**
   * Perform a select operation associated with a known domain and return an iterator of results.
   * A single request is made initially, and additional requests are made as needed when the
   * iterator is read.
   */
  def select(expression: String, domain: Domain)(implicit consistency: Consistency): Iterator[ItemSnapshot] =
    select[ItemSnapshot](i => new ItemSnapshot(domain.item(i.getName), i.getAttributes), expression)

  /**
   * Perform a select operation and return an iterator of results.  The results are simple
   * maps of attributes names to sets of values.  A single request is made initially, and
   * additional requests are made as needed when the iterator is read.
   */
  def select(expression: String)(implicit consistency: Consistency): Iterator[ItemNameSnapshot] =
    select[ItemNameSnapshot](i => new ItemNameSnapshot(i.getName, i.getAttributes), expression)

  /**
   * Perform a select operation and return an iterator of results.  The results are item objects
   * which contain no attributes.  A single request is made initially and additional requests
   * are made as needed when the iterator is read.
   */
  def items(expression: String, domain: Domain)(implicit consistency: Consistency): Iterator[Item] =
    select[Item](i => domain.item(i.getName), expression)

  /**
   * Perform a select operation and return an iterator of results.  Convert the results using
   * the supplied function. A single request is made initially, and additional requests are
   * made as needed when the stream is read.
   */
  private def select[T](convert: (simpledb.model.Item) => T, expression: String)(implicit consistency: Consistency): Iterator[T] = {
    Utils.tokenIterator { nextToken =>
      val request = new SelectRequest("select " + expression, consistency.isConsistent)
      if (nextToken.isDefined) {
        request.withNextToken(nextToken.get)
      }
      val response = sdb.select(request)
      (response.getItems, response.getNextToken)
    } map convert
  }

  /**
   * Return a proxy object representing the named simpleDB domain.  No request is made
   * and the domain may or may not exist on the server. A domain may be created on the server
   * using the 'create' method, or deleted using the 'delete' method.
   */
  def domain(name: String) = new Domain(name)

  /**
   * Return an iterator of all of the domains within the simpleDB account.  As usual this stream
   * is fetched lazily, and additional requests to simpleDB will be made only when needed.
   *
   * The stream consists of proxy objects that can be used to make further requests.
   */
  def domains: Iterator[Domain] = {
    sdb.listDomains().getDomainNames.asScala.iterator map { name => new Domain(name) }
  }

}
