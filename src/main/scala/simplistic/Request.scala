// Copyright 2008 Robin Barooah
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

object Request {

  trait SimpleDBRequest {
    def action: String
    def awsAccessKeyId: String
    def timeStamp: String
    def version: String = "2009-04-15"

    def parameters = Map(
      "Action" -> action,
      "AWSAccessKeyId" -> awsAccessKeyId,
      "Timestamp" -> timeStamp,
      "Version" -> version
    ) ++ specificParameters

    def specificParameters: Map[String, String]
  }

  trait CreateDomain extends SimpleDBRequest {
    def action = "CreateDomain"
    def domainName: String
    def specificParameters = Map("DomainName" -> domainName)
  }

  trait DeleteDomain extends SimpleDBRequest {
    def action = "DeleteDomain"
    def domainName: String
    def specificParameters = Map("DomainName" -> domainName)
  }

  trait ListDomains extends SimpleDBRequest {
    import Attributes._

    def action = "ListDomains"
    def maxNumberOfDomains:Option[Int]
    def nextToken:Option[String]
    def specificParameters = Seq(
      optional("MaxNumberOfDomains", maxNumberOfDomains),
      optional("NextToken", nextToken)
    ).flatten.toMap
  }

  trait DomainMetadata extends SimpleDBRequest {
    def action = "DomainMetadata"
    def domainName: String
    def specificParameters = Map("DomainName" -> domainName)
  }

  /**
   * Trait for constructing a PutAttributes request.
   */
  trait PutAttributes extends SimpleDBRequest {
    import Attributes._

    def action = "PutAttributes"
    def itemName: String
    def attributes: Map[String, (Set[String] , Boolean)]
    def condition: PutConditions.PutCondition
    def domainName: String
    def specificParameters = replacableAttributes ++
      Map("DomainName" -> domainName, "ItemName" -> itemName)

    def replacableAttributes: Map[String,String] = {
      def flattened = attributes flatMap { case (name, (value, replace)) =>
        for (each <- value) yield (name, each, replace)
      }

      def conds = condition match {
        case PutConditions.NoCondition =>
          List.empty
        case PutConditions.DoesNotExist(name) =>
          List(Map("Expected.1.Name" -> name,  "Expected.1.Exists" -> "false"))
        case PutConditions.Equals(name, value) =>
          List(Map("Expected.1.Name" -> name,  "Expected.1.Value" -> value))
      }

      def params: List[Map[String, String]] = {
        (flattened.toList zipWithIndex) map { case ((name, value, replace), pos) =>
          param("Name", pos, name) ++ param("Value", pos, value) ++
            (if (replace) param("Replace", pos, "True") else Map.empty)
        }
      }

      (Map[String, String]() /: (params ++ conds)) (_ ++ _)
    }
  }

  /**
   * Trait for constructing a BatchPutAttributes request.
   */
  trait BatchPutAttributes extends SimpleDBRequest {
    def action = "BatchPutAttributes"
    def domainName: String
    def operations: List[AttributeOperation]
    def specificParameters = {
      // collect the operations into a map of lists by item
      var byItem: Map[String, List[AttributeOperation]] = Map.empty
      for (op <- operations) {
        byItem += op.item -> ((byItem getOrElse (op.item,  List.empty)) ++ List(op))
      }

      // various parameter name encodings
      def item(itemNumber: Int) = "Item." + itemNumber
      def itemParameter(itemNumber: Int, name: String) = Map(item(itemNumber) + ".ItemName" -> name)
      def attribute(attributeNumber: Int) = ".Attribute." + attributeNumber
      def pair(itemNumber: Int, attributeNumber: Int, op:AttributeOperation) = Map(
        item(itemNumber) + attribute(attributeNumber) + ".Name" -> op.name,
        item(itemNumber) + attribute(attributeNumber) + ".Value" -> op.value
      )
      def replace(itemNumber: Int, pos: Int) = Map(
        item(itemNumber) + attribute(pos) + ".Replace" -> "true"
      )

      def itemOperations(itemNumber: Int, operations: List[AttributeOperation]) = {
        (operations zipWithIndex) flatMap {
          case (a: AddValue, pos) => pair(itemNumber, pos, a)
          case (r: ReplaceValue, pos) => pair(itemNumber, pos, r) ++ replace(itemNumber, pos)
        }
      }

      // Create the final map of parameters.
      Map("DomainName" -> domainName) ++ (
        (byItem.toList zipWithIndex) flatMap { case ((name, operations), itemNumber) =>
          itemParameter(itemNumber, name) ++ itemOperations(itemNumber, operations)
        }
      )
    }
  }

  /**
   * Case classes for the various operations possible on attributes.
   */
  sealed trait AttributeOperation {
    def item: String
    def name: String
    def value: String
  }
  case class AddValue(item: String, name: String, value: String) extends AttributeOperation
  case class ReplaceValue(item: String, name: String, value: String) extends AttributeOperation

  trait DeleteAttributes extends SimpleDBRequest {
    import Attributes._

    def action = "DeleteAttributes"
    def attributes:Map[String, Set[String]]
    def domainName: String
    def itemName: String
    def specificParameters = {
      Map("DomainName" -> domainName, "ItemName" -> itemName) ++ attributeNameValues
    }

    def attributeNameValues :Map[String, String] = {
      def flattened = attributes flatMap { case (name, values) =>
        if (values isEmpty) List((name, None)) else for (value <- values) yield (name, Some(value))
      }

      def numbered = flattened.toList zipWithIndex

      def params = numbered map { case ((name, value), pos) =>
        param ("Name", pos, name) ++ (value match {
          case None => param("Name", pos, name)
          case Some(value) => param("Value", pos, value)
        })
      }

      (Map[String,String]() /: params) (_ ++ _)
    }
  }

  trait GetAttributes extends SimpleDBRequest {
    import Attributes._
    def action = "GetAttributes"
    def itemName: String
    def domainName: String
    def attributes: Set[String]
    def consistency: Consistency

    def specificParameters = {
      attributeNames(attributes) ++ Map(
        "ItemName"       -> itemName,
        "DomainName"     -> domainName,
        "ConsistentRead" -> (if (consistency == ConsistentRead) "true" else "false")
      )
    }
  }

  trait Query extends SimpleDBRequest {
    import Attributes._

    def action = "Query"
    def maxNumberOfItems:Option[Int]
    def nextToken:Option[String]
    def queryExpression:Option[String]
    def domainName: String

    def specificParameters = {
      Map("DomainName" -> domainName) ++
        optional("QueryExpression", queryExpression) ++
        optional("MaxNumberOfItems", maxNumberOfItems) ++
        optional("NextToken", nextToken)
    }
  }

  trait QueryWithAttributes extends SimpleDBRequest {
    import Attributes._

    def action = "QueryWithAttributes"
    def attributes:Set[String]
    def maxNumberOfItems:Option[Int]
    def nextToken:Option[String]
    def queryExpression:Option[String]
    def domainName: String

    def specificParameters = {
      Map[String, String]("DomainName" -> domainName) ++
        attributeNames(attributes) ++
        optional("QueryExpression", queryExpression) ++
        optional("MaxNumberOfItems", maxNumberOfItems) ++
        optional("NextToken", nextToken)
    }
  }

  trait Select extends SimpleDBRequest {
      import Attributes._

      def action = "Select"
      def selectExpression: String
      def consistency: Consistency
      def nextToken: Option[String]
      def maxNumberOfItems: Option[Int]

      def specificParameters =
        Seq(
          Some("SelectExpression" -> selectExpression),
          Some("ConsistentRead" -> (if (consistency == ConsistentRead) "true" else "false")),
          optional("MaxNumberOfItems", maxNumberOfItems),
          optional("NextToken", nextToken)
        ).flatten.toMap
  }

  object Attributes {
    def param(kind: String, pos: Int, value: String) = Map("Attribute." + pos + "." + kind -> value)

    def attributeNames(names: Set[String]): Map[String, String] = {
      import scala.collection.immutable.HashMap;
      var coded: Map[String, String] = Map.empty
      var pos = 0;
      for (name <- names) {
        coded = coded + ("AttributeName." + pos -> name)
        pos += 1
      }
      coded
    }

    def optional[T](name: String, value: Option[T]): Option[(String, String)] =
      value map { v => (name, v.toString) }
  }
}

