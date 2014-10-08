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

import scala.xml._
import XMLFields._

class Errors(xml: NodeSeq) {
  val error = new Error(node("Error", xml))
}

class Error(xml: NodeSeq) {
  val code = string("Code", xml)
  val message = string("Message", xml)
  val boxUsage = optionalDouble("BoxUsage", xml)
}

object Format {
  def formatAttributes(map:Map[String, Set[String]]) =
    (map.keys map ( n => n + ": " + (map(n) mkString ", "))) mkString "\n"
}

/**
 * functions for breaking down XML
 */
object XMLFields {
  def node(name: String, xml: NodeSeq) =(xml \ name)
  def nodes(name: String, xml: NodeSeq) = (xml \ name)
  def string(name: String, xml: NodeSeq) = node(name, xml) text
  def optionalString(name: String, xml: NodeSeq) :Option[String] = {
    val found = string(name, xml)
    if (found.length > 0) Some(found)
    else None
  }
  def strings(name: String, xml: NodeSeq) = nodes(name, xml) map (_.text)
  def dateField(name: String, xml: NodeSeq) = dateFormat.parse(string(name, xml))
  def int(name: String, xml: NodeSeq) = Integer.parseInt(string(name, xml))
  def double(name: String, xml: NodeSeq) = java.lang.Double.parseDouble(string(name, xml))
  def optionalDouble(name: String, xml: NodeSeq): Option[Double] = {
    val found = string(name, xml)
    if (found.length > 0) Some(java.lang.Double.parseDouble(found))
    else None
  }
  def boolean(name: String, xml: NodeSeq) = string(name, xml) match {
    case "True" => true
    case "False" => false
  }
  def dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
}
