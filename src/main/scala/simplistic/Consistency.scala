/*
 * Copyright (c) 2011 Alex Boisvert
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package simplistic

sealed trait Consistency {
  def orElse(c: Consistency): Consistency
  def isConsistent: Boolean
}

case object UnspecifiedConsistency extends Consistency {
  override def orElse(c: Consistency) = c
  override def isConsistent = false
}

case object EventuallyConsistent extends Consistency {
  def orElse(c: Consistency) = this
  override def isConsistent = false
}

case object ConsistentRead extends Consistency {
  def orElse(c: Consistency) = this
  override def isConsistent = true
}
