package simplistic

import com.amazonaws.AmazonServiceException

object Exceptions {

  object ConditionalCheckFailed {
    def unapply(e: Exception) = e match {
      case e: AmazonServiceException if e.getErrorCode == "ConditionalCheckFailed" => Some(e)
      case _ => None
    }
  }

}