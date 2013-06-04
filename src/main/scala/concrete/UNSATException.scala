package concrete

class UNSATException(msg: String) extends Exception(msg)

object UNSATObject extends UNSATException("Inconsistent constraint")

object EmptyDomain extends UNSATException("Empty domain")
