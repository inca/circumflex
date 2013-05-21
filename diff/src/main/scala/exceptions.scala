package circumflex
package diff

class DiffException(msg: String) extends Exception(msg)

class DiffFailedException(msg: String) extends DiffException(msg)