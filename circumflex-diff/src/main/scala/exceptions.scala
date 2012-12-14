package pro.savant.circumflex
package diff

class DiffException(msg: String) extends Exception(msg)

class PatchFailedException(msg: String) extends DiffException(msg)

class DifferentiationFailedException(msg: String) extends DiffException(msg)