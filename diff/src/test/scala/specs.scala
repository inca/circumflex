package circumflex
package diff

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import matchers.MustMatchers

@RunWith(classOf[JUnitRunner])
class CircumflexDiffSpec
  extends FreeSpec with MustMatchers {

  val original = Seq("The", "awesome", "cat", "is", "really", "cool", "and", "etc.")
  val revised = Seq("The", "ordinary", "cat", "is", "also", "really", "cool")

  val m = new MyersDiff[String]

  val d = m.diff(original, revised)

  // Circumflex Myers Diff

  "After Diff Algorithm Difference" - {
    "have 3 EqualChunks" in {
      d.chunks.filter(_.isInstanceOf[EqualChunk[String]]).size must equal (3)
    }

    "have 1 ChangeChunk" in {
      d.chunks.filter(_.isInstanceOf[ChangeChunk[String]]).size must equal (1)
    }

    "have 1 DeleteChunk" in {
      d.chunks.filter(_.isInstanceOf[DeleteChunk[String]]).size must equal (1)
    }

    "have 1 InsertChunk" in {
      d.chunks.filter(_.isInstanceOf[InsertChunk[String]]).size must equal (1)
    }
  }

  "ChangeChunk" - {
    val chunk = d.chunks.filter(_.isInstanceOf[ChangeChunk[String]])(0).asInstanceOf[ChangeChunk[String]]

    "have original 'awesome' seq" in {
      chunk.original must equal (Seq("awesome"))
    }
    "have revised 'ordinary' seq" in {
      chunk.revised must equal (Seq("ordinary"))
    }
  }

  "EqualChunk" - {
    val chunk = d.chunks.filter(_.isInstanceOf[EqualChunk[String]])(0).asInstanceOf[EqualChunk[String]]

    "have the same original and revised seq" in {
      chunk.original must equal (chunk.revised)
    }
  }

  "InsertChunk" - {
    val chunk = d.chunks.filter(_.isInstanceOf[InsertChunk[String]])(0).asInstanceOf[InsertChunk[String]]

    "have zero length original seq" in {
      chunk.original.size must equal (0)
    }

    "have non zero revised seq" in {
      chunk.revised.size must equal (1)
    }
  }

  "DeleteChunk" - {
    val chunk = d.chunks.filter(_.isInstanceOf[DeleteChunk[String]])(0).asInstanceOf[DeleteChunk[String]]

    "have non zero original seq" in {
      chunk.original.size must equal (2)
    }

    "have zero length revised seq" in {
      chunk.revised.size must equal (0)
    }
  }
}




