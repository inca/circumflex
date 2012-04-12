package ru.circumflex
package diff

import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(CircumflexDiffSpec)

object CircumflexDiffSpec extends Specification {

  val original = Seq("The", "awesome", "cat", "is", "really", "cool", "and", "etc.")
  val revised = Seq("The", "ordinary", "cat", "is", "also", "really", "cool")

  val m = new MyersDiff[String]

  val d = m.diff(original, revised)

  // Circumflex Myers Diff

  "After Diff Algorithm Difference" should {
    "have 3 EqualChunks" in {
      d.chunks.filter(_.isInstanceOf[EqualChunk[String]]).size must_== 3
    }

    "have 1 ChangeChunk" in {
      d.chunks.filter(_.isInstanceOf[ChangeChunk[String]]).size must_== 1
    }

    "have 1 DeleteChunk" in {
      d.chunks.filter(_.isInstanceOf[DeleteChunk[String]]).size must_== 1
    }

    "have 1 InsertChunk" in {
      d.chunks.filter(_.isInstanceOf[InsertChunk[String]]).size must_== 1
    }
  }

  "ChangeChunk" should {
    val chunk = d.chunks.filter(_.isInstanceOf[ChangeChunk[String]])(0).asInstanceOf[ChangeChunk[String]]

    "have original 'awesome' seq" in {
      chunk.original must_== Seq("awesome")
    }
    "have revised 'ordinary' seq" in {
      chunk.revised must_== Seq("ordinary")
    }
  }

  "EqualChunk" should {
    val chunk = d.chunks.filter(_.isInstanceOf[EqualChunk[String]])(0).asInstanceOf[EqualChunk[String]]

    "have the same original and revised seq" in {
      chunk.original must_== chunk.revised
    }
  }

  "InsertChunk" should {
    val chunk = d.chunks.filter(_.isInstanceOf[InsertChunk[String]])(0).asInstanceOf[InsertChunk[String]]

    "have zero length original seq" in {
      chunk.original.size must_== 0
    }

    "have non zero revised seq" in {
      chunk.revised.size must_== 1
    }
  }

  "DeleteChunk" should {
    val chunk = d.chunks.filter(_.isInstanceOf[DeleteChunk[String]])(0).asInstanceOf[DeleteChunk[String]]

    "have non zero original seq" in {
      chunk.original.size must_== 2
    }

    "have zero length revised seq" in {
      chunk.revised.size must_== 0
    }
  }
}




