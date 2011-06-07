#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}

import ru.circumflex._, core._, web._

import org.specs.runner.JUnit4
import org.specs.Specification

class SpecsTest extends JUnit4(MySpec)

object MySpec extends Specification {

  doBeforeSpec {
    cx("cx.router") = classOf[Main]
    MockApp.start
  }

  doAfterSpec {
    MockApp.stop
  }

  "My application" should {
    "test itself" in {
      MockApp.get("/test").execute().content must_== "I'm fine, thanks!"
    }
  }
}