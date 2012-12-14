#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package ${package}


import pro.savant.circumflex._, core._, web._

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers

@RunWith(classOf[JUnitRunner])
class MySpec
  extends FreeSpec
  with BeforeAndAfter
  with MustMatchers {

  before {
    cx("cx.router") = classOf[Main]
    MockApp.start()
  }

  after {
    MockApp.stop()
  }

  "My application" - {
    "test itself" in {
      MockApp.get("/test").execute().content must equal ("I'm fine, thanks!")
    }
  }
}