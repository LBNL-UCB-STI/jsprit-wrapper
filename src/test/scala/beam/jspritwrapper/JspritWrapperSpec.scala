package beam.jspritwrapper

import beam.jspritwrapper.JspritWrapper._
import org.scalatest.{Matchers, WordSpecLike}

/**
 * @author Dmitry Openkov
 */
class JspritWrapperSpec extends WordSpecLike with Matchers {

  "Jsprit wrapper" must {
    "get solution out of Jsprit" in {
      val vehicles = Seq(
        Vehicle("v-1", Location(10, 10), 3),
        Vehicle("v-2", Location(2, 0), 2),
        Vehicle("v-3", Location(2, 0), 2),
      )
      val pickups = Seq(
        Pickup("pickup-1", Location(5, 0), 4),
        Pickup("pickup-2", Location(0, 5), 3),
        Pickup("pickup-3", Location(2, 4), 2),
        Pickup("pickup-4", Location(4, 2), 2),
      )
      val dropoffs = Seq(
        Dropoff("dropoff-1", Location(5, 5), 4),
        Dropoff("dropoff-2", Location(5, 14), 3),
        Dropoff("dropoff-3", Location(12, 5), 2),
        Dropoff("dropoff-4", Location(12, 0), 2),
      )
      val solution = JspritWrapper.solve(Problem(vehicles, pickups, dropoffs))

      solution.routes should not be empty

      solution.unassigned.map(_.id) should contain theSameElementsAs Vector("pickup-1", "dropoff-1")

      println(solution)
    }
  }

}
