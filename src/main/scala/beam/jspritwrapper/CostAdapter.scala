package beam.jspritwrapper

import com.graphhopper.jsprit.core.{problem => jproblem}
import jproblem.cost.AbstractForwardVehicleRoutingTransportCosts
import JspritWrapper.JLocation
import JspritWrapper.JVehicle
import JspritWrapper.JDriver

/**
 * @author Dmitry Openkov
 */
class CostAdapter(costCalculator: (Location, Location, Double, Option[Vehicle]) => TimeDistanceCost)
  extends AbstractForwardVehicleRoutingTransportCosts {

  import JspritWrapper.toLocation

  private def toVehicle(jVehicle: JVehicle): Vehicle = Vehicle(
    jVehicle.getId,
    jVehicle.getStartLocation,
    jVehicle.getType.getCapacityDimensions.get(JspritWrapper.WEIGHT_INDEX),
    jVehicle.isReturnToDepot,
  )

  override def getDistance(from: JLocation, to: JLocation, departureTime: Double, vehicle: JVehicle): Double = {
    costCalculator(from, to, departureTime, Option(vehicle).map(toVehicle)).distance
  }

  override def getTransportTime(from: JLocation, to: JLocation, departureTime: Double, driver: JDriver, vehicle: JVehicle): Double = {
    costCalculator(from, to, departureTime, Option(vehicle).map(toVehicle)).time
  }

  override def getTransportCost(from: JLocation, to: JLocation, departureTime: Double, driver: JDriver, vehicle: JVehicle): Double = {
    costCalculator(from, to, departureTime, Option(vehicle).map(toVehicle)).cost
  }
}
