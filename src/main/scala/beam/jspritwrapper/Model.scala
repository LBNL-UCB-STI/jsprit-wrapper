package beam.jspritwrapper

/**
 * @author Dmitry Openkov
 */


case class Location(x: Double, y: Double)

case class Vehicle(id: String, location: Location, capacity: Int, returnToDepot: Boolean = true)

sealed trait Service {
  def id: String

  def location: Location

  def capacity: Int

  def serviceTime: Double
}

case class Pickup(id: String, location: Location, capacity: Int, serviceTime: Double) extends Service

case class Dropoff(id: String, location: Location, capacity: Int, serviceTime: Double) extends Service

case class TimeDistanceCost(time: Double, distance: Double, cost: Double)

case class Problem(vehicles: Seq[Vehicle], services: Seq[Service],
                   cost: Option[(Location, Location, Double, Option[Vehicle]) => TimeDistanceCost] = None)

case class RouteActivity(service: Service, arrivalTime: Double)

case class Route(vehicle: Vehicle, startTime: Double, startLocation: Location, activities: IndexedSeq[RouteActivity],
                 endTime: Double, endLocation: Location) {
  val duration: Double = endTime - startTime
}

case class Solution(routes: IndexedSeq[Route], unassigned: IndexedSeq[Service])
