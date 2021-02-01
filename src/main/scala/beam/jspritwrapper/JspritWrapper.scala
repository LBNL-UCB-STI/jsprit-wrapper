package beam.jspritwrapper

import com.graphhopper.jsprit.core.algorithm.box.Jsprit
import com.graphhopper.jsprit.core.problem.job.{Delivery, Pickup => JPickup}
import com.graphhopper.jsprit.core.problem.solution.route.VehicleRoute
import com.graphhopper.jsprit.core.problem.solution.route.activity.{DeliverService, PickupService, TourActivity}
import com.graphhopper.jsprit.core.problem.vehicle.{VehicleImpl, VehicleType, VehicleTypeImpl}
import com.graphhopper.jsprit.core.problem.{VehicleRoutingProblem, Location => JLocation}
import com.graphhopper.jsprit.core.util.Solutions

import scala.collection.JavaConverters._

/**
 * @author Dmitry Openkov
 */
object JspritWrapper {

  case class Location(x: Double, y: Double)

  case class Vehicle(id: String, location: Location, capacity: Int)

  sealed trait Service {
    def id: String
  }

  case class Pickup(id: String, location: Location, capacity: Int) extends Service

  case class Dropoff(id: String, location: Location, capacity: Int) extends Service

  case class Problem(vehicles: Seq[Vehicle], pickups: Seq[Pickup], dropoffs: Seq[Dropoff])

  case class RouteLeg(service: Service, arrivalTime: Double)

  case class Route(vehicle: Vehicle, startTime: Double, legs: IndexedSeq[RouteLeg], endTime: Double)

  case class Solution(routes: IndexedSeq[Route], unassigned: IndexedSeq[Service])

  val WEIGHT_INDEX = 0

  def solve(problem: Problem): Solution = {
    val services = problem.pickups ++ problem.dropoffs

    def assertUniqueIds(ids: Seq[String]): Unit = {
      val notUniqueId = ids.groupBy(identity).collectFirst { case (id, ids) if ids.size > 1 => id }
      assert(notUniqueId.isEmpty, s"Same id: ${notUniqueId.get}")
    }

    assertUniqueIds(problem.vehicles.map(_.id))
    assertUniqueIds(services.map(_.id))


    val vehicleToType = extractJspritVehicleTypes(problem.vehicles)
    val vehicles = problem.vehicles.map(vehicle => toJspritVehicle(vehicle, vehicleToType))
    val pickups = problem.pickups.map(pickup => toJspritPickup(pickup))
    val dropoffs = problem.dropoffs.map(dropoff => toJspritDelivery(dropoff))

    val vrpBuilder = VehicleRoutingProblem.Builder.newInstance
    vrpBuilder.addAllVehicles(vehicles.asJava)
    vrpBuilder.addAllJobs((pickups ++ dropoffs).asJava)

    val jProblem = vrpBuilder.build

    val algorithm = Jsprit.createAlgorithm(jProblem)
    val solutions = algorithm.searchSolutions
    val bestSolution = Solutions.bestOf(solutions)
    val jRoutes: IndexedSeq[VehicleRoute] = bestSolution.getRoutes.asScala.toIndexedSeq

    val idToVehicle = problem.vehicles.map(vehicle => vehicle.id -> vehicle).toMap
    val idToService = services.map(service => service.id -> service).toMap
    val routes = jRoutes.map(vehicleRoute => toRoute(vehicleRoute, idToVehicle, idToService))
    val unassigned = bestSolution.getUnassignedJobs.asScala.toIndexedSeq
      .map(job => idToService(job.getId))
    Solution(routes, unassigned)
  }

  private def extractJspritVehicleTypes(vehicles: Seq[Vehicle]): Map[Vehicle, VehicleType] = {
    val capacitiesInt = vehicles.map(_.capacity).distinct
    val capacityToType = capacitiesInt.map { capacity =>
      capacity -> VehicleTypeImpl.Builder.newInstance(s"vehicle_type-$capacity")
        .addCapacityDimension(WEIGHT_INDEX, capacity)
        .build()
    }.toMap
    vehicles
      .map(v => v -> capacityToType(v.capacity))
      .toMap
  }

  private def toJspritVehicle(vehicle: Vehicle, vehicleTypes: Map[Vehicle, VehicleType]): VehicleImpl = {
    VehicleImpl.Builder.newInstance(vehicle.id)
      .setType(vehicleTypes(vehicle))
      .setStartLocation(toJspritLocation(vehicle.location))
      .build()
  }

  private def toJspritPickup(pickup: Pickup): JPickup = {
    JPickup.Builder.newInstance(pickup.id)
      .addSizeDimension(WEIGHT_INDEX, pickup.capacity)
      .setLocation(toJspritLocation(pickup.location))
      .build()
  }

  private def toJspritDelivery(dropoff: Dropoff): Delivery = {
    Delivery.Builder.newInstance(dropoff.id)
      .addSizeDimension(WEIGHT_INDEX, dropoff.capacity)
      .setLocation(toJspritLocation(dropoff.location))
      .build()
  }

  private def toJspritLocation(location: Location): JLocation = {
    JLocation.newInstance(location.x, location.y)
  }

  def toLeg(tourActivity: TourActivity, services: Map[String, Service]): RouteLeg = {
    tourActivity match {
      case pickup: PickupService =>
        RouteLeg(services(pickup.getJob.getId), pickup.getArrTime)
      case deliver: DeliverService =>
        RouteLeg(services(deliver.getJob.getId), deliver.getArrTime)
      case _ => throw new IllegalArgumentException(s"Unexpected activity $tourActivity")
    }
  }

  private def toRoute(vehicleRoute: VehicleRoute, vehicles: Map[String, Vehicle], services: Map[String, Service]): Route = {
    val vehicle = vehicles(vehicleRoute.getVehicle.getId)
    val activities: IndexedSeq[TourActivity] = vehicleRoute.getActivities.asScala.toIndexedSeq
    val legs = activities.map(activity => toLeg(activity, services))
    Route(vehicle, vehicleRoute.getStart.getEndTime, legs, vehicleRoute.getEnd.getArrTime)
  }
}
