package beam.jspritwrapper

import com.graphhopper.jsprit.core.algorithm.box.Jsprit
import com.graphhopper.jsprit.core.problem.job.{Delivery, Pickup => JPickup}
import com.graphhopper.jsprit.core.problem.solution.route.VehicleRoute
import com.graphhopper.jsprit.core.problem.solution.route.activity.{DeliverService, PickupService, TourActivity}
import com.graphhopper.jsprit.core.problem.vehicle.{VehicleImpl, VehicleTypeImpl}
import com.graphhopper.jsprit.core.problem.VehicleRoutingProblem
import com.graphhopper.jsprit.core.util.Solutions

import scala.collection.JavaConverters._
import scala.language.implicitConversions

/**
 * @author Dmitry Openkov
 */
object JspritWrapper {

  import com.graphhopper.jsprit.core.{problem => jspritproblem}

  type JVehicle = jspritproblem.vehicle.Vehicle
  type JVehicleType = jspritproblem.vehicle.VehicleType
  type JLocation = jspritproblem.Location
  type JDriver = jspritproblem.driver.Driver

  val WEIGHT_INDEX = 0

  implicit def toLocation(location: JLocation): Location =
    Location(location.getCoordinate.getX, location.getCoordinate.getY)

  def solve(problem: Problem): Solution = {

    def assertUniqueIds(ids: Seq[String]): Unit = {
      val notUniqueId = ids.groupBy(identity).collectFirst { case (id, ids) if ids.size > 1 => id }
      assert(notUniqueId.isEmpty, s"Same id: ${notUniqueId.get}")
    }

    assertUniqueIds(problem.vehicles.map(_.id))
    assertUniqueIds(problem.services.map(_.id))

    val vrpBuilder = VehicleRoutingProblem.Builder.newInstance

    problem.cost.foreach(calc => vrpBuilder.setRoutingCost(new CostAdapter(calc)))

    def extractJspritVehicleTypes(vehicles: Seq[Vehicle]): Map[Vehicle, JVehicleType] = {
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

    def toJspritVehicle(vehicle: Vehicle, vehicleTypes: Map[Vehicle, JVehicleType]): VehicleImpl = {
      VehicleImpl.Builder.newInstance(vehicle.id)
        .setType(vehicleTypes(vehicle))
        .setStartLocation(toJspritLocation(vehicle.location))
        .setReturnToDepot(vehicle.returnToDepot)
        .build()
    }

    def toJspritPickup(pickup: Pickup): JPickup = {
      JPickup.Builder.newInstance(pickup.id)
        .addSizeDimension(WEIGHT_INDEX, pickup.capacity)
        .setLocation(toJspritLocation(pickup.location))
        .setServiceTime(pickup.serviceTime)
        .build()
    }

    def toJspritDelivery(dropoff: Dropoff): Delivery = {
      Delivery.Builder.newInstance(dropoff.id)
        .addSizeDimension(WEIGHT_INDEX, dropoff.capacity)
        .setLocation(toJspritLocation(dropoff.location))
        .setServiceTime(dropoff.serviceTime)
        .build()
    }

    def toJspritLocation(location: Location): JLocation = jspritproblem.Location.newInstance(location.x, location.y)

    val vehicleToType = extractJspritVehicleTypes(problem.vehicles)
    val vehicles = problem.vehicles.map(vehicle => toJspritVehicle(vehicle, vehicleToType))
    val services = problem.services.map {
      case x: Pickup => toJspritPickup(x)
      case x: Dropoff => toJspritDelivery(x)
    }

    vrpBuilder.addAllVehicles(vehicles.asJava)
    vrpBuilder.addAllJobs(services.asJava)

    val jProblem = vrpBuilder.build

    val algorithm = Jsprit.createAlgorithm(jProblem)
    val solutions = algorithm.searchSolutions
    val bestSolution = Solutions.bestOf(solutions)
    val jRoutes: IndexedSeq[VehicleRoute] = bestSolution.getRoutes.asScala.toIndexedSeq

    val idToVehicle = problem.vehicles.map(vehicle => vehicle.id -> vehicle).toMap
    val idToService = problem.services.map(service => service.id -> service).toMap
    val routes = jRoutes.map(vehicleRoute => toRoute(vehicleRoute, idToVehicle, idToService))
    val unassigned = bestSolution.getUnassignedJobs.asScala.toIndexedSeq
      .map(job => idToService(job.getId))
    Solution(routes, unassigned)

  }


  private def toRouteActivity(tourActivity: TourActivity, services: Map[String, Service]): RouteActivity = {
    tourActivity match {
      case pickup: PickupService =>
        RouteActivity(services(pickup.getJob.getId), pickup.getArrTime)
      case deliver: DeliverService =>
        RouteActivity(services(deliver.getJob.getId), deliver.getArrTime)
      case _ => throw new IllegalArgumentException(s"Unexpected activity $tourActivity")
    }
  }

  private def toRoute(vehicleRoute: VehicleRoute, vehicles: Map[String, Vehicle], services: Map[String, Service]): Route = {
    val vehicle = vehicles(vehicleRoute.getVehicle.getId)
    val activities: IndexedSeq[TourActivity] = vehicleRoute.getActivities.asScala.toIndexedSeq
    val tourActivities = activities.map(activity => toRouteActivity(activity, services))
    Route(vehicle, vehicleRoute.getStart.getEndTime, vehicleRoute.getStart.getLocation, tourActivities,
      vehicleRoute.getEnd.getArrTime, vehicleRoute.getEnd.getLocation)
  }
}
