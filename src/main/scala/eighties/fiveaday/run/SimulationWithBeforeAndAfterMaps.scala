/**
  * Created by Romain Reuillon on 09/05/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package eighties.fiveaday.run

import better.files._
import eighties.fiveaday.observable
import eighties.fiveaday.population._
import eighties.h24.simulation.MoveType
import eighties.h24.space
import eighties.h24.space._
import eighties.h24.tools.Log.log
import org.geotools.data.{DataUtilities, DefaultTransaction}
import org.geotools.geometry.jts.{Geometries, ReferencedEnvelope}
import org.geotools.geopkg.{FeatureEntry, GeoPackage}
import org.geotools.referencing.CRS
import org.locationtech.jts.geom.{GeometryFactory, Coordinate => JCoordinate}
import scopt.OParser

import java.io.File
import scala.util.Random
object SimulationWithBeforeAndAfterMaps {
  def run(
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    days: Int,
    population: java.io.File,
    moves: java.io.File,
    distributionConstraints: java.io.File,
    outputPath: java.io.File,
    moveType: MoveType,
    rng: Random,
    exportData: Boolean): World[Individual] = {
    var initWorld: World[Individual] = null
    var geopkg: GeoPackage = null
    val geometryFactory = new GeometryFactory
    def visit(world: World[Individual], obb: BoundingBox, gridSize: Int, option: Option[(Int, Int)]): Unit = {
      def createEntry(featureTypeName: String, world1: World[Individual], world2: Option[World[Individual]], home: Boolean = false): Unit = {
        def aggregator: Array[Double] => Double = v => v.sum / v.length
        def getValue(individual: Individual) = if (individual.healthy) 1.0 else 0.0
        def mapIndex(index: Index[Individual]) = index.cells.map(_.map(individuals => if (individuals.nonEmpty) Some((aggregator(individuals.map(getValue)), individuals.size, individuals.filter(_.healthy).size, individuals.filterNot(_.healthy).size)) else None))
        val entry = new FeatureEntry
        val specs = "geom:Polygon:srid=3035, propHealthy:Double, num:Integer, numHealthy:Integer, numUnhealthy:Integer"
        val featureType = DataUtilities.createType(featureTypeName, specs)
        entry.setGeometryColumn (featureType.getGeometryDescriptor.getLocalName)
        val geometryType = featureType.getGeometryDescriptor.getType
        val gType = Geometries.getForName (geometryType.getName.getLocalPart)
        entry.setGeometryType (gType)
        val referencedEnvelope = new ReferencedEnvelope(obb.minI, obb.maxI+gridSize, obb.minJ, obb.maxJ+gridSize, CRS.decode("EPSG:3035"))
        log(s"bbox ${obb.minI} - ${obb.minJ} ($gridSize)")
        entry.setBounds(referencedEnvelope)
        log("create featureentry " + featureType.getTypeName)
        geopkg.create(entry, featureType)
        val transaction = new DefaultTransaction()
        log("create writer")
        val writer = geopkg.writer(entry,true, null, transaction)
        log(s"Let's go (${world1.sideI} - ${world1.sideJ})")
        val index1 = space.Index.indexIndividuals(world1, if home then Individual.homeV.get else Individual.locationV.get)
        val mappedValues = if (world2.isDefined) {
          val index2 = space.Index.indexIndividuals(world2.get, if home then Individual.homeV.get else Individual.locationV.get)
          val mappedValues1 = mapIndex(index1)
          val mappedValues2 = mapIndex(index2)
          mappedValues1.zipWithIndex.map {
            case (array1, ind1) => array1.zipWithIndex.map {
              case (array2, ind2) => (array2, mappedValues2(ind1)(ind2)) match {
                case (Some(v1), Some(v2)) => Some(v2._1-v1._1, v2._2-v1._2, v2._3-v1._3, v2._4-v1._4)
                case _ => None
              }
            }
          }
        } else {
          mapIndex(index1)
        }
        for {
          (l, i) <- mappedValues.zipWithIndex
          (valueOption, j) <- l.zipWithIndex
          if valueOption.isDefined
        } {
          val ii = obb.minI + i * gridSize
          val jj = obb.minJ + j * gridSize
          val value = valueOption.get
          val (p0, p1, p2, p3)  = (new JCoordinate(ii, jj), new JCoordinate(ii+gridSize, jj), new JCoordinate(ii+gridSize, jj+gridSize), new JCoordinate(ii, jj+gridSize))
          def polygon = geometryFactory.createPolygon(Array(p0,p1,p2,p3,p0))
          val values = Array[AnyRef](
            polygon,
            value._1.asInstanceOf[AnyRef],
            value._2.asInstanceOf[AnyRef],
            value._3.asInstanceOf[AnyRef],
            value._4.asInstanceOf[AnyRef]
          )
          def simpleFeature = writer.next
          simpleFeature.setAttributes(values)
          writer.write()
        }
        log("close writer")
        writer.close()
        log("commit transaction")
        transaction.commit()
        log("close transaction")
        transaction.close()
      }
      option match {
        case Some((day, slice)) =>
          if (day == 0 && exportData) {
            createEntry(s"Initial_$slice", world, None)
          } else if (day == days-1) {
            // the last simulation
            if (exportData) {
              createEntry(s"Final_$slice", world, None)
//              createEntry("Diff", initWorld, Some(world))
              if (slice == 2)
                createEntry(s"Final", world, None, true)
                log("close geopackage")
                geopkg.close()
            } else if (slice == 2) {
              def soc = observable.weightedInequalityRatioBySexAge(world)
              def initSoc = observable.weightedInequalityRatioBySexAge(initWorld)
              util.mapHealth(world, obb, world.sideI, world.sideJ, outputPath.toScala / "home" / "1_end.tiff", soc.toString, "", maxValue = 0.5, fraction = 5)
              util.mapHealthDiff(initWorld, world, obb, world.sideI, world.sideJ, outputPath.toScala / "home" / "2_diff.tiff", (soc - initSoc).toString, "", fraction = 8)
            }
          }
        case None =>
          // the first simulation
          initWorld = world
          if (exportData) {
            // initialize the geopackage
            val params = new java.util.HashMap[String, Object]()
            params.put("dbtype", "geopkg")
            params.put("database", outputPath.getPath)
            log("create geopackage")
            geopkg = new GeoPackage(outputPath)
            log("init geopackage")
            geopkg.init()
            // create the first entry
//            createEntry("Initial", world, None)
          } else {
            def soc = observable.weightedInequalityRatioBySexAge(world)
            log(s"delta health: ${observable.deltaHealthByCategory(world, distributionConstraints)}")
            log(s"social inequality: ${observable.weightedInequalityRatioBySexAge(world)}")
            util.mapHealth(world, obb, world.sideI, world.sideJ, outputPath.toScala / "home" / "0_start.tiff", soc.toString, "", maxValue = 0.5, fraction = 5)
          }
      }
    }
    Simulation.run(
      maxProbaToSwitch = maxProbaToSwitch,
      constraintsStrength = constraintsStrength,
      inertiaCoefficient = inertiaCoefficient,
      healthyDietReward = healthyDietReward,
      days = days,
      population = population,
      moves = moves,
      distributionConstraints = distributionConstraints,
      moveType = moveType,
      rng = rng,
      visitor = Some(visit)
    )
  }
}

@main def SimulationWithBeforeAndAfterMapsApp(args: String*): Unit = {
  case class Config(
    population: Option[File] = None,
    randomPopulation: Option[File] = None,
    moves: Option[File] = None,
    distribution: Option[File] = None,
    output: Option[File] = None,
    seed: Option[Long] = None,
    scenario: Option[Int] = None,
    exportData: Boolean = true)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("5aday simulator with map"),
      opt[File]('d', "distribution")
        .required()
        .action((x, c) => c.copy(distribution = Some(x)))
        .text("Initial distribution of opinion"),
      opt[File]('p', "population")
        .required()
        .action((x, c) => c.copy(population = Some(x)))
        .text("population file generated with h24"),
      opt[File]('r', "randomPopulation")
        .required()
        .action((x, c) => c.copy(randomPopulation = Some(x)))
        .text("random population file generated with h24"),
      opt[File]('m', "moves")
        .required()
        .action((x, c) => c.copy(moves = Some(x)))
        .text("path of the moves"),
      opt[File]('o', "output")
        .required()
        .action((x, c) => c.copy(output = Some(x)))
        .text("result directory where the maps are generated"),
      opt[Int]('c', "scenario")
        .validate(x => if(x < 1 || x > 5) failure("Scenario must be in the range 1 - 5") else success)
        .action((x, c) => c.copy(scenario = Some(x)))
        .text("scenario"),
      opt[Long]('s', "seed")
        .action((x, c) => c.copy(seed = Some(x)))
        .text("seed for the random number generator"),
      opt[Boolean]('e', "export")
        .action((x, c) => c.copy(exportData = x))
        .text("export the results rather than produce the maps")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      def run(scenario: Int, parameterName: String, seed: Long): Unit = {
        val rng = new Random(seed)
        val moves = config.moves.get
        val distributionConstraints = config.distribution.get
        val parameterMap = util.getParameterMap
        val scenarioMap = Map(
          (1, (RandomPop, MoveType.No)),
          (2, (RandomPop, MoveType.Random)),
          (3, (ObservedPop, MoveType.No)),
          (4, (ObservedPop, MoveType.Random)),
          (5, (ObservedPop, MoveType.Data))
        )
        val (pop, moveType) = scenarioMap(scenario)
        val moveTypeString = moveType match {
          case MoveType.Data => "ObservedMove"
          case MoveType.Random => "RandomMove"
          case MoveType.No => "NoMove"
        }
        val popName = pop match {
          case RandomPop => "RandomPop"
          case ObservedPop => "ObservedPop"
        }
        println(s"popName = $popName")
        val output = if (config.exportData) {
          val outputFile = config.output.get.toScala / s"results_${parameterName}_Scenario${scenario}_${popName}_${moveTypeString}_$seed.gpkg"
          outputFile.parent.createDirectories()
          outputFile
        } else {
          val outputDirectory = config.output.get.toScala / s"results_${parameterName}_Scenario${scenario}_${popName}_${moveTypeString}_$seed"
          outputDirectory.createDirectories()
          outputDirectory
        }
        val worldFeatures = pop match {
          case RandomPop => config.randomPopulation.get
          case ObservedPop => config.population.get
        }
        val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap(parameterName)
        SimulationWithBeforeAndAfterMaps.run(
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          days = 6,
          population = worldFeatures,
          moves = moves,
          distributionConstraints = distributionConstraints,
          output.toJava,
          moveType,
          rng = rng,
          config.exportData
        )
      }
      val parameterName = "HigherProp"
      val seed = config.seed.getOrElse(42L)
      if (config.scenario.isDefined) run(config.scenario.get, parameterName, seed)
      else {
        for (scenario <- 1 to 5) run(scenario, parameterName, seed)
      }
    case _ =>
  }
}
