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

import java.io.File
import java.util.Calendar

import better.files._
import eighties.h24.generation._
import eighties.fiveaday.opinion.interchangeConviction
import eighties.fiveaday.population._
import eighties.h24.space._
import eighties.fiveaday.observable
import eighties.fiveaday.health._
import eighties.h24.simulation.MoveType
import eighties.h24.simulation.simulateWithVisitor
import scopt.OParser

import scala.util.Random

object SimulationWithBeforeAndAfterMaps extends App {

  case class Config(
    population: Option[File] = None,
    moves: Option[File] = None,
    distribution: Option[File] = None,
    output: Option[File] = None,
    seed: Option[Long] = None,
    scenario: Option[Int] = None)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("5aday simulator with map"),
      // option -f, --foo
      opt[File]('d', "distribution")
        .required()
        .action((x, c) => c.copy(distribution = Some(x)))
        .text("Initial distribution of opinion"),
      opt[File]('p', "population")
        .required()
        .action((x, c) => c.copy(population = Some(x)))
        .text("population file generated with h24"),
      opt[File]('m', "moves")
        .required()
        .action((x, c) => c.copy(moves = Some(x)))
        .text("path of the moves"),
      opt[File]('o', "output")
        .required()
        .action((x, c) => c.copy(output = Some(x)))
        .text("result directory where the maps are generated"),
      opt[Int]('c', "scenario")
        .validate(x => if(x < 1 || x > 5) Left("Scenario must be in the range 1 - 5") else Right())
        .action((x, c) => c.copy(scenario = Some(x)))
        .text("scenario"),
      opt[Long]('s', "seed")
        .action((x, c) => c.copy(seed = Some(x)))
        .text("seed for the random number generator")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      def run(scenario: Int, parameterName: String, seed: Long) {
        val rng = new Random(seed)

        val moves = config.moves.get
        val distributionConstraints = config.distribution.get

        val parameterMap = Map(
          ("environment", (0.08433503404492204, 0.14985650295919095, 0.7333227248982435, 0.03291749337039018, 0.23398559843386524)),
          ("partner", (0.029443016524418164, 0.05415637081735669, 0.20870694702023695, 0.10353942953774942, 0.7233067964061539)),
          ("aout", (1.0, 0.894687222564233, 0.0, 1.0, 0.985952008896386)),
          ("calibration", (0.9999372894598938, 3.780104482210688E-5, 0.1391139744735069, 0.10973200449784659, 0.9011227599761806)),
          ("calibration_fitness", (0.8507843893208267, 0.45377746673575825, 0.6585498777924014, 0.210784861364803, 0.2589547233574915)),
          ("ose", (1.0, 0.12454546, 0.66766742, 0.26597869, 0.24032422)),
          ("duo_0703", (0.996, 0.405, 0.787, 0.161, 0.102))
        )
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

        val output = config.output.get.toScala / s"results_${parameterName}_Scenario${scenario}_${popName}_${moveTypeString}_$seed"
        output.createDirectories

        val worldFeatures = config.population.get

        val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap(parameterName)

        val healthCategory = generateHealthCategory(distributionConstraints)
        val interactionMap = generateInteractionMap(distributionConstraints)
        def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, rng)
        def exchange(moved: World[Individual], day: Int, slice: Int, rng: Random) = {
          println(s"simulate day $day, slice $slice")
          interchangeConviction(
            moved,
            slice,
            interactionMap,
            maxProbaToSwitch = maxProbaToSwitch,
            constraintsStrength = constraintsStrength,
            inertiaCoefficient = inertiaCoefficient,
            healthyDietReward = healthyDietReward,
            interpersonalInfluence = interpersonalInfluence,
            rng
          )
        }

        val days = 6
        var initWorld: World[Individual] = null
        def visit(world: World[Individual], obb: BoundingBox, option: Option[(Int, Int)]) = {
          option match {
            case Some((day, slice)) =>
              println(s"\tday $day - slice $slice")
              if (day == days-1 && slice == 2) {
                def soc = observable.weightedInequalityRatioBySexAge(world)
                def initSoc = observable.weightedInequalityRatioBySexAge(initWorld)
                util.mapHealth(world, obb, world.sideI, world.sideJ, output / "home" / "1_end.tiff", soc.toString, "", maxValue = 0.5, fraction = 5)
                util.mapHealthDiff(initWorld, world, obb, world.sideI, world.sideJ, output / "home" / "2_diff.tiff", (soc - initSoc).toString, "", fraction = 8)
              }
            case None =>
              println(s"Init")
              initWorld = world
              def soc = observable.weightedInequalityRatioBySexAge(world)
              println(s"${Calendar.getInstance.getTime}: delta health: ${observable.deltaHealth(world)}")
              println(s"${Calendar.getInstance.getTime}: social inequality: ${observable.weightedInequalityRatioBySexAge(world)}")

              util.mapHealth(world, obb, world.sideI, world.sideJ, output / "home" / "0_start.tiff", soc.toString, "", maxValue = 0.5, fraction = 5)
          }
        }
        val world = simulateWithVisitor[Individual](
          days,
          worldFeatures,
          moves,
          moveType,
          buildIndividual,
          exchange,
          Individual.stableDestinationsV,
          Individual.locationV,
          Individual.homeV,
          Individual.socialCategoryV.get,
          visit,
          rng
        )
        println(s"${Calendar.getInstance.getTime}: finished simulation for $days days")
        println(s"${Calendar.getInstance.getTime}: delta health: ${observable.deltaHealth(world)}")
        println(s"${Calendar.getInstance.getTime}: social inequality: ${observable.weightedInequalityRatioBySexAge(world)}")
      }

      val parameterName = "duo_0703"
      val seed = config.seed.getOrElse(42L)

      if (config.scenario.isDefined) run(config.scenario.get, parameterName, seed)
      else {
        for (scenario <- 1 to 5) run(scenario, parameterName, seed)
      }
    case _ =>
  }
}
