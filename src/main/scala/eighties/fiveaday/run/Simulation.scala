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
import better.files.{File => ScalaFile, _}
import eighties.fiveaday.health._
import eighties.fiveaday.observable
import eighties.fiveaday.opinion.interchangeConviction
import eighties.fiveaday.population.Individual
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.dynamic.MoveMatrix.{LocatedCell, TimeSlice}
import eighties.h24.generation._
import eighties.h24.simulation._
import eighties.h24.social.AggregatedSocialCategory
import eighties.h24.space._
import eighties.h24.tools.Log.log
import monocle._
import scopt.OParser

import scala.util.Random

object Simulation {

  def run(
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    days: Int,
    population: java.io.File,
    moves: java.io.File,
    distributionConstraints: java.io.File,
    moveType: MoveType,
    rng: Random,
    visitor: Option[(World[Individual], BoundingBox, Int, Option[(Int, Int)]) => Unit] = None): World[Individual] = {

    val healthCategory = generateHealthCategory(distributionConstraints)

    def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)

    def exchange(moved: World[Individual], day: Int, slice: Int, rng: Random): World[Individual] = {
      log(s"simulate day $day, slice $slice")

      interchangeConviction(
        moved,
        maxProbaToSwitch = maxProbaToSwitch,
        constraintsStrength = constraintsStrength,
        inertiaCoefficient = inertiaCoefficient,
        healthyDietReward = healthyDietReward,
        rng
      )
    }

    simulate(
      days,
      population,
      moves,
      moveType,
      buildIndividual,
      exchange,
      Individual.locationV,
      Individual.homeV,
      Individual.socialCategoryV.get,
      rng,
      visitor
    )
  }

  def simulate(
    days: Int,
    population: java.io.File,
    moves: java.io.File,
    moveType: MoveType,
    buildIndividual: (IndividualFeature, Random) => Individual,
    exchange: (World[Individual], Int, Int, Random) => World[Individual],
    location: Lens[Individual, Location],
    home: Lens[Individual, Location],
    socialCategory: Individual => AggregatedSocialCategory,
    rng: Random,
    visitor: Option[(World[Individual], BoundingBox, Int, Option[(Int, Int)]) => Unit] = None): World[Individual] = {

    val moveMatrix = MoveMatrix.load(moves)
    def locatedCell: LocatedCell = (timeSlice: TimeSlice, i: Int, j: Int) => moveMatrix.get((i, j), timeSlice)
    def worldFeature = WorldFeature.load(population)
    try {
      simulateWorld(
        days = days,
        world = () => initialiseWorld(worldFeature, moveType, location, home, socialCategory, buildIndividual, locatedCell, rng),
        bbox = worldFeature.originalBoundingBox,
        gridSize = worldFeature.gridSize,
        locatedCell = locatedCell,
        moveType = moveType,
        exchange = exchange,
        stableDestinations = Individual.stableDestinationsV,
        location = location,
        home = home.get,
        socialCategory = socialCategory,
        rng = rng,
        visitor = visitor
      )
    } finally moveMatrix.close()
  }


  def initialiseWorld(
    worldFeature: WorldFeature,
    moveType: MoveType,
    location: Lens[Individual, Location],
    home: Lens[Individual, Location],
    socialCategory: Individual => AggregatedSocialCategory,
    buildIndividual: (IndividualFeature, Random) => Individual,
    locatedCell: LocatedCell,
    rng: Random): World[Individual] = {
    def world = generateWorld(worldFeature.individualFeatures, buildIndividual, location, home, rng)
    moveType match {
      case MoveType.Data => assignRandomDayLocation(world, locatedCell, Individual.dayDestinationV, location.get, home.get, socialCategory, rng)
      case MoveType.Random => world
      case MoveType.No => world
    }
  }

  def assignRandomDayLocation(
    world: World[Individual],
    locatedCell: LocatedCell,
    dayDestination: Lens[Individual, Location],
    location: Individual => Location,
    home: Individual => Location,
    socialCategory: Individual => AggregatedSocialCategory,
    rng: Random): World[Individual] = {
    import eighties.h24.dynamic

    val newIndividuals = Array.ofDim[Individual](world.individuals.length)
    var index = 0

    for {
      (line, i) <- Focus[Index[Individual]](_.cells).get(Index.indexIndividuals(world, location)).zipWithIndex
      (individuals, j) <- line.zipWithIndex
    } {
      val workTimeMovesFromCell = locatedCell(dayTimeSlice, i, j)

      assert(workTimeMovesFromCell != null)

      for {
        individual <- individuals
      } {
        def newIndividual =
          dynamic.sampleDestinationInMoveMatrix(workTimeMovesFromCell, individual, socialCategory, rng) match {
            case Some(d) => dayDestination.replace(d)(individual)
            case None => dayDestination.replace(home(individual))(individual)
          }
        newIndividuals(index) = newIndividual
        index += 1
      }
    }

    Focus[World[Individual]](_.individuals).set(newIndividuals)(world)
  }

}

object Fit {

  def fitness(world: World[Individual], file: File): Double = observable.deltaHealthByCategory(world, file)

  def run(
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    days: Int,
    population: java.io.File,
    moves: java.io.File,
    distributionConstraints: java.io.File,
    moveType: MoveType,
    rng: Random): Double = {

    fitness(
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
        rng = rng
      ),
      distributionConstraints
    )
  }
}

@main def SimulationApp(args: String*): Unit = {

  case class Config(
    population: Option[File] = None,
    moves: Option[File] = None,
    distribution: Option[File] = None,
    seed: Option[Long] = None)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("5aday simulator"),
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
        .text("result path where the moves are generated"),
      opt[Long]('s', "seed")
        .action((x, c) => c.copy(seed = Some(x)))
        .text("seed for the random number generator")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val seed = config.seed.getOrElse(42L)
      val rng = new Random(seed)

      log("loading population")
      val worldFeatures = config.population.get
      val moves = config.moves.get

      val distributionConstraints = config.distribution.get

      val parameterMap = util.getParameterMap
      // FIXME Add named parameter set parameter?
      val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap("summer2020")

      val world =
        Simulation.run(
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          days = 6,
          population = worldFeatures,
          moves = moves,
          distributionConstraints = distributionConstraints,
          moveType = MoveType.Data,
          rng = rng
        )

      log("population " + world.individuals.length)
      log("delta health: " + observable.deltaHealthByCategory(world, distributionConstraints))
      log("social inequality: " + observable.weightedInequalityRatioBySexAge(world))
    case _ =>
  }

}


