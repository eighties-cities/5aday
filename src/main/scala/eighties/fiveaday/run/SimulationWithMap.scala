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

import better.files.Dsl.SymbolicOperations
import better.files._
import eighties.fiveaday.observable
import eighties.fiveaday.population._
import eighties.h24.simulation.MoveType
import eighties.h24.social._
import eighties.h24.space._
import eighties.h24.tools.Log.log
import scopt.OParser

import scala.util.Random

object SimulationWithMap {
  def run(
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    interpersonalInfluence: Double,
    days: Int,
    population: java.io.File,
    moves: java.io.File,
    distributionConstraints: java.io.File,
    outputPath: java.io.File,
    moveType: MoveType,
    rng: Random): World[Individual] = {
    val categories = outputPath.toScala / "health.csv"
    categories.parent.createDirectories()
    categories < "index,day,slice,effective,healthy,ratio,avgOpinion,socialInequality,e\n"
    def getCategoryFile(cat: AggregatedSocialCategory) = {
      outputPath.toScala / s"${Sex.toCode(cat.sex)}_${AggregatedAge.toCode(cat.age)}_${AggregatedEducation.toCode(cat.education)}.csv"
    }
    AggregatedSocialCategory.all.foreach { cat =>
      def f = getCategoryFile(cat)
      f < "index,day,slice,sex,age,educ,effective,healthy,ratio,avgOpinion\n"
    }

    def writeFileByCategory(day: Int, slice: Int, world: World[Individual], file: File, socialInequality: Double, e: Double): Unit = {
      def categoryInfo(category: Vector[Individual]) =
        if (category.isEmpty) List(0, 0, 0.0)
        else {
          val categorySize = category.size
          val nbHealthy = category.count(_.healthy)
          val avgOpinion = category.map(_.opinion.toDouble).sum / categorySize
          List(categorySize, nbHealthy, nbHealthy.toDouble / categorySize, avgOpinion)
        }
      val index = day * 3 + slice
      AggregatedSocialCategory.all.foreach { cat =>
        def individualOfCategory = World.individualsVector[Individual].get(world).filter(Individual.socialCategoryV.get(_) == cat)
        getCategoryFile(cat) << s"""$index,$day,$slice,${Sex.toCode(cat.sex)},${AggregatedAge.toCode(cat.age)},${AggregatedEducation.toCode(cat.education)},${categoryInfo(individualOfCategory).mkString(",")}"""
      }
      val size = World.individualsVector[Individual].get(world).size
      val nbHealthy = World.individualsVector[Individual].get(world).count(_.healthy)
      def ratio = nbHealthy.toDouble / size
      def avgOpinion = World.individualsVector[Individual].get(world).map(_.opinion).sum / size
      file.toScala << s"""$index,$day,$slice,$size,$nbHealthy,$ratio,$avgOpinion,$socialInequality,$e"""
    }
    val parameters = outputPath.toScala / "parameters.csv"
    parameters < "maxProbaToSwitch,constraintsStrength,inertiaCoefficient,healthyDietReward,interpersonalInfluence\n"
    parameters << s"$maxProbaToSwitch,$constraintsStrength,$inertiaCoefficient,$healthyDietReward,$interpersonalInfluence"
    def visit(world: World[Individual], obb: BoundingBox, option: Option[(Int, Int)]): Unit = {
      option match {
        case Some((day, slice)) =>
          val soc = observable.weightedInequalityRatioBySexAge(world)
          val e = observable.erreygersE(world)
          util.mapHealth(world, obb, world.sideI, world.sideJ, outputPath.toScala / "home" / f"$day%02d_$slice.tiff", soc.toString, f"$day%02d_$slice", maxValue = 0.5, fraction = 5)
          util.mapHealth(world, obb, world.sideI, world.sideJ, outputPath.toScala / "location" / f"$day%02d_$slice.tiff", soc.toString, f"$day%02d_$slice", atHome = false, maxValue = 0.5, fraction = 5)
          writeFileByCategory(day, slice, world, categories.toJava, soc, e)
        case None =>
          util.writeState(world, outputPath.toScala / "init.csv")
      }
    }
    def worldAtTheEnd = Simulation.run(
      maxProbaToSwitch = maxProbaToSwitch,
      constraintsStrength = constraintsStrength,
      inertiaCoefficient = inertiaCoefficient,
      healthyDietReward = healthyDietReward,
      interpersonalInfluence = interpersonalInfluence,
      days = days,
      population = population,
      moves = moves,
      distributionConstraints = distributionConstraints,
      moveType = moveType,
      rng = rng,
      visitor = Some(visit)
    )
    util.writeState(worldAtTheEnd, outputPath.toScala / "final.csv")
  }
}

sealed trait PopType
case object RandomPop extends PopType
case object ObservedPop extends PopType

object SimulationWithMapApp extends App {
  case class Config(
    population: Option[File] = None,
    randomPopulation: Option[File] = None,
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
        .text("seed for the random number generator"),
      opt[Long]('s', "seed")
        .action((x, c) => c.copy(seed = Some(x)))
        .text("seed for the random number generator")
    )
  }
  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      def run(scenario: Int, parameterName: String, seed: Long): Unit = {
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
          ("duo_0703", (0.996, 0.405, 0.787, 0.161, 0.102)),
          ("summer2020", (0.010216504, 0.0, 0.806896825, 0.767755389, 0.790965130)),
          ("hope2020", (0.02571037, 0.00878027, 0.55859533, 0.72271431, 0.6156984))
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
        output.createDirectories()
        val worldFeatures = pop match {
          case RandomPop => config.randomPopulation.get
          case ObservedPop => config.population.get
        }
        val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap(parameterName)
        val world = SimulationWithMap.run(
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          interpersonalInfluence = interpersonalInfluence,
          days = 6,
          population = worldFeatures,
          moves = moves,
          distributionConstraints = distributionConstraints,
          output.toJava,
          moveType,
          rng = rng
        )
        log("population " + world.individuals.length)
        log("delta health: " + observable.deltaHealthByCategory(world, distributionConstraints))
        log("social inequality: " + observable.weightedInequalityRatioBySexAge(world))
      }
      val parameterName = "summer2020"
      val seed = config.seed.getOrElse(42L)
      if (config.scenario.isDefined) run(config.scenario.get, parameterName, seed)
      else {
        for (scenario <- 1 to 5) run(scenario, parameterName, seed)
      }
    case _ =>
  }
}
