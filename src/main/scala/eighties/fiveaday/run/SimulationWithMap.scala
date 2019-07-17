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

import java.util.Calendar

import better.files._
import eighties.h24.dynamic.MoveMatrix._
import eighties.h24.dynamic.{MoveMatrix, _}
import eighties.h24.generation._
import eighties.fiveaday.opinion.interchangeConviction
import eighties.fiveaday.population._
import eighties.fiveaday.run.Fit.fitness
import eighties.h24.space._
import eighties.fiveaday.observable
import eighties.h24.{dynamic, space}
import eighties.fiveaday.health._
import eighties.h24.simulation.MoveType
import eighties.h24.social._

import scala.annotation.tailrec
import scala.util.Random

object FitWithMap {
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
    rng: Random) = {

    println(Calendar.getInstance.getTime + " loading population")
    def worldFeature = WorldFeature.load(File(population.toURI)) //generatedData / "population.bin")

    val healthCategory = generateHealthCategory(File(distributionConstraints.toURI))
    val interactionMap = generateInteractionMap(File(distributionConstraints.toURI))

    println(Calendar.getInstance.getTime + " compute bounding box")
    val bbox = worldFeature.originalBoundingBox

    def locatedCell: LocatedCell = (timeSlice: TimeSlice, i: Int, j: Int) => MoveMatrix.loadCell(File(moves.toURI), timeSlice, i, j)

    /*
    def mapOpinion(world: World, bb: BoundingBox, file: File, atHome: Boolean = true) = {
      def getValue(individual: Individual) = individual.opinion
      val socialInequality = observable.socialInequality(world)
      worldMapper.mapColorHSV(world, bb, file, getValue, atHome, socialInequality.toString)
    }
    */
    val categories = File(outputPath.toURI) / "health.csv"
    categories.parent.createDirectories
    categories < "index,day,slice,effective,healthy,ratio,avgOpinion,socialInequality,e\n"
    def getCategoryFile(cat: AggregatedSocialCategory) = {
      File(outputPath.toURI) / s"${Sex.toCode(cat.sex)}_${AggregatedAge.toCode(cat.age)}_${AggregatedEducation.toCode(cat.education)}.csv"
    }
    AggregatedSocialCategory.all.foreach { cat =>
      def f = getCategoryFile(cat)
      f < "index,day,slice,sex,age,educ,effective,healthy,ratio,avgOpinion\n"
    }

    def writeFileByCategory(day: Int, slice: Int, world: World[Individual], file: File, socialInequality: Double, e: Double) = {
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
      file << s"""$index,$day,$slice,$size,$nbHealthy,$ratio,$avgOpinion,$socialInequality,$e"""
    }

    val parameters = File(outputPath.toURI) / "parameters.csv"
    parameters < "maxProbaToSwitch,constraintsStrength,inertiaCoefficient,healthyDietReward,interpersonalInfluence\n"
    parameters << s"$maxProbaToSwitch,$constraintsStrength,$inertiaCoefficient,$healthyDietReward,$interpersonalInfluence"

    def simulateOneDay(world: space.World[Individual], bb: BoundingBox, timeSlices: List[TimeSlice], locatedCell: LocatedCell, day: Int, slice: Int = 0): World[Individual] =
      timeSlices match {
        case Nil => world
        case time :: t =>
          println(Calendar.getInstance.getTime + " simulate day " + day +  ", time slice " + time)
          def moved = moveType match {
            case MoveType.Data => dynamic.moveInMoveMatrix(world, locatedCell, time, Individual.stableDestinationsV.get, Individual.locationV, Individual.homeV.get, Individual.socialCategoryV.get, rng)
            case MoveType.Random => dynamic.randomMove(world, time, 1.0, Individual.locationV, Individual.stableDestinationsV.get, rng)
            case MoveType.No => world
          }
          val convicted = interchangeConviction(
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
          val soc = observable.weightedInequalityRatioBySexAge(convicted)
          val e = observable.erreygersE(convicted)
          util.mapHealth(convicted, bb, File(outputPath.toURI) / "home" / f"$day%02d_$slice.tiff", soc.toString, f"$day%02d_$slice", maxValue = 0.5, fraction = 5)
          util.mapHealth(convicted, bb, File(outputPath.toURI) / "location" / f"$day%02d_$slice.tiff", soc.toString, f"$day%02d_$slice", atHome = false, maxValue = 0.5, fraction = 5)
          writeFileByCategory(day, slice, convicted, categories, soc, e)
          //mapOpinion(convicted, bb, File(outputPath.toURI) / "opinion" / "home" / f"${day}%02d_${slice}.tiff")
          //mapOpinion(convicted, bb, File(outputPath.toURI) / "opinion" / "location" / f"${day}%02d_${slice}.tiff", false)
          simulateOneDay(convicted, bb, t, locatedCell, day, slice + 1)
      }

    def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, rng)
    val worldInit = generateWorld(worldFeature.individualFeatures, buildIndividual, Individual.locationV, Individual.homeV, rng)

    val populationWithMoves = moveType match {
      case MoveType.Data =>
        val fixedDay =  assignRandomDayLocation(worldInit, locatedCell, Individual.stableDestinationsV, Individual.locationV.get, Individual.homeV.get, Individual.socialCategoryV.get, rng)
        assignFixNightLocation(
          fixedDay,
          Individual.stableDestinationsV,
          Individual.homeV.get
        )
      case MoveType.Random => assignFixNightLocation(worldInit, Individual.stableDestinationsV, Individual.homeV.get)
      case MoveType.No => assignFixNightLocation(worldInit, Individual.stableDestinationsV, Individual.homeV.get)
    }

    util.writeState(populationWithMoves, File(outputPath.toURI) / "init.csv")
    println(Calendar.getInstance.getTime + " run simulation")

    //mapHealth(world, bbox, File(outputPath.toURI) / "health" / "home" / f"00_0.tiff", soc.toString, f"00_0")
    //mapHealth(world, bbox, File(outputPath.toURI) / "health" / "home" / f"00_0.tiff", soc.toString, f"00_0", false)
    //mapOpinion(world, bbox, File(outputPath.toURI) / "opinion" / "home" / f"00_0.tiff")
    //mapOpinion(world, bbox, File(outputPath.toURI) / "opinion" / "location" / f"00_0.tiff", false)

    @tailrec def simulateAllDays(day: Int, world: World[Individual]): World[Individual] =
      if(day == days) world
      else {
        def newWorld = simulateOneDay(world, bbox, timeSlices.toList, locatedCell, day)
        simulateAllDays(day + 1, newWorld)
      }

    val worldAtTheEnd = simulateAllDays(0, populationWithMoves)
    println(Calendar.getInstance.getTime + " finished simulation for " + days + " days")
    val deltaHealth = fitness(worldAtTheEnd)
    util.writeState(worldAtTheEnd, File(outputPath.toURI) / "final.csv", Some(deltaHealth))
    deltaHealth
  }

  def loadMatrix(data: File) = data.lines.drop(1)
}



sealed trait PopType
case object RandomPop extends PopType
case object ObservedPop extends PopType

object SimulationWithMapApp extends App {

  def apply(scenario: Int, parameterName: String, seed: Long) {
    val rng = new Random(seed)

    val generatedData = File("data")
    val dataDirectory = File("../data/")
    val moves = generatedData / "moves"
    val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat_2002_2008.csv"

    val scenarioMap = Map(
      (1, (RandomPop, MoveType.No)),
      (2, (RandomPop, MoveType.Random)),
      (3, (ObservedPop, MoveType.No)),
      (4, (ObservedPop, MoveType.Random)),
      (5, (ObservedPop, MoveType.Data))
    )

    val parameterMap = Map(
      ("environment", (0.08433503404492204, 0.14985650295919095, 0.7333227248982435, 0.03291749337039018, 0.23398559843386524)),
      ("partner", (0.029443016524418164, 0.05415637081735669, 0.20870694702023695, 0.10353942953774942, 0.7233067964061539)),
      ("aout", (1.0, 0.894687222564233, 0.0, 1.0, 0.985952008896386)),
      ("calibration", (0.9999372894598938, 3.780104482210688E-5, 0.1391139744735069, 0.10973200449784659, 0.9011227599761806)),
      ("calibration_fitness", (0.8507843893208267, 0.45377746673575825, 0.6585498777924014, 0.210784861364803, 0.2589547233574915)),
      ("ose", (1.0, 0.12454546, 0.66766742, 0.26597869, 0.24032422))
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

    val output = File(s"results_${parameterName}_Scenario${scenario}_${popName}_${moveTypeString}_$seed")
    output.createDirectories

    val worldFeatures = pop match {
      case RandomPop => generatedData / "population_random.bin"
      case ObservedPop => generatedData / "population.bin"
    }
    val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap(parameterName)

    FitWithMap.run(
      maxProbaToSwitch = maxProbaToSwitch,
      constraintsStrength = constraintsStrength,
      inertiaCoefficient = inertiaCoefficient,
      healthyDietReward = healthyDietReward,
      interpersonalInfluence = interpersonalInfluence,
      days = 6,
      population = worldFeatures.toJava,
      moves = moves.toJava,
      distributionConstraints = distributionConstraints.toJava,
      output.toJava,
      moveType,
      rng = rng
    )
  }
//  val scenario = 1
  val parameterName = "ose"
  val seed = 42
//  SimulationWithMapApp(5, "calibration", seed)
//  SimulationWithMapApp(5, parameterName, seed)
  for (scenario <- 1 to 5) SimulationWithMapApp(scenario, parameterName, seed)
}
