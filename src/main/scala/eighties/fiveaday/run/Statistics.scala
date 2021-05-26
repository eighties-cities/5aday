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

import better.files.Dsl.SymbolicOperations
import better.files.{FileExtensions, File => ScalaFile}
import eighties.fiveaday.observable
import eighties.fiveaday.observable.sexAgeEducation
import eighties.fiveaday.population.Individual
import eighties.h24.generation.timeSlices
import eighties.h24.simulation._
import eighties.h24.social.{AggregatedAge, AggregatedEducation, AggregatedSocialCategory, Sex}
import eighties.h24.space._
import eighties.h24.tools.Log.log
import scopt.OParser

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Statistics extends App {

  case class Config(
    population: Option[File] = None,
    moves: Option[File] = None,
    distribution: Option[File] = None,
    seed: Option[Long] = None,
    output: Option[File] = None,
    replications: Int = 100)

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("5aday Statistics"),
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
        .optional()
        .action((x, c) => c.copy(seed = Some(x)))
        .text("seed for the random number generator"),
      opt[File]('o', "output")
        .required()
        .action((x, c)=> c.copy(output = Some(x)))
        .text("output directory"),
      opt[Int]('r', "replications")
        .required()
        .action((x, c) => c.copy(replications = x))
        .text("number of replications")
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
      val outputPath = config.output.get
      val replications = config.replications

      val parameterMap = Map(
        ("environment", (0.08433503404492204, 0.14985650295919095, 0.7333227248982435, 0.03291749337039018, 0.23398559843386524)),
        ("partner", (0.029443016524418164, 0.05415637081735669, 0.20870694702023695, 0.10353942953774942, 0.7233067964061539)),
        ("aout", (1.0, 0.894687222564233, 0.0, 1.0, 0.985952008896386)),
        ("calibration", (0.9999372894598938, 3.780104482210688E-5, 0.1391139744735069, 0.10973200449784659, 0.9011227599761806)),
        ("calibration_fitness", (0.8507843893208267, 0.45377746673575825, 0.6585498777924014, 0.210784861364803, 0.2589547233574915)),
        ("ose", (1.0, 0.12454546, 0.66766742, 0.26597869, 0.24032422)),
        ("duo_0703", (0.996, 0.405, 0.787, 0.161, 0.102)),
        ("summer2020", (0.010216504, 0.0, 0.806896825, 0.767755389, 0.790965130)),
        ("hope2020", (0.02571037, 0.00878027, 0.55859533, 0.72271431, 0.6156984)),
        ("valentine2021",(1.0, 0.209712299284924, 0.80338439724895, 0.190788311771708, 0.0166381262141282))
      )

      val (maxProbaToSwitch, constraintsStrength, inertiaCoefficient, healthyDietReward, interpersonalInfluence) = parameterMap("valentine2021")

      val days = 6

      var categoryStats = Map[AggregatedSocialCategory, Int]()
      val dayStats = mutable.Map[AggregatedSocialCategory, Double]()
      val eveningStats = mutable.Map[AggregatedSocialCategory, Double]()
      val globalHealthStats = Array.fill(days, timeSlices.length, 6)(ArrayBuffer[Double]())
      val categoryHealthStats = Array.fill(days, timeSlices.length, 4)(mutable.Map[AggregatedSocialCategory, ArrayBuffer[Double]]())
      def visit(world: World[Individual], obb: BoundingBox, gridSize: Int, option: Option[(Int, Int)]): Unit = {
        def percentageOfIndividualsNotAtHome(world: World[Individual], map: mutable.Map[AggregatedSocialCategory, Double]) =
          AggregatedSocialCategory.all.foreach { cat =>
            val agents = sexAgeEducation(cat.sex, cat.age, cat.education)(world)
            val v = map.getOrElse(cat, 0.0) + (agents.individuals.count(ind => ind.location != ind.home).toDouble/agents.individuals.length)
            map.put(cat, v)
          }
        option match {
          case Some((day, slice)) =>
            if (slice == 1) {
              // the day time slice
              percentageOfIndividualsNotAtHome(world, dayStats)
            }
            if (slice == 2) {
              // the evening time slice
              percentageOfIndividualsNotAtHome(world, eveningStats)
            }
            AggregatedSocialCategory.all.foreach { cat =>
              def individualOfCategory = World.individualsVector[Individual].get(world).filter(Individual.socialCategoryV.get(_) == cat)
              util.vectorStats(individualOfCategory).zipWithIndex.foreach{
                case (value, ind) =>
                  val map = categoryHealthStats(day)(slice)(ind)
                  val v = map.getOrElse(cat, ArrayBuffer())
                  v+=value
                  map.put(cat,v)
              }
//              getCategoryFile(outputPath, cat) << s"""$index,$day,$slice,${Sex.toCode(cat.sex)},${AggregatedAge.toCode(cat.age)},${AggregatedEducation.toCode(cat.education)},${util.vectorStats(individualOfCategory).mkString(",")}"""
            }
            util.vectorStats(world.individuals).zipWithIndex.foreach{ case (value, ind) => globalHealthStats(day)(slice)(ind)+=value }
            globalHealthStats(day)(slice)(4)+=observable.weightedInequalityRatioBySexAge(world)
            globalHealthStats(day)(slice)(5)+=observable.erreygersE(world)
//            SimulationWithMap.writeFileByCategory(outputPath, day, slice, world, categories.toJava, soc, e)
          case None => // the first simulation... nothing to do
            if (categoryStats.isEmpty)
              categoryStats = AggregatedSocialCategory.all.map{cat=>cat -> sexAgeEducation(cat.sex, cat.age, cat.education)(world).individuals.length}.toMap
            util.writeState(world, outputPath.toScala / "init.csv")
        }
      }

      for {_ <- 0 until replications}
        Simulation.run(
          maxProbaToSwitch = maxProbaToSwitch,
          constraintsStrength = constraintsStrength,
          inertiaCoefficient = inertiaCoefficient,
          healthyDietReward = healthyDietReward,
          interpersonalInfluence = interpersonalInfluence,
          days = days,
          population = worldFeatures,
          moves = moves,
          distributionConstraints = distributionConstraints,
          moveType = MoveType.Data,
          rng = rng,
          Some(visit)
        )
      // the last simulation: we write this down now
      outputPath.mkdirs()
      // global statistics
      val file = outputPath.toScala / "statistics.csv"
      // headers
      file < Seq("sex","age","education","nb agents", "avg % different day cell", "avg % different evening cell\n").mkString(",")
      AggregatedSocialCategory.all.map { cat =>
        file << (Seq(s"${cat.sex}", s"${cat.age}", s"${cat.education}", s"${categoryStats(cat)}") ++
          Seq(s"${dayStats(cat) / (replications * days)}") ++
          Seq(s"${eveningStats(cat) / (replications * days)}")).mkString(",")
      }
      // health file
      val globalHealthFile = outputPath.toScala / "health.csv"
      globalHealthFile < "index,day,slice,effective,healthy,ratio,avgOpinion,socialInequality,e\n"
      def getCategoryFile(cat: AggregatedSocialCategory) = {
        outputPath.toScala / s"${Sex.toCode(cat.sex)}_${AggregatedAge.toCode(cat.age)}_${AggregatedEducation.toCode(cat.education)}.csv"
      }
      AggregatedSocialCategory.all.foreach { cat =>
        def f = getCategoryFile(cat)
        f < "index,day,slice,sex,age,educ,effective,healthy,ratio,avgOpinion\n"
      }
      for {
        day <- (0 until days)
        slice <- (0 until timeSlices.length)
      } {
        val index = day * 3 + slice
        //${categoryStats.values.sum} and globalHealthStats(day)(slice)(0) should be the same
        globalHealthFile <<
          s"""$index,$day,$slice,${categoryStats.values.sum},
             |${globalHealthStats(day)(slice)(1).sum/replications},
             |${globalHealthStats(day)(slice)(2).sum/replications},
             |${globalHealthStats(day)(slice)(3).sum/replications},
             |${globalHealthStats(day)(slice)(4).sum/replications},
             |${globalHealthStats(day)(slice)(5).sum/replications}""".stripMargin.replaceAll("\n", "")
        AggregatedSocialCategory.all.foreach { cat =>
          def f = getCategoryFile(cat)
          f <<
            s"""$index,$day,$slice,${Sex.toCode(cat.sex)},${AggregatedAge.toCode(cat.age)},${AggregatedEducation.toCode(cat.education)},
               |${categoryStats(cat)},
               |${categoryHealthStats(day)(slice)(1)(cat).sum/replications},
               |${categoryHealthStats(day)(slice)(2)(cat).sum/replications},
               |${categoryHealthStats(day)(slice)(3)(cat).sum/replications}""".stripMargin.replaceAll("\n", "")
        }
      }

      // parameters
      val parameters = outputPath.toScala / "parameters.csv"
      parameters < "maxProbaToSwitch,constraintsStrength,inertiaCoefficient,healthyDietReward,interpersonalInfluence\n"
      parameters << s"$maxProbaToSwitch,$constraintsStrength,$inertiaCoefficient,$healthyDietReward,$interpersonalInfluence"

      log("all done!")
    case _ =>
  }

}


