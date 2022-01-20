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
import better.files.FileExtensions
import eighties.fiveaday.health.generateHealthCategory
import eighties.fiveaday.population.Individual
import eighties.h24.generation.{IndividualFeature, WorldFeature}
import eighties.h24.social.AggregatedSocialCategory
import eighties.h24.space.{World, generateWorld}
import eighties.h24.tools.Log.log
import scopt.OParser

import java.io.File
import scala.collection.mutable
import scala.util.Random

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics

object InitStatistics extends App {

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
      programName("5aday Init Statistics"),
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

  def stats(v: Seq[Double]) = {
    val stats = new DescriptiveStatistics
    v.foreach(stats.addValue)
    (stats.getMean,stats.getPercentile(50))
  }

  OParser.parse(parser, args, Config()) match {
    case Some(config) =>
      val seed = config.seed.getOrElse(42L)
      val rng = new Random(seed)

      log("loading population")
      val worldFeatures = config.population.get
//      val moves = config.moves.get
      val distributionConstraints = config.distribution.get
      val outputPath = config.output.get
      val replications = config.replications

      val healthyStats = mutable.Map[AggregatedSocialCategory, Double]()
      val opinionStats = mutable.Map[AggregatedSocialCategory, Double]()
      val opinionMedStats = mutable.Map[AggregatedSocialCategory, Seq[Double]]()
      val healthyOpinionStats = mutable.Map[AggregatedSocialCategory, Double]()
      val healthyOpinionMedStats = mutable.Map[AggregatedSocialCategory, Seq[Double]]()
      val unhealthyOpinionStats = mutable.Map[AggregatedSocialCategory, Double]()
      val unhealthyOpinionMedStats = mutable.Map[AggregatedSocialCategory, Seq[Double]]()
      for {_ <- 0 until replications} {
        val healthCategory = generateHealthCategory(distributionConstraints)
        def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)
        def worldFeature = WorldFeature.load(worldFeatures)
        val world = generateWorld(
          worldFeature.individualFeatures,
          buildIndividual,
          Individual.locationV,
          Individual.homeV, rng)
        AggregatedSocialCategory.all.map { cat =>
          val individualOfCategory = World.individualsVector[Individual].get(world).filter(Individual.socialCategoryV.get(_) == cat)
          val healthyOfCategory = individualOfCategory.filter(_.healthy)
          val unhealthyOfCategory = individualOfCategory.filterNot(_.healthy)
          val categorySize = individualOfCategory.size
          val propHealthy = individualOfCategory.count(_.healthy).toDouble / categorySize
          healthyStats.put(cat, healthyStats.getOrElse(cat, 0.0) + propHealthy)
          val statsOpinion = stats(individualOfCategory.map(_.opinion.toDouble))
          opinionStats.put(cat, opinionStats.getOrElse(cat, 0.0) + statsOpinion._1)
          opinionMedStats.put(cat, opinionMedStats.getOrElse(cat, Seq[Double]()) :+ statsOpinion._2)
          val statsHealthyOpinion = stats(healthyOfCategory.map(_.opinion.toDouble))
          healthyOpinionStats.put(cat, healthyOpinionStats.getOrElse(cat, 0.0) + statsHealthyOpinion._1)
          healthyOpinionMedStats.put(cat, healthyOpinionMedStats.getOrElse(cat, Seq()) :+ statsHealthyOpinion._2)
          val statsUnhealthyOpinion = stats(unhealthyOfCategory.map(_.opinion.toDouble))
          unhealthyOpinionStats.put(cat, unhealthyOpinionStats.getOrElse(cat, 0.0) + statsUnhealthyOpinion._1)
          unhealthyOpinionMedStats.put(cat, unhealthyOpinionMedStats.getOrElse(cat, Seq()) :+ statsUnhealthyOpinion._2)
        }
      }
      // the last simulation: we write this down now
      outputPath.mkdirs()
      // global statistics
      val file = outputPath.toScala / "statistics.csv"
      // headers
      file < Seq("sex","age","education","healthy","opinionAvg","opinionMed",
        "healthyAvgOpinion","healthyMedOpinion","unhealthyAvgOpinion","unhealthyMedOpinion").mkString(",")+"\n"
      AggregatedSocialCategory.all.map { cat =>
        val statsOpinion = stats(opinionMedStats(cat))
        val statsHealthyOpinion = stats(healthyOpinionMedStats(cat))
        val statsUnhealthyOpinion = stats(unhealthyOpinionMedStats(cat))
        file << (Seq(s"${cat.sex}", s"${cat.age}", s"${cat.education}") ++
          Seq(s"${healthyStats(cat) / replications}") ++
          Seq(s"${opinionStats(cat) / replications}") ++
          Seq(s"${statsOpinion._2}") ++
          Seq(s"${healthyOpinionStats(cat) / replications}") ++
          Seq(s"${statsHealthyOpinion._2}") ++
          Seq(s"${unhealthyOpinionStats(cat) / replications}") ++
          Seq(s"${statsUnhealthyOpinion._2}")).mkString(",")
      }
      log("all done!")
    case _ =>
  }
}
