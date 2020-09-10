package eighties.fiveaday.tools

import java.util.Calendar

import better.files.File
import com.github.tototoshi.csv.{CSVParser, defaultCSVFormat}
import eighties.fiveaday.observable.{filterIndividualBySexAge, sexAgeEducation, weightedInequality}
import eighties.fiveaday.population.Individual
import eighties.fiveaday.health._
import eighties.h24.social._
import eighties.h24.space.generateWorld
import eighties.h24.generation.{IndividualFeature, WorldFeature}

import scala.util.Random

object ComputeSocialInequalityFromSyntheticPopulation extends App {
  import HealthMatrix._
  val dataDirectory = File("data")
  val generatedData = File("../h24/results_IDF")
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat_2002_2008.csv"
  val parser = new CSVParser(defaultCSVFormat)
  val header = headers(distributionConstraints)

  println(Calendar.getInstance.getTime + " loading population")
  val population = generatedData / "population.bin"

  def worldFeature = WorldFeature.load(population.toJava)

  val healthCategory = generateHealthCategory(distributionConstraints.toJava)
  val seed = 42
  val rng = new Random(seed)

  def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)
  val world = generateWorld(worldFeature.individualFeatures, buildIndividual, Individual.locationV, Individual.homeV, rng)

  for (date <- Seq("2002","2008")) {
    val stats =
      distributionConstraints.lines.drop(1).flatMap(l => parser.parseLine(l)).map {
        cs =>
          val n = cs(header(s"n_$date")).toDouble
          val conso = cs(header(s"conso_5_$date")).toDouble
          AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) ->
            (conso * n) / n
      }.toMap

    val all = world.individuals.length
    val bySA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
        val high = sexAgeEducation(sex, age, AggregatedEducation.High)(world)
        //      val numberOfHighHealthy = numberOfHealthy(high) + 1
        val numberOfHigh = high.individuals.length
        val numberOfHighHealthy = numberOfHigh * stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.High))
//        val (numberOfHighHealthy, numberOfHigh) = stats.get(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.High)).get
        val low = sexAgeEducation(sex, age, AggregatedEducation.Low)(world)
//        //      val numberOfLowHealthy = numberOfHealthy(low) + 1
        val numberOfLow = low.individuals.length
        val numberOfLowHealthy = numberOfLow * stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Low))
//        val (numberOfLowHealthy, numberOfLow) = stats.get(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Low)).get

        val middle = sexAgeEducation(sex, age, AggregatedEducation.Middle)(world)
        val numberOfMiddle = middle.individuals.length
        val numberOfMiddleHealthy = numberOfMiddle * stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Middle))
//        val (numberOfMiddleHealthy, numberOfMiddle) = stats.get(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Middle)).get
        println(s"$sex $age for $date numberOfLow = $numberOfLow = $numberOfLowHealthy")
        println(s"$sex $age for $date numberOfMiddle = $numberOfMiddle = $numberOfMiddleHealthy")
        println(s"$sex $age for $date numberOfHigh = $numberOfHigh = $numberOfHighHealthy")

//        val all = world.individuals.length
        val sa = filterIndividualBySexAge(sex, age)(world).individuals.length
        assert(sa == numberOfLow + numberOfMiddle + numberOfHigh)
//        val sa = numberOfLow + numberOfMiddle + numberOfHigh

//        println(s"$sex $age for $date = " + sa)
        //        println("sex = " + Sex.toCode(sex) + " age = " + AggregatedAge.toCode(age) + " => " + sa + " / " + all + " => " + (sa.toDouble / all))
        //        println("numberOfHighHealthy = " + numberOfHighHealthy + " numberOfHigh = " + numberOfHigh + " => " + (numberOfHighHealthy.toDouble / numberOfHigh))
        //        println("numberOfLowHealthy = " + numberOfLowHealthy + " numberOfLow = " + numberOfLow + " => " + (numberOfLowHealthy.toDouble / numberOfLow))
        //        val v = ((numberOfHighHealthy.toDouble / numberOfHigh) / (numberOfLowHealthy.toDouble / numberOfLow)) *
        //          (sa.toDouble / all)
        //        println("si = " + v)
//        ((numberOfHighHealthy.toDouble / numberOfHigh) / (numberOfLowHealthy.toDouble / numberOfLow)) *
//          (sa.toDouble / all)
        weightedInequality(numberOfHighHealthy, numberOfHigh, numberOfLowHealthy, numberOfLow, sa)
      }

//    val all = stats.map(_._2._2).sum

    println(s"SI for $date = ${bySA.sum / all} with $all")
  }
}
