package eighties.fiveaday.tools

import better.files.File
import com.github.tototoshi.csv.{CSVParser, defaultCSVFormat}
import eighties.fiveaday.health._
import eighties.fiveaday.observable.weightedInequality
import eighties.h24.social._

object ComputeSocialInequality extends App {
  import HealthMatrix._
  val dataDirectory = File("../data/")
  val generatedData = File("data")
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat.csv"
  val parser = new CSVParser(defaultCSVFormat)
  val header = headers(distributionConstraints)

//  println(Calendar.getInstance.getTime + " loading population")
//  val population = generatedData / "population.bin"
//
//  def worldFeature = WorldFeature.load(population)

  val healthCategory = generateHealthCategory(distributionConstraints.toJava)
//  val seed = 42
//  val rng = new Random(seed)
//
//  val world = generateWorld(worldFeature.individualFeatures, healthCategory, rng)

  for (date <- Seq("1996","2002","2008")) {
    val stats =
      distributionConstraints.lines.drop(1).flatMap(l => parser.parseLine(l)).map {
        cs =>
          val n = cs(header(s"n_$date")).toDouble
          val conso = cs(header(s"conso_5_$date")).toDouble
          AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) ->
            (conso * n, n)
      }.toMap

    val bySA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
//        val high = sexAgeEducation(sex, age, AggregatedEducation.High)(world)
        //      val numberOfHighHealthy = numberOfHealthy(high) + 1
//        val numberOfHigh = high.individuals.length + 1
//        val numberOfHighHealthy = numberOfHigh * stats.get(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.High)).get + 1
        val (numberOfHighHealthy, numberOfHigh) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.High))
//        val low = sexAgeEducation(sex, age, AggregatedEducation.Low)(world)
//        //      val numberOfLowHealthy = numberOfHealthy(low) + 1
//        val numberOfLow = low.individuals.length + 1
//        val numberOfLowHealthy = numberOfLow * stats.get(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Low)).get + 1
        val (numberOfLowHealthy, numberOfLow) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Low))

        val (_, numberOfMiddle) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Middle))
//        println(s"$sex $age for $date numberOfLow = $numberOfLow = $numberOfLowHealthy")
//        println(s"$sex $age for $date numberOfMiddle = $numberOfMiddle = $numberOfMiddleHealthy")
//        println(s"$sex $age for $date numberOfHigh = $numberOfHigh = $numberOfHighHealthy")

//        val all = world.individuals.length
//        val sa = sexAge(sex, age)(world).individuals.length
        val sa = numberOfLow + numberOfMiddle + numberOfHigh

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

    val all = stats.map(_._2._2).sum

    println(s"SI for $date = " + bySA.sum / all + s" with $all")
  }
}
