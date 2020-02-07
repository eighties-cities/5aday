package eighties.fiveaday.tools

import better.files.File
import com.github.tototoshi.csv.{CSVParser, defaultCSVFormat}
import eighties.fiveaday.health._
import eighties.fiveaday.observable.weightedInequality
import eighties.h24.social._

object ComputeSocialInequality extends App {
  import HealthMatrix._
  val dataDirectory = File("data/")
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat_2002_2008.csv"
  val parser = new CSVParser(defaultCSVFormat)
  val header = headers(distributionConstraints)

  val healthCategory = generateHealthCategory(distributionConstraints.toJava)

  for (date <- Seq("2002","2008")) {
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
        val (numberOfHighHealthy, numberOfHigh) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.High))
        val (numberOfLowHealthy, numberOfLow) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Low))
        val (_, numberOfMiddle) = stats(AggregatedSocialCategory(sex = sex, age = age, education = AggregatedEducation.Middle))
        val sa = numberOfLow + numberOfMiddle + numberOfHigh
        weightedInequality(numberOfHighHealthy, numberOfHigh, numberOfLowHealthy, numberOfLow, sa)
      }

    val all = stats.map(_._2._2).sum

    println(s"SI for $date = " + bySA.sum / all + s" with $all")
  }
}
