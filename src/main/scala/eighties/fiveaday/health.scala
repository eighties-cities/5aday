package eighties.fiveaday

/*
 * Copyright (C) 2019 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import better.files._
import com.github.tototoshi.csv.{CSVParser, defaultCSVFormat}
import eighties.fiveaday.population.{ChangeConstraints, HealthCategory, Healthy, Unhealthy}
import eighties.h24.generation.RasterVariate
import eighties.h24.social.{AggregatedAge, AggregatedEducation, AggregatedSocialCategory, Sex}

import scala.util.Random


object health {

  object HealthMatrix {
    def headers(file: File) = {
      val parser = new CSVParser(defaultCSVFormat)
      parser.parseLine(file.lines.head).get.zipWithIndex.toMap
    }

    def sex(v: String) =
      v match {
        case "1" => Sex.Male
        case "2" => Sex.Female
      }

    def age(v: String) =
      v match {
        case "1" => AggregatedAge.Age1
        case "2" => AggregatedAge.Age2
        case "3" => AggregatedAge.Age3
      }

    def education(v: String) =
      v match {
        case "1" => AggregatedEducation.Low
        case "2" => AggregatedEducation.Middle
        case "3" => AggregatedEducation.High
      }

  }

  def generateHealthCategory(file: java.io.File): (AggregatedSocialCategory, Random) => HealthCategory = {
    import HealthMatrix._
    val parser = new CSVParser(defaultCSVFormat)


    case class CSVLine(
                        consomation2002: Double,
                        habit: Double,
                        budget: Double,
                        time: Double,
                        opinionDistributionH: Vector[Double],
                        opinionDistributionU: Vector[Double])

    val header = headers(file.toScala)

    val stats =
      file.toScala.lines.drop(1).flatMap(l => parser.parseLine(l)).map {
        cs =>
          val indexH1 = header("opinion_index_Hq1")
          val indexH5 = header("opinion_index_Hq5")
          val indexU1 = header("opinion_index_Uq1")
          val indexU5 = header("opinion_index_Uq5")
          val n = cs(header("n_2002")).toDouble
          AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) ->
            CSVLine(
              consomation2002 = cs(header("conso_5_2002")).toDouble,
              habit = cs(header("habit_constraint")).toDouble,
              budget = cs(header("budget_constraint")).toDouble,
              time = cs(header("time_constraint")).toDouble,
              opinionDistributionH = cs.slice(indexH1, indexH5).map(_.toDouble).toVector,
              opinionDistributionU = cs.slice(indexU1, indexU5).map(_.toDouble).toVector
            )
      }.toMap

    (category: AggregatedSocialCategory, random: Random) => {
      val line = stats(category)
      val constraints = ChangeConstraints(budget = random.nextDouble() < line.budget, habit = random.nextDouble() < line.habit, time = random.nextDouble() < line.time)

      val behaviour = if (random.nextDouble() < stats(category).consomation2002) Healthy else Unhealthy

      val distributionH = stats(category).opinionDistributionH
      val distributionU = stats(category).opinionDistributionU
      val distribution = if (behaviour == Healthy) distributionH else distributionU
      val opinion = new RasterVariate(distribution.toArray, Seq(distribution.size)).compute(random).head

      HealthCategory(opinion.toFloat, behaviour, constraints)
    }
  }

  case class Interactions(
                           breakfastInteraction: Double,
                           lunchInteraction: Double,
                           dinnerInteraction: Double)

  def generateInteractionMap(file: java.io.File) = {
    import HealthMatrix._
    val header = headers(file.toScala)
    val parser = new CSVParser(defaultCSVFormat)
    file.toScala.lines.drop(1).flatMap(l => parser.parseLine(l)).map {
      cs =>
        AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) ->
          Interactions(
            breakfastInteraction = cs(header("social_context_breakfast")).toDouble,
            lunchInteraction = cs(header("social_context_lunch")).toDouble,
            dinnerInteraction = cs(header("social_context_dinner")).toDouble)
    }.toMap
  }

}
