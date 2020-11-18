/**
  * Created by Romain Reuillon on 11/05/16.
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
package eighties.fiveaday

import better.files._
import better.files.Dsl.SymbolicOperations
import com.github.tototoshi.csv.{CSVParser, defaultCSVFormat}
import eighties.fiveaday.health._
import eighties.h24.social._
import eighties.fiveaday.population._
import eighties.h24.space._

object observable {

  def byEducation[T](b: Vector[Float] => T)(world: World[Individual]): Array[(AggregatedAge, T)] =
      for {
        ed <- AggregatedEducation.all
        level = World.individualsVector[Individual].get(world).filter(i => Individual.education.get(i)  == ed)
      } yield ed -> b(level.map(Individual.opinion.get))

//  def resume(world: World) = {
//    import breeze.linalg._
//    import breeze.stats._
//    byEducation[Vector[Float]](b => Vector(mean(b), scala.math.sqrt(variance(DenseVector(b: _*))), median(DenseVector(b: _*))))(world)
//  }

  def filterIndividualBySexAge(sex: Sex, age: AggregatedAge)(world: World[Individual]): World[Individual] =
    World.individuals[Individual].modify(_.filter(i => Individual.sex.get(i) == sex && Individual.age.get(i) == age))(world)

  def sexAgeEducation(sex: Sex, age: AggregatedAge, education: AggregatedEducation)(world: World[Individual]): World[Individual] =
    World.individuals[Individual].modify(_.filter(i => Individual.sex.get(i) == sex && Individual.age.get(i) == age && Individual.education.get(i) == education))(world)

  sealed class HealthCategories(val numberOfHigh: Int, val numberOfHighHealthy: Int, val numberOfMiddle: Int, val numberOfMiddleHealthy: Int, val numberOfLow: Int, val numberOfLowHealthy: Int)
  /**
  E from Erreygers, G. 2009. "Correcting the concentration index, Journal of Health Economics 28:521-524.
   */
  def erreygersE_(cat: HealthCategories, n: Int): Double = {
    def rankHigh = cat.numberOfHigh.toDouble / 2
    def rankMiddle = (2 * cat.numberOfHigh + cat.numberOfMiddle).toDouble / 2
    def rankLow = (2 * cat.numberOfHigh + 2 * cat.numberOfMiddle + cat.numberOfLow).toDouble / 2
    def z(rank: Double) = (n + 1).toDouble / 2 - rank
    8 * (z(rankLow) * cat.numberOfLowHealthy + z(rankMiddle) * cat.numberOfMiddleHealthy + z(rankHigh) * cat.numberOfHighHealthy) / math.pow(n, 2)
  }

  def getCategories(world: World[Individual]): HealthCategories = {
    val high = World.individualsVector[Individual].get(world).filter(i => Individual.education.get(i) == AggregatedEducation.High)
    val middle = World.individualsVector[Individual].get(world).filter(i => Individual.education.get(i) == AggregatedEducation.Middle)
    val low = World.individualsVector[Individual].get(world).filter(i => Individual.education.get(i) == AggregatedEducation.Low)
    val numberOfHighHealthy = high.count(_.healthy)
    val numberOfHigh = high.length
    val numberOfMiddleHealthy = middle.count(_.healthy)
    val numberOfMiddle = middle.length
    val numberOfLowHealthy = low.count(_.healthy)
    val numberOfLow = low.length
    new HealthCategories(numberOfHigh, numberOfHighHealthy, numberOfMiddle, numberOfMiddleHealthy, numberOfLow, numberOfLowHealthy)
  }
  def erreygersE(world: World[Individual]): Double = {
    erreygersE_(getCategories(world), world.individuals.length)
  }

  // def numberOfHealthy(world: World[Individual]) = world.individuals.count(Individual.healthy.get)
  def numberOfHealthy(world: World[Individual]): Int = world.individuals.count(_.healthy)
  def numberOfHealthyV(world: Vector[Individual]): Int = world.count(_.healthy)

  def weightedInequality(numberOfHighHealthy:Double, numberOfHigh:Double, numberOfLowHealthy:Double, numberOfLow:Double, total: Double): Double = {
    val propHighHealthy = if (numberOfHighHealthy == 0) (numberOfHighHealthy + 1) / (numberOfHigh + 1) else numberOfHighHealthy / numberOfHigh
    val propLowHealthy = if (numberOfLowHealthy == 0) (numberOfLowHealthy + 1) / (numberOfLow + 1) else numberOfLowHealthy / numberOfLow
    (propHighHealthy / propLowHealthy) * total
  }
  def weightedInequalityRatioBySexAge(world: World[Individual]): Double = {
    val all = world.individuals.length
    val bySA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
        val high = sexAgeEducation(sex, age, AggregatedEducation.High)(world)
        val numberOfHighHealthy = numberOfHealthy(high)
        val numberOfHigh = high.individuals.length
        val low = sexAgeEducation(sex, age, AggregatedEducation.Low)(world)
        val numberOfLowHealthy = numberOfHealthy(low)
        val numberOfLow = low.individuals.length
        val sa = filterIndividualBySexAge(sex, age)(world).individuals.length
//        ((numberOfHighHealthy.toDouble / numberOfHigh) / (numberOfLowHealthy.toDouble / numberOfLow)) * (sa.toDouble / all)
//        println(s"$sex - $age => high healthy = $numberOfHighHealthy - low healthy = $numberOfLowHealthy (high = $numberOfHigh - low = $numberOfLow) => $sa")
        weightedInequality(numberOfHighHealthy, numberOfHigh, numberOfLowHealthy, numberOfLow, sa)
      }
    bySA.sum / all
  }

  def inequalityRatioBySexAge(world: World[Individual]): Map[(AggregatedAge, AggregatedAge), (Double, Int)] = {
    def numberOfHealthy(world: World[Individual], education: AggregatedEducation) = world.individuals.count(i => Individual.healthy.get(i) && Individual.education.get(i) == education)
    def numberOfIndividuals(world: World[Individual], education: AggregatedEducation) = world.individuals.count(i => Individual.education.get(i) == education)

    val totalNumberOfIndividuals = world.individuals.length

    val bySEA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
        val sae = filterIndividualBySexAge(sex, age)(world)

        val numberOfHealthyEducatedIndividuals = numberOfHealthy(sae, AggregatedEducation.High) + 1
        val numberOfEducatedIndividuals = numberOfIndividuals(sae, AggregatedEducation.High) + 1

        val numberOfHealthyUneducatedIndividuals = numberOfHealthy(sae, AggregatedEducation.Low) + 1
        val numberOfUneducatedIndividuals = numberOfIndividuals(sae, AggregatedEducation.Low) + 1
        val ratioOfRatio = (numberOfHealthyEducatedIndividuals.toDouble / numberOfEducatedIndividuals) / (numberOfHealthyUneducatedIndividuals / numberOfUneducatedIndividuals)
        (sex, age) -> (ratioOfRatio, sae.individuals.length / totalNumberOfIndividuals)
      }
    bySEA.toMap
  }

  def healthyRatioBySexAge(world: World[Individual]): Map[(AggregatedAge, AggregatedAge), (Double, Double)] = {
    val totalNumberOfIndividual = world.individuals.length

    val bySEA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
        val sae = filterIndividualBySexAge(sex, age)(world)
        val numberOfHealthyIndividuals = numberOfHealthy(sae) + 1
        val numberOfIndividuals = sae.individuals.length + 1
        val fractionOfWorld = numberOfIndividuals.toDouble / totalNumberOfIndividual
        (sex, age) -> (numberOfHealthyIndividuals.toDouble / numberOfIndividuals,  fractionOfWorld)
      }
    bySEA.toMap
  }


  def healthyRatioByCategory(world: World[Individual]): Map[AggregatedSocialCategory, (Double, Double)] = {

    def filterIndividuals(category: AggregatedSocialCategory) =
      World.individualsVector[Individual].get(world).filter(i => Individual.socialCategoryV.get(i) == category)
    val totalNumberOfIndividual = world.individuals.length

    val bySEA =
      for {
        category <- AggregatedSocialCategory.all
      } yield {
        val indiviualsOfCategory = filterIndividuals(category)
        val numberOfHealthyIndividuals = indiviualsOfCategory.count(i => Individual.healthy.get(i)) + 1
        val numberOfIndividuals = indiviualsOfCategory.size + 1
        val fractionOfWorld = numberOfIndividuals.toDouble / totalNumberOfIndividual
        category -> (numberOfHealthyIndividuals.toDouble / numberOfIndividuals, fractionOfWorld)
      }
    bySEA.toMap
  }


  def healthyByCategory(world: World[Individual]): Map[AggregatedSocialCategory, Double] = {
    def filterIndividuals(category: AggregatedSocialCategory) =
      World.individualsVector[Individual].get(world).filter(i => Individual.socialCategoryV.get(i) == category)

//    val totalNumberOfIndividual = world.individuals.length
    {
      for {
        category <- AggregatedSocialCategory.all
      } yield {
//        def indiviualsOfCategory = filterIndividuals(category)
//        def numberOfHealthyIndividuals = indiviualsOfCategory.count(i => Individual.healthy.get(i)) + 1
//        def individualsOfCategory = sexAgeEducation(category.sex, category.age, category.education)(world)
        def individualsOfCategory = World.individualsVector[Individual].get(world).filter(Individual.socialCategoryV.get(_) == category)
        def numberOfHealthyIndividuals = numberOfHealthyV(individualsOfCategory)
//        println(s"Category $category => ${individualsOfCategory.size} / $numberOfHealthyIndividuals")
//        val indiviualsOfCategory2 = filterIndividuals(category)
//        val numberOfHealthyIndividuals2 = indiviualsOfCategory2.count(i => Individual.healthy.get(i))
//        val numberOfIndividuals2 = indiviualsOfCategory2.size
//        println(s"\t$numberOfIndividuals2 / $numberOfHealthyIndividuals2")
        category -> numberOfHealthyIndividuals.toDouble
      }
    }.toMap
  }

  def readConso2008(file: File): Map[AggregatedSocialCategory, Double] = {
    import HealthMatrix._

    val parser = new CSVParser(defaultCSVFormat)
    val header = headers(file)

    file.lines.drop(1).flatMap(l => parser.parseLine(l)).map { cs =>
      //val numberOfIndividuals = cs(header("n_" + date)).toInt
      val conso = cs(header("conso_5_2008")).toDouble

      //val socialCategory = AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu"))))
      //val numberOfIndividuals = world.individuals.count(i => Individual.socialCategoryV.get(i) == socialCategory)

      AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) -> conso
    }.toMap
  }


  def behaviourBySocialCategoryInData(file: File, world: World[Individual]): Map[AggregatedSocialCategory, Double] = {
    val map = readConso2008(file)
    val byCat =
      for {
        category <- AggregatedSocialCategory.all
      } yield category -> map(category)
    byCat.toMap
  }

  def behaviourBySexAgeInData(file: File, date: Int, world: World[Individual]): Map[(Sex, AggregatedAge), Double] = {
    def numberOfIndividuals(category: AggregatedSocialCategory) = World.individualsVector[Individual].get(world).count(i => Individual.socialCategoryV.get(i) == category)
    val map = readConso2008(file)
    val bySA =
      for {
      sex <- Sex.all
      age <- AggregatedAge.all
      } yield {
        val values = map.filter(cat => cat._1.sex == sex && cat._1.age == age)

        val categorySize = values.map { case(cat, _) => numberOfIndividuals(cat) }
        val numberOfHealthyAllEdu = (values.values zip categorySize).map { case (conso, n) => conso * n }
        val numberOfIndivdualsAllEdu = categorySize.sum

        val proportionOfHealthyAllEdu = numberOfHealthyAllEdu.sum / numberOfIndivdualsAllEdu

        (sex, age) -> proportionOfHealthyAllEdu
      }
    bySA.toMap
  }

  def inequalityBySexAgeInData(file: File, date: Int, world: World[Individual]): Map[(Sex, AggregatedAge), Double] = {
    val map = readConso2008(file)

    val bySA =
      for {
        sex <- Sex.all
        age <- AggregatedAge.all
      } yield {
        val valuesEducated = map.filter(cat => cat._1.sex == sex && cat._1.age == age && cat._1.education == AggregatedEducation.High).values //map { case (cat, conso) => conso }
        val valuesUneducated = map.filter(cat => cat._1.sex == sex && cat._1.age == age && cat._1.education == AggregatedEducation.Low).values //.map { case (conso, _) => conso }
        val inequalityRatio = valuesEducated.head / valuesUneducated.head

        (sex, age) -> inequalityRatio
      }
    bySA.toMap
  }


  def getConsoByAggregatedSocialCategoy(file: File, date: Int): Map[AggregatedSocialCategory, Double] = {
    import HealthMatrix._
    val parser = new CSVParser(defaultCSVFormat)
    val header = headers(file)
    file.lines.drop(1).flatMap(l => parser.parseLine(l)).map { cs =>
      AggregatedSocialCategory(sex = sex(cs(header("Sex"))), age = age(cs(header("Age"))), education = education(cs(header("Edu")))) ->
        cs(header("conso_5_" + date)).toDouble
    }.toMap
  }

  def deltaHealthByAgeSex(world: World[Individual], file: File, date: Int): Double = {
    lazy val simulated = healthyRatioBySexAge(world)
    def survey = behaviourBySexAgeInData(file, date, world)
    survey.map{
      case (cat, proportionOfHealthyAllEdu) =>
        val (simHealthyRatio, groupWeight) = simulated(cat)
        scala.math.pow(simHealthyRatio - proportionOfHealthyAllEdu, 2.0) * groupWeight
    }.toArray.sum
  }
  def deltaHealthByCategory(world: World[Individual], file: File): Double = {
    lazy val simulated = healthyByCategory(world)
    def survey = behaviourBySocialCategoryInData(file, world)
    def numberOfIndividuals(category: AggregatedSocialCategory) = World.individualsVector[Individual].get(world).count(i => Individual.socialCategoryV.get(i) == category)
    survey.map{
      case (cat, propOfHealthy) =>
        val sim = simulated(cat)
        val num = numberOfIndividuals(cat)
        println(s"${propOfHealthy * num} -- $sim => ${math.abs(propOfHealthy * num - sim)}")
        math.abs(propOfHealthy * num - sim)
    }.toArray.sum
  }

//  def deltaHealth(world: World[Individual]) = {
//    val numberOfHeathy2008 = Seq(
//      36305.538,//11276.434,
//      8805.884,//7999.845,
//      11353.824,//9858.309,
//      91006.248,//88032.482,
//      73594.333,//73687.556,
//      103712.167,//103744.666,
//      76025.221,//74013.450,
//      62327.913,//62604.347,
//      53876.324,//54322.470,
//      61716.522,//18589.743,
//      18455.859,//16630.094,
//      8373.215,//7126.289,
//      120970.496,//116803.309,
//      96996.636,//97040.503,
//      95648.246,//95607.811,
//      120552.568,//117412.814,
//      125029.552,//125236.552,
//      88817.1//89520.300
//    )
//    (numberOfHeathy2008 zip healthyByCategory(world).values).map { case(x, y) =>
//      println(s"$x -- $y => ${math.abs(x - y)}")
//      math.abs(x - y) }.sum
//  }

  def deltaInequality(world: World[Individual], file: File, date: Int): Double = {
    lazy val simulated = inequalityRatioBySexAge(world)
    def survey = inequalityBySexAgeInData(file, date, world)
    survey.map{
      case (cat, inequalityRatio) =>
        val (simHealthyRatio, groupWeight) = simulated(cat)
        scala.math.pow(simHealthyRatio - inequalityRatio, 2.0) * groupWeight

    }.toArray.sum
  }


  def saveEffectivesAsCSV(world: World[Individual], output: File): Unit = {
    output.parent.createDirectories()
    output.delete(swallowIOExceptions = true)

    Index.getLocatedCells(Index.indexIndividuals(world, Individual.locationV.get)).foreach {
      case (c, l) =>
        def numbers = AggregatedSocialCategory.all.map { cat => c.count(i => Individual.socialCategoryV.get(i) == cat)}
        output << s"""${l._1},${l._2},${numbers.mkString(",")}"""
    }
  }

  def moran[T](matrix: Array[Array[T]], quantity: T => Double): Double = {
    def adjacentCells(i: Int, j: Int, size: Int = 1) =
      for {
        oi ← -size to size
        oj ← -size to size
        if i != oi || j != oj
        if i + oi >= 0
        if j + oj >= 0
        if i + oi < matrix.length
        if j + oj < matrix(i + oi).length
      } yield matrix(i + oi)(j + oj)

    def localNeighbourhoodPairs =
      for {
        (cellI, (i, j)) ← zipWithIndices(matrix).flatten
        cellJ ← adjacentCells(i, j)
      } yield (cellI, cellJ, 1.0)

    val flatCells = matrix.toVector.flatten// ?
    val totalQuantity = flatCells.map(quantity).sum
    val averageQuantity = totalQuantity / flatCells.size

    def numerator =
      localNeighbourhoodPairs.map {
        case (cellI, cellJ, weight) ⇒
          val term1 = if (quantity(cellI) == 0) 0.0 else quantity(cellI) - averageQuantity.toDouble
          val term2 = if (quantity(cellJ) == 0) 0.0 else quantity(cellJ) - averageQuantity.toDouble
          weight * term1 * term2
      }.sum

    def denominator =
      flatCells.map {
        cell ⇒
          if (quantity(cell) <= 0) 0
          else math.pow(quantity(cell) - averageQuantity.toDouble, 2)
      }.sum

    val totalWeight = localNeighbourhoodPairs.map { case (_, _, weight) ⇒ weight }.sum

    if (denominator <= 0) 0
    else (flatCells.size.toDouble / totalWeight.toDouble) * (numerator / denominator)
  }
}

object test extends App {
  println(observable.erreygersE_(new observable.HealthCategories(5,5,5,0,5,0), 15))
}