package eighties.fiveaday.run

import better.files.Dsl.SymbolicOperations
import better.files.File
import com.github.tototoshi.csv.{CSVReader, defaultCSVFormat}
import eighties.fiveaday.population.Individual
import eighties.fiveaday.{observable, worldMapper}
import eighties.h24.space.{BoundingBox, World}

import scala.collection.immutable.Seq
import scala.io.Source

object util {
  def getParameterMap = {
    val reader = CSVReader.open(Source.fromResource("parameters.csv"))(defaultCSVFormat)

    def toTuple(map: Map[String, String]) = (
      map("maxProbaToSwitch").toDouble,
      map("constraintsStrength").toDouble,
      map("inertiaCoefficient").toDouble,
      map("healthyDietReward").toDouble,
      map("interpersonalInfluence").toDouble)

    reader.allWithHeaders().map { map => map("Name") -> toTuple(map) }.toMap
  }
  def mapHealth(world: World[Individual], obb: BoundingBox, width: Int, height: Int, file: File, textLeft: String, textRight: String, atHome: Boolean = true, maxValue: Double = 1.0, fraction: Int = 4, rescale: Boolean = true): Unit = {
    def getValue(individual: Individual) = if (individual.healthy) 1.0 else 0.0
    worldMapper.mapGray(world, obb, width, height, file, getValue, atHome, textLeft, textRight, maxValue = maxValue, fraction = fraction, rescale = rescale)
  }
  def mapHealthDiff(world1: World[Individual], world2: World[Individual], obb: BoundingBox, width: Int, height: Int, file: File, textLeft: String, textRight: String, atHome: Boolean = true, maxValue: Double = 1.0, fraction: Int = 4, rescale: Boolean = true): Unit = {
    def getValue(individual: Individual) = if (individual.healthy) 1.0 else 0.0
    worldMapper.mapGrayDiff(world1, world2, obb, width, height, file, getValue, atHome, textLeft, textRight, maxValue = maxValue, fraction = fraction, rescale = rescale)
  }
  def mapOpinion(world: World[Individual], obb: BoundingBox, file: File, atHome: Boolean = true, textLeft: String, textRight: String, maxValue: Double = 1.0): Unit = {
    def getValue(individual: Individual) = individual.opinion
    worldMapper.mapColorHSV(world, obb, file, getValue, atHome, textLeft, textRight, maxValue = maxValue)
  }

  def writeState(world: World[Individual], file: File, deltaHealth: Option[Double] = None): World[Individual] = {
    file.parent.createDirectories()
    file < "effective,healthy,high,highHealty,middle,middleHealthy,low,lowHealthy,socialInequality,e" + (deltaHealth match {
      case None => "\n"
      case Some(_) => ",deltaHealth\n"
    })
    val size = World.individualsVector[Individual].get(world).size
    val nbHealthy = World.individualsVector[Individual].get(world).count(_.healthy)
    val soc = observable.weightedInequalityRatioBySexAge(world)
    val cat = observable.getCategories(world)
    val e = observable.erreygersE_(cat, size)
    file << s"""$size,$nbHealthy,${cat.numberOfHigh},${cat.numberOfHighHealthy},${cat.numberOfMiddle},${cat.numberOfMiddleHealthy},${cat.numberOfLow},${cat.numberOfLowHealthy},$soc,$e""" + (deltaHealth match {
      case None => ""
      case Some(d) => s""",$d"""
    })
    world
  }

  def vectorStats(category: Seq[Individual]): Seq[Double] =
    if (category.isEmpty) List(0, 0, 0.0, 0.0)
    else {
      val categorySize = category.size
      val nbHealthy = category.count(_.healthy)
      val avgOpinion = category.map(_.opinion.toDouble).sum / categorySize
      Seq(categorySize, nbHealthy, nbHealthy.toDouble / categorySize, avgOpinion)
    }
}
