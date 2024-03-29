package eighties.fiveaday.tools

import java.util.Calendar

import better.files.File
import eighties.fiveaday.health.generateHealthCategory
import eighties.fiveaday.population.Individual
import eighties.fiveaday.{observable, worldMapper}
import eighties.h24.generation.{IndividualFeature, WorldFeature}
import eighties.h24.space.generateWorld

import scala.util.Random

@main def MapPopulation(args: String*): Unit = {
  val rng = new Random(42)
  val popFile = "population.bin"
  val result = "results"
  def features = WorldFeature.load((File("data") / popFile).toJava)
  println(Calendar.getInstance.getTime.toString + " features")

  val dataDirectory = File("../data")
  val distributionConstraints = dataDirectory / "initialisation_distribution_per_cat.csv"

  val healthCategory = generateHealthCategory(distributionConstraints.toJava)

  def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)
  val world = generateWorld(features.individualFeatures, buildIndividual, Individual.locationV, Individual.homeV, rng)

  println(Calendar.getInstance.getTime.toString + " world")
  val bb = features.originalBoundingBox
  def filter(n:Int) = n>=30
//  worldMapper.mapRGB(world, File("results") / "map.tiff")
  def getHealthyValue(individual: Individual) = if (individual.healthy) 1.0 else 0.0
  def atHome = true
  val socialInequality = observable.weightedInequalityRatioBySexAge(world)

  println(Calendar.getInstance.getTime.toString + s" socialInequality=$socialInequality")

  println(Calendar.getInstance.getTime.toString + " health")
  worldMapper.mapColorHSV(world, bb, File(result) / "health.tiff", getHealthyValue, atHome, socialInequality.toString, "00_0", filter)

  def getOpinionValue(individual: Individual) = individual.opinion
  println(Calendar.getInstance.getTime.toString + " opinion")
  worldMapper.mapColorHSV(world, bb, File(result) / "opinion.tiff", getOpinionValue, atHome, socialInequality.toString, "00_0", filter)

  println(Calendar.getInstance.getTime.toString + " pop")
  def getPop(individual: Individual) = 1.0
  worldMapper.mapColorHSV(world, bb, File(result) / "pop.tiff", getPop,  atHome, socialInequality.toString, "00_0", filter, v=>v.sum, 0.0, 10000.0)
}
