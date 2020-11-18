package eighties.fiveaday.tools

import better.files.File
import eighties.h24.generation._
import eighties.fiveaday.population.Individual
import eighties.h24.space._
import eighties.fiveaday.observable._
import eighties.fiveaday.health._

import scala.util.Random

object CellCSV extends App {
  val rng = new Random(42)

  def features = WorldFeature.load(File("results/population.bin").toJava)

  val dataDirectory = File("../data/")
  val pathEGT = dataDirectory / "EGT 2010/presence semaine EGT"
  val distributionConstraints = dataDirectory / "initialisation_distribution_par_cat.csv"

  val healthCategory = generateHealthCategory(distributionConstraints.toJava)

  def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)
  def world = generateWorld(features.individualFeatures, buildIndividual, Individual.locationV, Individual.homeV, rng)

  val output = File("results") / "cells.csv"

  saveEffectivesAsCSV(world, output)

}
