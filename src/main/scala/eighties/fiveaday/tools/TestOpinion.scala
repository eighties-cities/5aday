package eighties.fiveaday.tools

import java.awt.Color

import better.files.File
import eighties.h24.dynamic.MoveMatrix.TimeSlice
import eighties.fiveaday.health._
import eighties.fiveaday.opinion.InterchangeConviction
import eighties.fiveaday.population._
import eighties.h24.space.Location
import better.files.Dsl.SymbolicOperations

import eighties.h24.social._
import scala.util.Random

object OpinionFunction {

  def individual(opinion: Float, behaviour: Behaviour) =
    Individual(
      socialCategory = AggregatedSocialCategory.all.head,
      healthCategory = HealthCategory(opinion, behaviour, ChangeConstraints(true, true, true)),
      home = (0, 0),
      location = (0, 0),
      stableDestinations = Map.empty[TimeSlice, Location]
    )


  def balanced = Vector.fill(50)(individual(0.9f, Healthy)) ++ Vector.fill(50)(individual(0.1f, Unhealthy))
  def unhealthyMajority = Vector.fill(10)(individual(0.9f, Healthy)) ++ Vector.fill(90)(individual(0.1f, Unhealthy))
  def healthyMajority = Vector.fill(90)(individual(0.9f, Healthy)) ++ Vector.fill(10)(individual(0.1f, Unhealthy))
  def balancedMiddle = Vector.fill(50)(individual(0.5f, Healthy)) ++ Vector.fill(50)(individual(0.5f, Unhealthy))
  def random(rng: Random) = Vector.fill(100)(individual(rng.nextFloat, if (rng.nextBoolean) Healthy else Unhealthy))

  def interact(cell: Vector[Individual], steps: Int, rng: Random, file: File, colors: String) = {
    var currentStep = 0
    def step(cell: Vector[Individual]) = InterchangeConviction.interchangeConvictionInCell(
      cell,
      timeOfDay = 1,
      interactions = Map(AggregatedSocialCategory.all.head -> Interactions(1.0, 1.0, 1.0)),
      maxProbaToSwitch = 1.0,
      constraintsStrength = 0.0,
      inertiaCoefficient = 0.5,
      healthyDietReward = 0.1,
      interpersonalInfluence = 0.001,
      rng
    )
    def write(cell: Vector[Individual]) = {
      file << s"""$currentStep,${cell.map(_.opinion).mkString(",")},$colors"""
      currentStep = currentStep + 1
      cell
    }
    Iterator.iterate(cell)(step).map(write).drop(steps).next()
  }
}

object TestOpinion extends App {
  import OpinionFunction._

  val rng = new Random(42)
  val cell = random(rng)
  def color(v: Double) = {
    val color = Color.getHSBColor(((1.0 - v) * 240.0 / 360.0).toFloat,1f,1f)
    (color.getRed*65536 + color.getGreen*256 + color.getBlue).toString
  }
  val colors = cell.map(i=>color(i.opinion)).mkString(",")
  val file = File("results") / "opinions.csv"
  if (file.exists) file.delete()
  file.parent.createDirectories()
  val lastCell = interact(cell, 9999, rng, file, colors)
  println(cell.count(Individual.healthy.get) + " " + cell.map(Individual.opinion.get).sum / 100)
  println(lastCell.count(Individual.healthy.get)+ " " + lastCell.map(Individual.opinion.get).sum / 100)
}
