package eighties.fiveaday

import eighties.fiveaday.population._
import eighties.h24.space._
import org.apache.commons.math3.util.FastMath
import monocle._

import scala.util.Random

object opinion {

//  def sigmaAdoption(current: Double, all: Vector[Double], sigma: Double, random: Random): Double = {
//    val dist = new Gaussian(0.0, sigma)
//    val d = all.map(x => dist.value(x - current)).toArray
//    val v = new RasterVariate(d, Array(d.length))
//    val index = (v.compute(random)(0) * d.length).toInt
//    all(index)
//  }

  def binomialAdoption(current: Double, all: Vector[Float], gama: Double, random: Random): Double = {
    val other = all(random.nextInt(all.size))
    val distance = math.abs(current - other)

    val p = 1 / FastMath.pow(1 + distance, gama)
    if(random.nextDouble() <= p) other else current
  }

  object InterchangeConviction {

    // C'est validé - Clémentine "Le nouveau code a l'air sensas'"
    def interchangeConvictionInCell(
     cell: Vector[Individual],
     maxProbaToSwitch: Double,
     constraintsStrength: Double,
     inertiaCoefficient: Double,
     healthyDietReward: Double,
     random: Random): Vector[Individual] = {

      def booleanToDouble(b: Boolean) = if(b) 1.0 else 0.0

      def dietReward(individual: Individual) = {
        def opinion = Individual.opinion.get(individual)
//        def getReward(o: Opinion): Opinion =  healthyDietReward.toFloat * o
//        if(Individual.behaviourV.get(individual) == Healthy) getReward(opinion) else 0.0f
        def getReward(o: Opinion): Opinion = math.min(1.0f,(1.0f + healthyDietReward.toFloat) * o)
        if(Individual.behaviourV.get(individual) == Healthy) getReward(opinion) else opinion
      }

      def updateBehaviour(individual: Individual): Individual = {
        def budgetConstraint = booleanToDouble(Individual.budget.get(individual))
        def habitConstraint = booleanToDouble(Individual.budget.get(individual))

        val probaToSwitch =
          Individual.behaviourV.get(individual) match
            case Healthy =>
              val maxY = maxProbaToSwitch - (budgetConstraint * -1.0 * constraintsStrength) - (habitConstraint * -1 * constraintsStrength)
              math.max(0, maxY * (-2 * Individual.opinion.get(individual)) + 1)
            case Unhealthy =>
              val maxY = maxProbaToSwitch - (budgetConstraint * constraintsStrength) - (habitConstraint * constraintsStrength)
              math.max(0, maxY * (2 * Individual.opinion.get(individual)) - 1)


        if (random.nextDouble() < probaToSwitch)
          Individual.behaviourV.modify {
            case Healthy => Unhealthy
            case Unhealthy => Healthy
          }(individual)
        else individual
      }

      // Nb: Clémentine trouve ça clair ! => C'est confirmé
      def updateIndividual(averageOpinion: Double)(individual: Individual) = {
        def newOpinion =
//          math.min(
//            1.0,
//            dietReward(individual) +
//              (inertiaCoefficient * Individual.opinion.get(individual).toDouble +
//                (1 - inertiaCoefficient) * averageOpinion)
//          )
          inertiaCoefficient * dietReward(individual) +
              (1 - inertiaCoefficient) * averageOpinion
        Individual.opinion.set(newOpinion.toFloat)(individual)
      }

      def newCell =
        if(cell.nonEmpty) {
          val averageOpinion = cell.map(Individual.opinion.get).sum.toDouble / cell.size
          cell.map { updateIndividual(averageOpinion) andThen updateBehaviour }
        } else cell

      newCell
    }

  }

  def interchangeConviction(
    world: World[Individual],
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    random: Random): World[Individual] = {

      val newIndividuals = Array.ofDim[Individual](world.individuals.length)
      var index = 0

      for {
        (c, _) <- Index.indexIndividuals(world, Individual.locationV.get).cells.flatten.zipWithIndex
      } {
        //if (i%1000 == 0) println(Calendar.getInstance.getTime + s" conviction in cell $i")
        val newCell = InterchangeConviction.interchangeConvictionInCell(
          c.toVector,
          maxProbaToSwitch,
          constraintsStrength,
          inertiaCoefficient,
          healthyDietReward,
          random)

        for {
          individual <- newCell
        } {
          newIndividuals(index) = individual
          index += 1
        }
    }

    Focus[World[Individual]](_.individuals).set(newIndividuals)(world)
  }
}

