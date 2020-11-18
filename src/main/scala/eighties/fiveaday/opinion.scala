package eighties.fiveaday

import eighties.fiveaday.health._
import eighties.fiveaday.population._
import eighties.h24.social._
import eighties.h24.space._
import org.apache.commons.math3.util.FastMath

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
    def interchangeConvictionInCell(
     cell: Vector[Individual],
     timeOfDay: Int,
     interactions: Map[AggregatedSocialCategory, Interactions],
     maxProbaToSwitch: Double,
     constraintsStrength: Double,
     inertiaCoefficient: Double,
     healthyDietReward: Double,
     interpersonalInfluence: Double,
     random: Random): Vector[Individual] = {

      def booleanToDouble(b: Boolean) = if(b) 1.0 else 0.0

      def interactionProbability(individual: Individual) = timeOfDay match {
        case 0 => interactions(Individual.socialCategoryV.get(individual)).breakfastInteraction
        case 1 => interactions(Individual.socialCategoryV.get(individual)).lunchInteraction
        case 2 => interactions(Individual.socialCategoryV.get(individual)).dinnerInteraction
      }

      def peering(cell: Vector[Individual]): (Vector[(Individual, Individual)], Vector[Individual]) = {
        val (interactingPeople, passivePeople) = cell.partition { individual => random.nextDouble() < interactionProbability(individual) }
        def randomizedInteractingPeople = random.shuffle(interactingPeople)
        val (couples, singles) = randomizedInteractingPeople.grouped(2).toVector.partition(_.size == 2)
        (couples.map { case Vector(i1, i2) => (i1, i2) }, passivePeople ++ singles.flatten)
      }

      def dietRewardOpinion(individual: Individual) = {
        def opinion = Individual.opinion.get(individual)
        def getReward(o: Opinion): Opinion =  math.min(1.0, (1.0 + healthyDietReward) * o).toFloat
        if(Individual.behaviourV.get(individual) == Healthy) getReward(opinion) else opinion
      }

      def interactingOpinion(ego: Individual, partner: Option[Individual]): Opinion = partner.map(i => Individual.opinion.get(i)).getOrElse(Individual.opinion.get(ego))

      def passiveOpinion(individual: Individual, healthRatio: Option[Double]): Opinion = healthRatio.map(_.toFloat).getOrElse(Individual.opinion.get(individual))

      def updateBehaviour(individual: Individual): Individual = {
        
        def probaSwitchToUnhealthy = {
          val y =
            maxProbaToSwitch +
              booleanToDouble(Individual.budget.get(individual)) * constraintsStrength -
              booleanToDouble(Individual.habit.get(individual)) * constraintsStrength

          val d = math.max(0.0, -2.0 * Individual.opinion.get(individual) + 1.0)
          math.max(0.0, y * d)
        }

        def probaSwitchToHealthy = {
          val y =
            maxProbaToSwitch -
              booleanToDouble(Individual.budget.get(individual)) * constraintsStrength -
              booleanToDouble(Individual.habit.get(individual)) * constraintsStrength

          val d = math.max(0.0, 2.0 * Individual.opinion.get(individual) - 1.0)
          math.max(0.0, y * d)
        }

        Individual.behaviourV.modify {
          case Healthy => if (random.nextDouble() < probaSwitchToUnhealthy) Unhealthy else Healthy
          case Unhealthy => if (random.nextDouble() < probaSwitchToHealthy) Healthy else Unhealthy
        }(individual)
      }

      // Nb: Clémentine trouve ça clair ! => C'est confirmé
      def updateIndividual(individual: Individual, partner: Option[Individual], healthyRatio: Option[Double]/*, interactions: Map[Individual, Individual]*/) = {
        def a = inertiaCoefficient * dietRewardOpinion(individual)
        def b = interpersonalInfluence * interactingOpinion(individual, partner)
        def c = (1 - interpersonalInfluence) * passiveOpinion(individual, healthyRatio)
        Individual.opinion.set((a + (1 - inertiaCoefficient) * (b + c)).toFloat)(individual)
      }

      def newCell = {
        // Optimized version of Individual.behaviourV.get(i) == Healthy
        val healthyRatio = if(cell.nonEmpty) Some(cell.count(i => i.healthy).toDouble / cell.size) else None
        val (interactingPeople, _) = peering(cell)
//
//        interactingPeople.flatMap {
//          case (i1, i2) => Vector(updateIndividual(i1, Some(i2), healthyRatio), updateIndividual(i2, Some(i1), healthyRatio))
//        } ++ passivePeople.map(i => updateIndividual(i, None, healthyRatio))
//
        val interactions = (interactingPeople ++ interactingPeople.map(_.swap)).toMap
        cell.map(i => updateIndividual(i, interactions.get(i), healthyRatio)).map(updateBehaviour)
      }

      newCell
    }
  }

  def interchangeConviction(
    world: World[Individual],
    timeOfDay: Int,
    interactions: Map[AggregatedSocialCategory, Interactions],
    maxProbaToSwitch: Double,
    constraintsStrength: Double,
    inertiaCoefficient: Double,
    healthyDietReward: Double,
    interpersonalInfluence: Double,
    random: Random): World[Individual] = {

    val newIndividuals = Array.ofDim[Individual](world.individuals.length)
    var index = 0

    for {
      (c, _) <- Index.indexIndividuals(world, Individual.locationV.get).cells.flatten.zipWithIndex
    } {
      //if (i%1000 == 0) println(Calendar.getInstance.getTime + s" conviction in cell $i")

      val newCell = InterchangeConviction.interchangeConvictionInCell(
        c.toVector,
        timeOfDay,
        interactions,
        maxProbaToSwitch,
        constraintsStrength,
        inertiaCoefficient,
        healthyDietReward,
        interpersonalInfluence,
        random)

      for {
        individual <- newCell
      } {
        newIndividuals(index) = individual
        index += 1
      }
    }

    World.individuals.set(newIndividuals)(world)
  }
}

