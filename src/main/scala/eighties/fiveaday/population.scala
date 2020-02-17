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


import eighties.h24.dynamic.MoveMatrix.TimeSlice
import eighties.h24.generation._
import eighties.h24.space._
import monocle.function.all._
import monocle.macros.Lenses
import eighties.h24.social._


import scala.util.Random

object population {


  /* --------------------------- Heath Category -------------------------- */
  type Opinion = Float

  sealed trait Behaviour
  object Healthy extends Behaviour
  object Unhealthy extends Behaviour

  object Behaviour {
    def fromBoolean(b: Boolean) = if(b) Healthy else Unhealthy
    def toBoolean(b: Behaviour) =
      b match {
        case Healthy => true
        case Unhealthy => false
      }

    def booleanIso = monocle.Iso(fromBoolean)(toBoolean)
  }

  object ChangeConstraints {
    lazy val all =
      for {
        habit <- Array(true, false)
        budget <- Array(true, false)
        time <- Array(true, false)
      } yield ChangeConstraints(habit, budget, time)

    def shortToChangeConstraintsIso = monocle.Iso[Byte, ChangeConstraints](i => all(i - Byte.MinValue))(c => (all.indexOf(c) + Byte.MinValue).toByte)
  }

  @Lenses case class ChangeConstraints(habit: Boolean, budget: Boolean, time: Boolean)

  object HealthCategory {
    def apply(
       opinion: Opinion,
       behaviour: Behaviour,
       changeConstraints: ChangeConstraints) =
      new HealthCategory(opinion, behaviour, ChangeConstraints.shortToChangeConstraintsIso(changeConstraints))
  }

  @Lenses case class HealthCategory(
    opinion: Opinion,
    behaviour: Behaviour,
    changeConstraints: Byte)

  /* ---------------------------- Individual ------------------------------ */


  object Individual {
    def apply(
      feature: IndividualFeature,
      healthCategory: (AggregatedSocialCategory, Random) => HealthCategory,
      random: Random,
      dayDestination: Option[Location] = None): Individual = {
      val socialCategory = AggregatedSocialCategory(feature)

      Individual(
        socialCategory = socialCategory,
        healthCategory = healthCategory(socialCategory, random),
        feature.location,
        feature.location,
        dayDestination
      )
    }

    def apply(
     socialCategory: AggregatedSocialCategory,
     healthCategory: HealthCategory,
     home: Location,
     location: Location,
     dayDestination: Option[Location]
    ): Individual =
      new Individual(
        AggregatedSocialCategory.shortAggregatedSocialCategoryIso(socialCategory),
        healthCategory.opinion,
        Behaviour.toBoolean(healthCategory.behaviour),
        healthCategory.changeConstraints,
        Location.toIndex(home),
        Location.toIndex(location),
        dayDestination.map(Location.toIndex).getOrElse(Location.noLocationIndex))

    def arrayMapIso = monocle.Iso[Array[(TimeSlice, Int)], Map[TimeSlice, Int]](_.toMap)(_.toArray)

    def locationV = Individual.location composeIso Location.indexIso
    def homeV = Individual.home composeIso Location.indexIso
    def socialCategoryV = Individual.socialCategory composeIso AggregatedSocialCategory.shortAggregatedSocialCategoryIso

    def arrayToMapOfStableLocation(array: Array[Short]) =
      (timeSlices zip array).filter(_._2 != Location.noLocationIndex).map { case(a, b) => a -> Location.fromIndex(b) }.toMap
    
    def mapOfStableLocationToArray(map: Map[TimeSlice, Location]) = timeSlices.map(t => map.get(t).map(Location.toIndex).getOrElse(Location.noLocationIndex)).toArray

    def timeSlicesMapIso = monocle.Iso[Array[Short], Map[TimeSlice, Location]] (arrayToMapOfStableLocation) (mapOfStableLocationToArray)

    def stableDestinationsV(i: Individual) = timeSlicesMapIso.get(Array(i.home, i.dayDestination, Location.noLocationIndex))
    def dayDestinationV = Individual.dayDestination composeIso Location.indexIso

    def education = socialCategoryV composeLens AggregatedSocialCategory.education
    def age = socialCategoryV composeLens AggregatedSocialCategory.age
    def sex = socialCategoryV composeLens AggregatedSocialCategory.sex
    def i = Individual.locationV composeLens first
    def j = Individual.locationV composeLens second

    def changeConstraintsV = changeConstraints composeIso ChangeConstraints.shortToChangeConstraintsIso
    def behaviourV = healthy composeIso Behaviour.booleanIso
    def budget = changeConstraintsV composeLens ChangeConstraints.budget
    def habit = changeConstraintsV composeLens ChangeConstraints.habit
    def time = changeConstraintsV composeLens ChangeConstraints.time
  }

  @Lenses case class Individual(
    socialCategory: Byte,
    opinion: Opinion,
    healthy: Boolean,
    changeConstraints: Byte,
    home: Short,
    location: Short,
    dayDestination: Short)

}
