package eighties.fiveaday.run

import better.files.File
import eighties.fiveaday.population.Individual
import eighties.h24.space.{BoundingBox, World}
import eighties.fiveaday.{observable, worldMapper}

object util {
  def mapHealth(world: World[Individual], obb: BoundingBox, bb: BoundingBox, file: File, textLeft: String, textRight: String, atHome: Boolean = true, maxValue: Double = 1.0, fraction: Int = 4, rescale: Boolean = true) = {
    def getValue(individual: Individual) = if (individual.healthy) 1.0 else 0.0
    worldMapper.mapGray(world, obb, bb, file, getValue, atHome, textLeft, textRight, maxValue = maxValue, fraction = fraction, rescale = rescale)
  }

  def writeState(world: World[Individual], file: File, deltaHealth: Option[Double] = None) = {
    file.parent.createDirectories
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
  }

}
