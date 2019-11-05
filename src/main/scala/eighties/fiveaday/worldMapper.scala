package eighties.fiveaday

import java.awt.Color
import java.awt.image.BufferedImage

import better.files.File
import eighties.fiveaday.population.Individual
import eighties.h24.space.BoundingBox
import eighties.h24.social.AggregatedEducation
import eighties.h24.space
import org.geotools.coverage.grid.GridCoverageFactory
import org.geotools.gce.geotiff.GeoTiffFormat
import org.geotools.geometry.jts.ReferencedEnvelope
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem
import eighties.h24.tools.math._

object worldMapper {
  val format = new GeoTiffFormat()
  def cat(ind: Individual) = Individual.education.get(ind) match {
    case AggregatedEducation.Low => 0
    case AggregatedEducation.Middle => 1
    case AggregatedEducation.High => 2
  }
  def mapRGB(world: space.World[Individual], file: File, getValue: Individual => Int = cat,
             cellSize: Int = 1000, crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = world.originI
    val minY = world.originJ
    val width = world.sideI
    val height = world.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world, Individual.locationV.get)
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val jj = height - j - 1
      val value = c map getValue
      val size = value.length
      val vec =
        if (size == 0) Array(0,0,0)
        else Array(value.count(_ == 0)*255/size, value.count(_ == 1)*255/size, value.count(_ == 2)*255/size)
      raster.setPixel(i, jj, vec)
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    file.parent.createDirectories()
    format.getWriter(file.toJava).write(coverage, null)
  }
  def mapGray(world: space.World[Individual],
              originalBoundingBox: BoundingBox,
              boundingBox: BoundingBox,
              file: File,
              getValue: Individual => Double,
              atHome: Boolean = true,
              textLeft: String= "",
              textRight: String= "",
              filter: Int => Boolean = _=>true,
              aggregator: Array[Double] => Double = v => v.sum / v.length,
              minValue: Double = 0.0,
              maxValue: Double = 1.0,
              rescale: Boolean = true,
              fraction: Int = 4,
              cellSize: Int = 1000,
              crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
//    val minX = originalBoundingBox.minI
//    val minY = originalBoundingBox.minJ
    val width = boundingBox.sideI
    val height = boundingBox.sideJ
//    val maxX = minX + width
//    val maxY = minY + height
    val rangeValues = maxValue - minValue
    val pixelSize = 10
    val bufferedImage = new BufferedImage(width*pixelSize, height*pixelSize, BufferedImage.TYPE_INT_ARGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world, if (atHome) Individual.homeV.get else Individual.locationV.get)
    val maxOuputValue = Math.pow(2,8) - 1.0
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val ii = i* pixelSize
      val jj = (height - j - 1) * pixelSize
      val values = c map getValue
      val size = values.length
      if (filter(size)) {
        def aggValue = aggregator(values)
        val color = if (size > 0) {
          val lambda = clamp(if (rescale) (aggValue - minValue) / rangeValues else aggValue)
          val v = ((1-lambda) * maxOuputValue).toInt
          Array(v, v, v, 255)
        } else {
          Array(0,0,0,0)
        }
        val vec = Array.fill(pixelSize * pixelSize)(color).flatten
        raster.setPixels(ii, jj, pixelSize, pixelSize, vec)
      }
    }
    if (!textLeft.isEmpty) {
      val g = bufferedImage.getGraphics
      import java.awt.{Color, Font}
      g.setColor(Color.black)
      g.setFont(new Font("Serif", Font.BOLD, 50))
      g.drawString(s"$textLeft", pixelSize, (height - 1) * pixelSize)
      g.drawString(s"$textRight", (width - 20) * pixelSize, (height - 1) * pixelSize)
      (0 to fraction).foreach{v=>
        val d = v.toDouble/fraction.toDouble
        val dv = (1.0 - d).toFloat
        val c = new Color(dv, dv, dv)
        val shift = 5 * pixelSize
        val h = (fraction - v) * shift
        g.setColor(c)
        g.fillRect((width - 20) * pixelSize, h, shift, shift)
        g.setColor(Color.black)
        g.drawString(s"${(minValue + d) * rangeValues}", (width - 14) * pixelSize, h + shift)
      }
    }
    val referencedEnvelope = new ReferencedEnvelope(originalBoundingBox.minI, originalBoundingBox.maxI, originalBoundingBox.minJ, originalBoundingBox.maxJ, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    file.parent.createDirectories
    format.getWriter(file.toJava).write(coverage, null)
  }
  def mapColorRGB(world: space.World[Individual],
                  boundingBox: BoundingBox,
                  file: File,
                  getValue: Individual => Double,
                  filter: Int => Boolean = _=>true,
                  aggregator: Array[Double] => Double = v => v.sum / v.length,
                  minValue: Double = 0.0,
                  maxValue: Double = 1.0,
                  cellSize: Int = 1000,
                  crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = boundingBox.minI
    val minY = boundingBox.minJ
    val width = boundingBox.sideI
    val height = boundingBox.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val rangeValues = maxValue - minValue
    val pixelSize = 10
    val colors = Array(
      (127.0,0.0,0.0),//dark red
      (255.0,0.0,0.0),//red
      (255.0,127.0,0.0),//orange
      (255.0,255.0,0.0),//yellow
      (127.0,255.0,0.0),//yellow-green
      (0.0,255.0,0.0),//green
      (0.0,255.0,255.0),//green-blue
      (0.0,255.0,255.0),//cyan
      (0.0,127.0,255.0),//cyan-blue
      (0.0,0.0,255.0)//blue
    ).reverse
    val bufferedImage = new BufferedImage(width*pixelSize, height*pixelSize, BufferedImage.TYPE_INT_ARGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world, Individual.locationV.get)
    def interpolate(lambda: Double, c1: (Double, Double, Double), c2: (Double, Double, Double)) = {
      Array(
        (1.0 - lambda) * c1._1 + lambda * c2._1,
        (1.0 - lambda) * c1._2 + lambda * c2._2,
        (1.0 - lambda) * c1._3 + lambda * c2._3,
        255)
    }
    def clamp(v: Double) = math.min(math.max(v, 0.0),1.0)
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val ii = i* pixelSize
      val jj = (height - j - 1) * pixelSize
      val values = c map getValue
      val size = values.length
      if (filter(size)) {
        def aggValue = aggregator(values)
        val color = if (size > 0) {
          val value = clamp((aggValue - minValue) / rangeValues) * (colors.length - 1)
          val ind = value.toInt
          val lambda = value - ind
          val c0 = colors(ind)
          val color0 = Array(c0._1, c0._2, c0._3, 255)
          if (ind == colors.length - 1) color0 else interpolate(lambda, colors(ind), colors(ind + 1))
        } else {
          Array(0.0, 0.0, 0.0, 0.0)
        }
        val vec = Array.fill(pixelSize * pixelSize)(color).flatten
        raster.setPixels(ii, jj, pixelSize, pixelSize, vec)
      }
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    file.parent.createDirectories
    format.getWriter(file.toJava).write(coverage, null)
  }
  def mapColorHSV(world: space.World[Individual],
                  boundingBox: BoundingBox,
                  file: File,
                  getValue: Individual => Double,
                  atHome: Boolean = true,
                  textLeft: String= "",
                  textRight: String= "",
                  filter: Int => Boolean = _=>true,
                  aggregator: Array[Double] => Double = v => v.sum / v.length,
                  minValue: Double = 0.0,
                  maxValue: Double = 1.0,
                  cellSize: Int = 1000,
                  crs: CoordinateReferenceSystem = CRS.decode("EPSG:3035")) = {
    val minX = boundingBox.minI
    val minY = boundingBox.minJ
    val width = boundingBox.sideI
    val height = boundingBox.sideJ
    val maxX = minX + width
    val maxY = minY + height
    val rangeValues = maxValue - minValue
    val pixelSize = 10
    val bufferedImage = new BufferedImage(width*pixelSize, height*pixelSize, BufferedImage.TYPE_INT_ARGB)
    val raster = bufferedImage.getRaster
    val index = space.Index.indexIndividuals(world, if (atHome) Individual.homeV.get else Individual.locationV.get)
    val hMax = 240.0/360.0
    def clamp(v: Double) = math.min(math.max(v, 0.0),1.0)
    for {
      (l, i) <- index.cells.zipWithIndex
      (c, j) <- l.zipWithIndex
    } yield {
      val ii = i* pixelSize
      val jj = (height - j - 1) * pixelSize
      val values = c map getValue
      val size = values.length
      if (filter(size)) {
        def aggValue = aggregator(values)
        val color = if (size > 0) {
          val lambda = clamp((aggValue - minValue) / rangeValues)
          Array(((1.0 - lambda) * hMax).toFloat, 1.0f, 1.0f, 255.0f)
        } else {
          Array(0.0f, 0.0f, 0.0f, 0.0f)
        }
        val c = Color.getHSBColor(color(0), color(1), color(2))
        val array = Array(c.getRed, c.getGreen, c.getBlue, color(3).toInt)
        val vec = Array.fill(pixelSize * pixelSize)(array).flatten
        raster.setPixels(ii, jj, pixelSize, pixelSize, vec)
      }
    }
    if (!textLeft.isEmpty) {
      val g = bufferedImage.getGraphics
      import java.awt.{Color, Font}
      g.setColor(Color.black)
      g.setFont(new Font("Serif", Font.BOLD, 50))
      g.drawString(s"$textLeft", pixelSize, (height - 1) * pixelSize)
      g.drawString(s"$textRight", (width - 20) * pixelSize, (height - 1) * pixelSize)
      (0 to 4).foreach{v=>
        val d = v.toDouble/4.0
        val c = Color.getHSBColor(((1.0 - d) * 240 / 360).toFloat, 1f, 1f)
        val shift = 5 * pixelSize
        val h = (4 - v) * shift
        g.setColor(c)
        g.fillRect((width - 20) * pixelSize, h, shift, shift)
        g.setColor(Color.black)
        g.drawString(s"$d", (width - 14) * pixelSize, h + shift)
      }
    }
    val referencedEnvelope = new ReferencedEnvelope(minX * cellSize, maxX * cellSize, minY * cellSize, maxY * cellSize, crs)
    val factory = new GridCoverageFactory
    val coverage = factory.create("GridCoverage", bufferedImage, referencedEnvelope)
    file.parent.createDirectories
    format.getWriter(file.toJava).write(coverage, null)
  }
}
