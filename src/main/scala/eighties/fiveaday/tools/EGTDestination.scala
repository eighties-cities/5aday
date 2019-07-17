package eighties.fiveaday.tools

import better.files.File
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
import eighties.h24.dynamic.MoveMatrix
import eighties.h24.dynamic.MoveMatrix.TimeSlices
import eighties.h24.generation.WorldFeature
import eighties.h24.space.BoundingBox
import org.geotools.data.shapefile.ShapefileDataStoreFactory
import org.geotools.data.{DataUtilities, Transaction}

/**
  */
object EGTDestination extends App {
  def flowDestinationsFromEGT(bb: BoundingBox, matrix: TimeSlices, res: File) = {
    val factory = new ShapefileDataStoreFactory
    val geomfactory = new GeometryFactory
    val file = DataUtilities.urlToFile(res.toJava.toURI.toURL)
    val dataStoreRes = factory.createDataStore(res.toJava.toURI.toURL)
    val featureTypeNameRes = "Res"
    val specsRes = "geom:Point:srid=3035"
    val featureTypeRes = DataUtilities.createType(featureTypeNameRes, specsRes)
    dataStoreRes.createSchema(featureTypeRes)
    val typeNameRes = dataStoreRes.getTypeNames()(0)
    val writerRes = dataStoreRes.getFeatureWriterAppend(typeNameRes, Transaction.AUTO_COMMIT)
    MoveMatrix.allMoves.getAll(matrix).map { v =>
      val loc = MoveMatrix.Move.location.get(v)
      val x = (bb.minI + loc._1) * 1000 + 500.0
      val y = (bb.minJ + loc._2) * 1000 + 500.0
      val valuesRes = Array[AnyRef](geomfactory.createPoint(new Coordinate(x, y)))
      val simpleFeatureRes = writerRes.next
      simpleFeatureRes.setAttributes(valuesRes)
      writerRes.write
    }
    writerRes.close
  }
  val outputPath = File("results")
  outputPath.createDirectories()
  val outFileRes = outputPath / "TEST_DEST_IDW.shp"
  def bb = WorldFeature.load(File("results/population.bin")).originalBoundingBox
  println(bb.minI + " " + bb.minJ)
  val moveTimeLapse = MoveMatrix.load(outputPath / "matrix.bin")

  flowDestinationsFromEGT(bb, moveTimeLapse, outFileRes)
}
