package eighties.fiveaday.tools

import java.util.Calendar

import better.files.File
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, MultiPolygon}
import org.locationtech.jts.index.strtree.STRtree
import eighties.fiveaday.population.{ChangeConstraints, HealthCategory, Healthy, Individual}
import eighties.h24.generation.{IndividualFeature, PolygonSampler, WorldFeature}
import eighties.h24.social.AggregatedSocialCategory
import eighties.h24.space.{Index, generateWorld}
import eighties.h24.tools.random.multinomial
import org.geotools.data.shapefile.{ShapefileDataStore, ShapefileDataStoreFactory}
import org.geotools.data.{DataUtilities, Transaction}

import scala.util.{Random, Try}

object ShapefilePopulationGenerator extends App {
  type Building = (MultiPolygon, Double)
  def index(aFile: File) = {
    val store = new ShapefileDataStore(aFile.toJava.toURI.toURL)
    try {
      val reader = store.getFeatureReader
      try {
        Try {
          val featureReader = Iterator.continually(reader.next).takeWhile(_ => reader.hasNext)
          val index = new STRtree()
          featureReader.foreach { feature =>
            val geom = feature.getDefaultGeometry.asInstanceOf[MultiPolygon]
            val height = feature.getAttribute("HAUTEUR").asInstanceOf[Integer].toDouble
            index.insert(geom.getEnvelopeInternal, (geom,height))
          }
          index
        }
      } finally reader.close()
    } finally store.dispose()
  }
  val inputFileName = "population44.bin"
  val outputFileName = "population44_buildings.shp"
  val buildingFile = File("buildings_laea.shp")
  val path = File("data")
  val outputPath = File("results")
  val seed = 42
  val rng = new Random(seed)
  outputPath.createDirectories
  val outFile = outputPath / outputFileName
  val specs = "geom:Point:srid=3035," +
              "cellX:Integer," +
              "cellY:Integer," +
              "ageCat:String," +
              "sex:String," +
              "education:String"
  val geometryFactory = new GeometryFactory
  val factory = new ShapefileDataStoreFactory
  val dataStore = factory.createDataStore(outFile.toJava.toURI.toURL)
  val featureTypeName = "Object"
  val featureType = DataUtilities.createType(featureTypeName, specs)
  dataStore.createSchema(featureType)
  val typeName = dataStore.getTypeNames()(0)
  val writer = dataStore.getFeatureWriterAppend(typeName, Transaction.AUTO_COMMIT)
  val worldFeature = WorldFeature.load(outputPath / inputFileName)
  println("bbox = " + worldFeature.boundingBox.minI + ", " + worldFeature.boundingBox.minJ )
  println("obbox = " + worldFeature.originalBoundingBox.minI + ", " + worldFeature.originalBoundingBox.minJ )
  println(Calendar.getInstance.getTime + " generating world")
  def healthCategory(a:AggregatedSocialCategory, b:Random) = HealthCategory(0.0f, Healthy,ChangeConstraints(false,false,false))

  def buildIndividual(feature: IndividualFeature, random: Random) = Individual(feature, healthCategory, random)
  val world = generateWorld(worldFeature.individualFeatures, buildIndividual, Individual.locationV, Individual.homeV, rng)

  val bbox = worldFeature.originalBoundingBox
  val indexedWorld = Index.indexIndividuals(world, Individual.homeV.get)
  val locCells = Index.getLocatedCells(indexedWorld)
  println(Calendar.getInstance.getTime + " indexing buidings")
  val buildingIndex = index(buildingFile).get
  println(Calendar.getInstance.getTime + " interating though cells")
  for {
    (features, (i,j)) <- locCells
  } {
    if (features.length > 0) {
      val cellgeom = geometryFactory.createPolygon(Array(
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble + 1.0) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble + 1.0) * 1000.0, (bbox.minJ + j.toDouble + 1.0) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble + 1.0) * 1000.0),
        new Coordinate((bbox.minI + i.toDouble) * 1000.0, (bbox.minJ + j.toDouble) * 1000.0)
      ))
      val relevant = buildingIndex.query(cellgeom.getEnvelopeInternal).toArray.toSeq.
        map(_.asInstanceOf[Building]).filter(_._1.intersects(cellgeom))
      val relevantBuildingVolumes = relevant.map {
        b => {
          val g = b._1.intersection(cellgeom)
          (b, b._2 * g.getArea)
        }
      }.toVector.filter { case (_, v) => v > 0 }
      if (relevantBuildingVolumes.isEmpty) {
        println("NoOOOOOOooooOOO building intersecting the cell")
        println("cell " + i + ", " + j + " = " + cellgeom.toText)
        println("ignoring " + features.length + " features")
        //throw new RuntimeException("NoOOOOOOooooOOO building intersecting the cell")
      } else {
        features.foreach(indiv => {
          val select = multinomial(relevantBuildingVolumes.toArray)(rng)
          val sampler = new PolygonSampler(select._1)
          val p = sampler(rng)
          val codes = AggregatedSocialCategory.toCode(Individual.socialCategoryV.get(indiv))
          val values = Array[AnyRef](
            geometryFactory.createPoint(p),
            Individual.homeV.get(indiv)._1.asInstanceOf[AnyRef],
            Individual.homeV.get(indiv)._2.asInstanceOf[AnyRef],
            codes(1).asInstanceOf[AnyRef],
            codes(0).asInstanceOf[AnyRef],
            codes(2).asInstanceOf[AnyRef]
          )
          val simpleFeature = writer.next
          simpleFeature.setAttributes(values)
          writer.write()
        })
      }
    }
  }
  writer.close()
  dataStore.dispose()
}
