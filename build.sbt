
organization := "eighties"

name := "5aday"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.10"

crossScalaVersions := Seq("2.12.10", "2.13.1")


//val monocleVersion = "1.5.0-cats"
val monocleVersion = "1.6.0"

//val geotoolsVersion = "21.0
//val breezeVersion = "0.13.2"

resolvers ++= Seq(
  "osgeo" at "https://download.osgeo.org/webdav/geotools/",
  "geosolutions" at "https://maven.geo-solutions.it/",
  "geotoolkit" at "https://maven.geotoolkit.org/",
)

resolvers -= DefaultMavenRepository

libraryDependencies ++= Seq (
  "eighties" %% "h24" % "1.0-SNAPSHOT",
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  //"org.geotools" % "geotools" % geotoolsVersion,
  /*"org.geotools" % "gt-referencing" % geotoolsVersion,
  "org.geotools" % "gt-shapefile" % geotoolsVersion,
  "org.geotools" % "gt-epsg-wkt" % geotoolsVersion,
  "org.geotools" % "gt-cql" % geotoolsVersion,
  "org.geotools" % "gt-geotiff" % geotoolsVersion,
  "org.geotools" % "gt-image" % geotoolsVersion,
  "org.geotools" % "gt-coverage" % geotoolsVersion,
  "org.geotools" % "gt-geojson" % geotoolsVersion,*/
  //"com.github.tototoshi" %% "scala-csv" % "1.3.4",
  "org.apache.commons" % "commons-compress" % "1.11",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.tukaani" % "xz" % "1.6",
  //"com.github.pathikrit" %% "better-files" % "2.17.1",
  //"org.scalanlp" %% "breeze" % breezeVersion,
  //"org.scalanlp" %% "breeze-natives" % breezeVersion,
  //"org.scalanlp" %% "breeze-viz" % breezeVersion,
  //"org.typelevel"  %% "squants"  % "1.1.0",
  "joda-time" % "joda-time" % "2.9.7",
  //"com.thoughtworks.xstream" % "xstream" % "1.4.9",
  //"io.suzaku" %% "boopickle" % "1.2.6",
  //"it.geosolutions.jaiext" % "jaiext" % "1.0.20"
//  "javax.media" % "jai-core" % "1.1.3"  % "runtime" from "https://download.osgeo.org/webdav/geotools/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
//  "javax.media" % "jai_codec" % "1.1.3" % "runtime",
//  "javax.media" % "jai_imageio" % "1.1" % "runtime"
)
 
//addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions ++= {
  scalaBinaryVersion.value match {
    case x if x.startsWith("2.12") => Seq("-target:jvm-1.8")
    case _ => Seq("-target:jvm-1.8", "-language:postfixOps", "-Ymacro-annotations")
  }
}

libraryDependencies ++= {
  scalaBinaryVersion.value match {
    case x if x.startsWith("2.12") => Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
    case _ => Seq()
  }
}

enablePlugins(SbtOsgi)

updateOptions := updateOptions.value.withGigahorse(false)

osgiSettings

OsgiKeys.exportPackage := Seq("eighties.*;-split-package:=merge-first")

OsgiKeys.importPackage := Seq("*;resolution:=optional")
//OsgiKeys.importPackage := Seq("")

OsgiKeys.privatePackage := Seq("!java.*,!scala.*,*")

//OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
OsgiKeys.requireCapability := """osgi.ee; osgi.ee="JavaSE";version:List="1.8,1.9""""

//excludeFilter in unmanagedSources := "OpinionMapper.scala" || "MapPopulation.scala" || "worldMapper.scala"

excludeFilter in unmanagedSources := "OpinionMapper.scala"

OsgiKeys.additionalHeaders :=  Map(
  "Specification-Title" -> "Spec Title",
  "Specification-Version" -> "Spec Version",
  "Specification-Vendor" -> "Eighties",
  "Implementation-Title" -> "Impl Title",
  "Implementation-Version" -> "Impl Version",
  "Implementation-Vendor" -> "Eighties"
)

OsgiKeys.embeddedJars := (Keys.externalDependencyClasspath in Compile).value map (_.data) filter (f=> (f.getName startsWith "gt-"))

// do not use coursier at the moment: it fails on jai_core for some reason
useCoursier := false

