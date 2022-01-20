
organization := "eighties"

name := "5aday"

version := "1.0-SNAPSHOT"

scalaVersion := "3.1.0"

crossScalaVersions := Seq("3.1.0")

//val monocleVersion = "3.0.1"

resolvers ++= Seq(
  "osgeo" at "https://repo.osgeo.org/repository/release/",
  "geosolutions" at "https://maven.geo-solutions.it/",
  "geotoolkit" at "https://maven.geotoolkit.org/",
)

resolvers -= DefaultMavenRepository

libraryDependencies ++= Seq (
  "eighties" %% "h24" % "1.0-SNAPSHOT",
  //"com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  //"com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  //"com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,
  "org.apache.commons" % "commons-compress" % "1.11",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.tukaani" % "xz" % "1.6",
  "joda-time" % "joda-time" % "2.9.7",
  "javax.media" % "jai-core" % "1.1.3" from "https://repo.osgeo.org/repository/release/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar",
)
 
excludeDependencies += ExclusionRule("org.typelevel", "cats-kernel_2.13")

scalacOptions ++= Seq(/*"-release:11",*/ "-language:postfixOps")

enablePlugins(SbtOsgi)

updateOptions := updateOptions.value.withGigahorse(false)

osgiSettings

OsgiKeys.exportPackage := Seq("eighties.*;-split-package:=merge-first")
//OsgiKeys.exportPackage := Seq("eighties.fiveaday;-split-package:=merge-first","eighties.fiveaday.run;-split-package:=merge-first","eighties.fiveaday.tools;-split-package:=merge-first")

OsgiKeys.importPackage := Seq("*;resolution:=optional")
//OsgiKeys.importPackage := Seq("")

OsgiKeys.privatePackage := Seq("!java.*,!scala.*,*")

//OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
OsgiKeys.requireCapability := """osgi.ee; osgi.ee="JavaSE";version:List="1.8,1.9""""

//excludeFilter in unmanagedSources := "OpinionMapper.scala" || "MapPopulation.scala" || "worldMapper.scala"

unmanagedSources / excludeFilter := "OpinionMapper.scala"

OsgiKeys.additionalHeaders :=  Map(
  "Specification-Title" -> "Spec Title",
  "Specification-Version" -> "Spec Version",
  "Specification-Vendor" -> "Eighties",
  "Implementation-Title" -> "Impl Title",
  "Implementation-Version" -> "Impl Version",
  "Implementation-Vendor" -> "Eighties"
)

OsgiKeys.embeddedJars := (Compile / Keys.externalDependencyClasspath).value map (_.data) filter (f=> f.getName startsWith "gt-")

//scalacOptions ++= Seq("-deprecation", "-feature")
