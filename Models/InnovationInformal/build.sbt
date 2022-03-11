scalaVersion := "2.13.1"

name := "innovationinformal"

version := "0.1-SNAPSHOT"

//mainClass in (Compile, run) := Some("innovationinformal.")

// model as openmole plugin
enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("innovationinformal.*")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

//libraryDependencies ++= Seq(
//  "org.apache.commons" % "commons-math3" % "3.6.1",
//  "com.github.tototoshi" %% "scala-csv" % "1.3.6"
//)
