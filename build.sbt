name := "sts_backend"

version := "1.0"

lazy val `sts_backend` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq( jdbc , anorm , cache , ws,
  "org.json4s" %% "json4s-jackson" % "3.2.11"
)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  