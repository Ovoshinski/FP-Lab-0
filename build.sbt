ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.9.0"

name := "lab0"

libraryDependencies ++= Seq(
  //os-lib — работа с файлами и запуск процессов
  "com.lihaoyi" %% "os-lib" % "0.11.7",

  //upickle — работа с JSON
  "com.lihaoyi" %% "upickle" % "4.4.3",

  //munit — тесты
  "org.scalameta" %% "munit" % "1.3.6" % Test
)

Compile / mainClass := Some("lab0.Main")

//Для великого и могучего
Compile / scalacOptions ++= Seq("-encoding", "UTF-8")