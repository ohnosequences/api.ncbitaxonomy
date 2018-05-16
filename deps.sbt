libraryDependencies ++= Seq(
  "ohnosequences" %% "trees" % "0.0.0-33-gb545edb"
) ++ testDependencies

val testDependencies = Seq(
  "org.scalatest" %% "scalatest"       % "3.0.4" % Test,
  "ohnosequences" %% "db-ncbitaxonomy" % "0.0.1"
)
