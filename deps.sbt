libraryDependencies ++= Seq(
  "ohnosequences" %% "trees" % "0.0.0-39-gf56d086"
) ++ testDependencies

val testDependencies = Seq(
  "org.scalatest" %% "scalatest"       % "3.0.4" % Test,
  "ohnosequences" %% "db-ncbitaxonomy" % "0.0.1"
)
