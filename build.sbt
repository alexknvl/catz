//val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
//val discipline = "org.typelevel" %% "discipline" % "0.7.3" % "test"
//val scalatest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
val simulacrum = "com.github.mpilquist" %% "simulacrum" % "0.10.0"

lazy val commonSettings = List(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  //addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  organization := "com.alexknvl",
  version := "0.0.1",
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.1",
  // scalaVersion := "2.12.0",
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  scalacOptions ++= List(
    "-deprecation", "-unchecked", "-feature",
    "-encoding", "UTF-8",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-Ypartial-unification",
    "-Ykind-polymorphism",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Xfuture"),
  resolvers ++= List(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")),
  wartremoverWarnings ++= Warts.all
)

lazy val base = (project in file("base"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-base",
    libraryDependencies += simulacrum)

lazy val bifunctor = (project in file("bifunctor"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-bifunctor")

lazy val category = (project in file("category"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-category",
    libraryDependencies += simulacrum)
  .dependsOn(base, bifunctor)

lazy val algebra = (project in file("algebra"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-algebra",
    libraryDependencies += simulacrum)
  .dependsOn(base, category)

lazy val monad = (project in file("monad"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-monad",
    libraryDependencies += simulacrum)
  .dependsOn(base, category, algebra)

lazy val comonad = (project in file("comonad"))
  .settings(commonSettings: _*)
  .settings(
    name := "catz-comonad",
    libraryDependencies += simulacrum)
  .dependsOn(base, category, algebra)

lazy val profunctor = (project in file("profunctor"))
  .settings(name := "catz-profunctor")
  .settings(commonSettings: _*)
  .settings(libraryDependencies += simulacrum)
  .dependsOn(base, category, algebra)

lazy val root = (project in file("."))
  .settings(name := "catz")
  .settings(commonSettings: _*)
  .aggregate(base, category, algebra, monad, comonad, profunctor)
