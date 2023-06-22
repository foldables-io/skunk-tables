// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "io.foldables"
ThisBuild / organizationName := "Foldables"
ThisBuild / startYear := Some(2023)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("chuwy", "Anton Parkhomenko")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

ThisBuild / scalaVersion := "3.3.0"

// Versions
lazy val skunk     = "0.6.0"
lazy val iron      = "2.1.0"
lazy val quotidian = "0.0.6"
lazy val munit     = "0.7.29"
lazy val munitCE   = "1.0.7"

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "skunk-tables",
    libraryDependencies ++= Seq(
      "org.tpolecat"                %% "skunk-core"          % skunk,
      "org.tpolecat"                %% "skunk-circe"         % skunk,

      "io.github.iltotore"          %% "iron"                % iron,
      "io.github.iltotore"          %% "iron-circe"          % iron,

      "io.github.kitlangton"        %% "quotidian"           % quotidian,

      "org.scalameta"               %% "munit"               % munit           % Test,
      "org.typelevel"               %% "munit-cats-effect-3" % munitCE         % Test
    )
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
