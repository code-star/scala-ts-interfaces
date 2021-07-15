addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")

lazy val logger = ConsoleLogger()

val publishedVersion = "0.5.0"

lazy val scala_tsi_version = {
  val v = sys.env.get("CI") match {
    case Some(_) =>
      logger.info("Running on CI, getting version from -Dplugin.version")
      sys.props.get("plugin.version").getOrElse(throw new IllegalArgumentException("-Dplugin.version not set"))
    case None =>
      sys.props.get("plugin.version").getOrElse(publishedVersion)
  }
  logger.info(
    s"Using scala-tsi $v\t" +
      s"plugin.version=${sys.props.get("plugin.version")}\t" +
      s"published=$publishedVersion\t" +
      s"CI=${sys.props.get("CI")}"
  )
  v
}

addSbtPlugin("com.scalatsi" % "sbt-scala-tsi" % scala_tsi_version)
