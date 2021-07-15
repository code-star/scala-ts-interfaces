addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")

val pluginVersionParam = sys.props
  .get("plugin.version")
  .getOrElse(sys.error("""|The system property 'plugin.version' is not defined.
                          |Specify this property using the scriptedLaunchOpts -D.""".stripMargin))

addSbtPlugin("com.scalatsi" % "sbt-scala-tsi" % pluginVersionParam)
