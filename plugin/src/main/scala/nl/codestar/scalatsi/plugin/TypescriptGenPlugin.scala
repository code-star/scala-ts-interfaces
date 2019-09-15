package nl.codestar.scalatsi.plugin

import sbt.Keys._
import sbt._
import sbt.info.BuildInfo

object TypescriptGenPlugin extends AutoPlugin {
  object autoImport {
    // User settings
    val typescriptClassesToGenerateFor = settingKey[Seq[String]]("Classes to generate typescript interfaces for")
    val typescriptGenerationImports    = settingKey[Seq[String]]("Additional imports (i.e. your packages)")
    val typescriptOutputLocation       = settingKey[File]("Directory or file where all typescript interfaces will be written to")
    val typescriptMapping              = settingKey[Map[String, String]](
      """Translation table of the JVM namespaces to the typescript modules.
        | an entry like `myservice.api -> backend.api`.
        | will emit the "myservice.api.MyClass" JVM class as a "backend.api.MyClass" typescript interface.
      """.stripMargin
    )
    val typescriptIgnoredPrefix        = settingKey[String](
      """A prefix to strip from your prefix/packages.
        | Setting this to `com.company` will emit `com.company.myservice.api` as `myservice.api`.
        | Will be applied before typescriptMapping
      """.stripMargin
    )
    @deprecated("Use typescriptOutputLocation instead", "0.2.0")
    val typescriptOutputFile = typescriptOutputLocation

    val generateTypescript = taskKey[Unit]("Generate typescript for this project")

    val generateTypescriptGeneratorApplication = taskKey[Seq[File]]("Generate an app to generate typescript interfaces")
  }

  import autoImport._

  override def trigger = allRequirements

  private val scala_ts_compiler_version = BuildInfo.version

  /** Default user settings */
  private lazy val defaults = Seq(
    typescriptGenerationImports := Seq(),
    typescriptClassesToGenerateFor := Seq(),
    typescriptOutputLocation := target.value / "typescript-interfaces",
    typescriptMapping := Map(),
    typescriptIgnoredPrefix := "",
  )

  /** Settings for the plugin, should generally not be modified by the user */
  private lazy val pluginSettings = Seq(
    // Add the library to the dependencies
    libraryDependencies += "nl.codestar" %% "scala-tsi" % scala_ts_compiler_version,
    // Task settings
    generateTypescript := runTypescriptGeneration.value,
    generateTypescriptGeneratorApplication in Compile := createTypescriptGenerationTemplate(
      typescriptGenerationImports.value,
      typescriptClassesToGenerateFor.value,
      sourceManaged.value,
      typescriptOutputLocation.value
    ),
    sourceGenerators in Compile += generateTypescriptGeneratorApplication in Compile
  )

  override lazy val projectSettings: Seq[Def.Setting[_]] = defaults ++ pluginSettings

  def createTypescriptGenerationTemplate(
    imports: Seq[String],
    typesToGenerate: Seq[String],
    sourceManaged: File,
    typescriptOutputFile: File
  ): Seq[File] = {
    val targetFile = sourceManaged / "nl" / "codestar" / "scalasti" / "generator" / "ApplicationTypescriptGeneration.scala"

    val toWrite: String = txt
      .generateTypescriptApplicationTemplate(imports, typesToGenerate, typescriptOutputFile.getAbsolutePath)
      .body
      .stripMargin

    IO.write(targetFile, toWrite)
    Seq(targetFile)
  }

  def runTypescriptGeneration: Def.Initialize[Task[Unit]] =
    (runMain in Compile)
      .toTask(" nl.codestar.scalatsi.generator.ApplicationTypescriptGeneration")
      .dependsOn(generateTypescriptGeneratorApplication in Compile)
}
