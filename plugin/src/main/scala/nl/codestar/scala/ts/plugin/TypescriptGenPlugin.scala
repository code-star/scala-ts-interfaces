package nl.codestar.scala.ts.plugin

import sbt.Keys._
import sbt.{Def, _}

object TypescriptGenPlugin extends AutoPlugin {
  object autoImport {
    val generateTypescript = TaskKey[Unit]("generateTypescript", "Generate typescript this project")
    val generateTypescriptGeneratorApplication = TaskKey[Seq[File]]("generateTypescriptGeneratorApplication", "Generate an application that will generate typescript from the classes that are configured")
    val typescriptClassesToGenerateFor = SettingKey[Seq[String]]("Classes to generate typescript interfaces for")
    val typescriptGenerationImports = SettingKey[Seq[String]]("Additional imports (i.e. your packages so you don't need to prefix your classes)")
    //val inputDirectory = SettingKey[File]("typescript-input-directory")
    val typescriptOutputFile = SettingKey[File]("File where all typescript interfaces will be written to")
  }

  import autoImport._

  override def trigger = allRequirements
  // Do we need this?
  //override def `requires`: Plugins = JvmPlugin

  lazy val typescriptSettings: Seq[Def.Setting[_]] =
    Seq(
      generateTypescript := runTypescriptGeneration.value,
      typescriptGenerationImports := Seq(),
      typescriptClassesToGenerateFor := Seq(),
      generateTypescriptGeneratorApplication := createTypescriptGenerationTemplate(typescriptGenerationImports.value, typescriptClassesToGenerateFor.value, sourceManaged.value, typescriptOutputFile.value),
      sourceGenerators += generateTypescriptGeneratorApplication in Compile
    )

  override lazy val projectSettings =
    inConfig(Compile)(typescriptSettings)

  def createTypescriptGenerationTemplate(imports: Seq[String], typesToGenerate: Seq[String], sourceManaged: File, typescriptOutputFile: File): Seq[File] = {
    val targetFile = sourceManaged / "nl" / "codestar" / "scala" / "ts" / "generator" / "ApplicationTypescriptGeneration.scala"

    println(s"Going to write dummy scala file to ${targetFile.absolutePath}")

    val towrite: String = txt.generateTypescriptApplicationTemplate(imports, typesToGenerate, typescriptOutputFile.getAbsolutePath).body.stripMargin

    IO.write(targetFile, towrite)
    Seq(targetFile)
  }

  def runTypescriptGeneration: Def.Initialize[Task[Unit]] =
    (runMain in Compile)
      .toTask(" nl.codestar.scala.ts.generator.ApplicationTypescriptGeneration")
      .dependsOn(generateTypescriptGeneratorApplication in Compile)
}
