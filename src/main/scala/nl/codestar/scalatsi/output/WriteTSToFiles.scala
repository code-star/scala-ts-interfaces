package nl.codestar.scalatsi.output

import java.io.FileWriter

import nl.codestar.scalatsi.TypescriptType.TypescriptNamedType
import nl.codestar.scalatsi.TypescriptTypeSerializer

/** Write the typescript interfaces to files */
object WriteTSToFiles {
  def write(options: OutputOptions)(types: Seq[TypescriptNamedType]): Unit = {
    val output = TypescriptTypeSerializer.emits(types: _*)
    if (!options.targetLocation.exists()) {
      options.targetLocation.createNewFile()
    }
    val writer = new FileWriter(options.targetLocation)
    writer.write(output)
    writer.close()
  }

  // TODO: Write to separate files
  // TODO: Use the translation map
}
