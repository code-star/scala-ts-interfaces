package nl.codestar.scalatsi.generator

import nl.codestar.scalatsi.output.WriteTSToFiles

object GeneratorApp {
  def main(args: Array[String]): Unit = {
    WriteTSToFiles.write(UserOptions.value)(UserClasses.outputClasses)
  }
}
