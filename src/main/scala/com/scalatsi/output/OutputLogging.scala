package com.scalatsi.output

private[scalatsi] object OutputLogging {
  private def supportsColor = sys.env.get("TERM").exists(_.contains("color"))

  // SLF4J-compatible constants
  final private val ERROR_INT = 40
  final private val WARN_INT  = 30
  final private val INFO_INT  = 20

  /* simple logger interface */
  trait Logger {
    protected def color(level: Int): String
    protected val resetColor: String

    private def log(lvl: Int, msg: String, e: Throwable = null): Unit = {
      Console.err.println(s"${prefix(lvl)}$msg")
      if (e != null) {
        Console.err.print(s"${prefix(lvl)}${color(lvl)}")
        e.printStackTrace()
        Console.err.print(resetColor)
      }
    }

    final def info(msg: String): Unit                       = log(INFO_INT, msg)
    final def warn(msg: String, e: Throwable = null): Unit  = log(WARN_INT, msg, e)
    final def error(msg: String, e: Throwable = null): Unit = log(ERROR_INT, msg, e)

    private def prefix(lvl: Int) = lvl match {
      case INFO_INT  => s"[${color(lvl)}info$resetColor] "
      case WARN_INT  => s"[${color(lvl)}warn$resetColor] "
      case ERROR_INT => s"[${color(lvl)}error$resetColor] "
    }

    final def exit(msg: String, code: Int = 1, e: Throwable = null): Nothing = {
      require(code > 0, "Should exist with a non-zero exit code on failure")
      error(msg, e)
      // This will not stop SBT, and the non-zero exit will mark the task as unsuccessful
      sys.exit(code)
    }
  }

  def logger: Logger = if (supportsColor) new ColorLogger() else new NoColorLogger()

  private class ColorLogger extends Logger {
    final override def color(level: Int): String = level match {
      case ERROR_INT => Console.RED
      case WARN_INT  => Console.YELLOW
      case _         => ""
    }
    final override val resetColor = Console.RESET
  }

  private class NoColorLogger extends Logger {
    final override def color(level: Int): String = ""
    final override val resetColor: String        = ""
  }
}
