package stack

private[stack] trait Logger {
  def print(str: String)
  def println(string: String)
}

private[stack] object Logger {
  def named(names: Set[String]): Map[String, Logger] = {
    val colors = Color.wheel
    val pad = names.map(_.size).max
    names.map { name =>
      val color = colors.next
      (name, new Logger {
        def println(msg: String) = System.out.synchronized {
          System.out.println(
            ("%s %0$" + pad + "s |\033[0m %s").format(color, name, msg))
        }
        def print(msg: String) = System.out.synchronized {
          System.out.print(
            ("\r\033[2K%s %0$" + pad + "s |\033[0m %s").format(color, name, msg))
        }
      })
    }.toMap
  }
}
