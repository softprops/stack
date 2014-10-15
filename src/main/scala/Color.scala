package stack

private[stack] object Color {
  private[this] val colors = (91 to 96).map( i => s"\033[0;${i}m")
  def wheel = new Iterator[String] {
    private[this] def newIterator = util.Random.shuffle(colors).iterator
    @volatile private[this] var it = newIterator
    def hasNext = true
    def next = {
      if (!it.hasNext) it = newIterator
      it.next
    }
  }
}
