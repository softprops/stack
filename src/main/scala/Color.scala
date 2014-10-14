package stack

object Color {
  private[this] val colors = "\033[0;32m" :: "\033[0;33m" :: "\033[0;34m" :: "\033[0;35m" :: "\033[0;36m" ::  Nil
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
