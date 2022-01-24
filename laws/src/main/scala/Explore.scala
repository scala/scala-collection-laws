package laws

class Explore(possibilities: Array[Int]) {
  val N = possibilities.foldLeft(1L)(_ * _) match {
    case x if x >= 0 && x < 1024*1024*1024 => x.toInt
    case x                                 => throw new IllegalArgumentException(f"Too many options: $x")
  }

  val visiting = new collection.mutable.BitSet(N)
  val itinerary = collection.mutable.Queue(Array.fill(possibilities.length)(0))
  visiting += 0

  def index(ixs: Array[Int]) = 
    if (ixs.length != possibilities.length) -1
    else {
      var n = 0L
      var m = 1L
      var k = 0
      while (k < ixs.length) {
        n += ixs(k)*m
        m *= possibilities(k)
        k += 1
      }
      assert(n >= 0 && n < N)
      n.toInt
    }

  def advance(used: Array[Boolean]): Boolean = {
    if (used.length != possibilities.length)
      throw new IllegalArgumentException(f"Expected ${possibilities.length} entries, got ${used.length}")

    if (itinerary.isEmpty) false
    else {
      val old = itinerary.dequeue()
      var i = 0
      var n = 0
      while (i < used.length) { if (used(i)) n += 1 ; i += 1 }
      if (n > 0) {
        val explore = new Array[Array[Int]](2*n)
        var j = 0
        i = 0
        while (i < used.length) {
          if (used(i)) {
            if (old(i) > 0) {
              val another = old.clone
              another(i) -= 1
              val ix = index(another)
              if (!visiting(ix)) {
                visiting += ix
                explore(j) = another
                j += 1
              }
            }
            if (old(i)+1 < possibilities(i)) {
              val another = old.clone
              another(i) += 1
              val ix = index(another)
              if (!visiting(ix)) {
                visiting += ix
                explore(j) = another
                j += 1
              }
            }
          }
          i += 1
        }
        if (j > 0) {
          if (j == explore.length) itinerary.enqueueAll(explore)
          else itinerary.enqueueAll(java.util.Arrays.copyOf(explore, j))
        }
      }
      itinerary.nonEmpty
    }
  }

  def current: Option[Array[Int]] = if (itinerary.nonEmpty) Some(itinerary.front) else None

  override def toString = current match {
    case Some(xs) => f"Visited ${visiting.size} with ${itinerary.size} pending\n  ${xs.map(x => "%2d".format(x)).mkString(" ")}"
    case None     => f"Visited ${visiting.size} with ${itinerary.size} pending"
  }
}


/** Indicates that there is some set of parameters to be explored */
trait Exploratory[A] { self =>
  def sizes: Array[Int]

  final def explore(): Explore = new Explore(sizes)

  final def completeIterator(): Iterator[A] = {
    val e = explore()
    val b = Array.fill(sizes.length)(true)
    new collection.AbstractIterator[A] {
      private[this] var maybeA: Option[A] = None
      @annotation.tailrec def hasNext =
        maybeA.isDefined || (e.itinerary.nonEmpty && e.advance(b) && { maybeA = e.current.flatMap(self.lookup); hasNext })
      def next() =
        if (!hasNext) throw new NoSuchElementException("Empty exploratory iterator")
        else {
          val ans = maybeA.get
          maybeA = None
          ans
        }
    }
  }

  protected def validate(ixs: Array[Int]): Boolean =
    if (ixs.length != sizes.length) false
    else {
      var i = 0
      while (i < ixs.length) {
        if (ixs(i) < 0 || ixs(i) >= sizes(i)) return false
        i += 1
      }
      true
    }

  def lookup(ixs: Array[Int]): Option[A]
  final def lookup(e: Explore): Option[A] = e.current.flatMap{ ixs => lookup(ixs) }

  def map[B](f: A => B): Exploratory[B] = new Exploratory[B] {
    def sizes = self.sizes
    def lookup(ixs: Array[Int]) = self.lookup(ixs).map(f)
  }
}

