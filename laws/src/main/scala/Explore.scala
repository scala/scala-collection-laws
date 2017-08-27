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

  def advance(used: Array[Boolean]): Boolean = 
    if (itinerary.isEmpty || used.length != possibilities.length) false
    else {
      val old = itinerary.dequeue
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
          if (j == explore.length) itinerary.enqueue(explore: _*)
          else itinerary.enqueue(java.util.Arrays.copyOf(explore, j): _*)
        }
      }
      itinerary.nonEmpty
    }

  def current: Option[Array[Int]] = if (itinerary.nonEmpty) Some(itinerary.front) else None
}
