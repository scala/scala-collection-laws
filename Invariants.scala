package invs

object Invariants {
  def theSame[A](xs: TraversableOnce[A], ys: TraversableOnce[A], ordered: Boolean = false) {
    if (ordered) {
      val b = new collection.mutable.ArrayBuffer[A]
      xs.foreach(a => b += _)
      val i = b.result.iterator
      i.corresponds(ys)(_ == _)
    }
    else {
      val hx, hy = new collection.mutable.HashMap[A, Int]
      xs.foreach(a => hx(a) = hx.getOrElseUpdate(a,0))
      ys.foreach(a => hy(a) = hy.getOrElseUpdate(a,0))
      hx.size == hy.size && hx.forall{ case (k,n) => hy.get(k).exists(_ == n) }
    }
  }
  case class GenTest1[C](title: String, pristine: String, needs: Seq[String], test: String, reusable: Boolean, instance: C) {
    lazy val valid = (instance.getClass.getMethods.map(x => dedollar(x.getName)).toSet & needs.toSet) == needs.toSet
    lazy val code = List(
      s"//$title",
      s"@Test",
      s"def test_${sanitized(title)} {",
      s"  ${if (reusable) "val" else "def"} x = pristine",
      s"  assert{ $test }",
      s"}"
     ).mkString("\n")
  }
  
  /* For consistently orderable collections,
   *   theSame means a.size == b.size and a.foreach(buf += _); i = buf.result.iterator; b.forall(_ == i.next) && !i.hasNext
   * For collections that cannot be consistently ordered,
   *   theSame means a.size == b.size and CountedSet(a) == CountedSet(b)
   */
  
  /* TraversableOnce */
    /* exists(p) iff find(p).isDefined */
    /* forall(p) implies isEmpty || exists(p) */
    /* foreach(x => y |= p(x)) iff exists(p) */
    /* toIterator.size == size */
    /* toIterator.exists(p) iff exists(p) */
    /* toStream.size == size */
    /* toStream.exists(p) iff exists(p) */
    /* toTraversable.size == size */
    /* toTraversable.exists(p) iff exists(p) */
    /* aggregate(z)((b,a) => b, (b1, b2) => b1) == z */
    /* collectFirst(pf).isDefined iff exists(pf.isDefinedAt) */
    /*! copyToArray docs says will not terminate on inf, but it will because array is finite. */
    /*! copyToArray is hard to get working right.  Deprecate?  Or return an Int? */
    /* b.clear; copyToBuffer(b); b.result.size == size */
    /* b.clear; copyToBuffer(b); b.exists(p) iff exists(p) */
    /* count(p) > 0 iff exists(p) */
    /* count(p) == size iff forall(p) */
    /* count(p) == { var y=0; foreach(x => if (p(x)) y += 1); y } */
    /* filter(p).size == count(p) */
    /* filter(p).forall(p) == true */
    /*! Why isn't MonadOps a value class? */
    /* flatMap--monad laws */
    /* fold(z)(_ op _) == foldLeft(z)(_ op _) == foldRight(z)(_ op _) for z: A and commutative op */
    /* map--monad laws */
    /* max == reduce(_ max _) */
    /* maxBy(f) is in the set of x s.t. f(x) is maximal */
    /* min == reduce(_ min _) */
    /* minBy(f) is in the set of x s.t. f(x) is minimal */
    /* nonEmpty iff exists(_ => true) */
    /* product == fold(1)(_ * _) */
    /* reduce(_ op _) == reduceLeft(_ op _) == reduceRight(_ op _) for commutative op */
    /* reduceLeftOption.isDefined iff nonEmpty */
    /* reduceRightOption.isDefined iff nonEmpty */
    /* reduce(_ op _) == reduceLeftOption(_ op _).get == reduceRightOption(_ op _).get for nonEmpty && commutative op */
    /* size == count(_ => true) */
    /* sum == fold(0)(_ + _) */
    /* to[C[A]].exists(p) iff exists(p) */
    /* toArray.size == size */
    /* toArray.exists(p) iff exists(p) */
    /* toBuffer -- same deal, size && exists */
    /* toIndexedSeq -- same deal */
    /* toIterable -- same deal */
    /* toList -- same deal */
    /* toMap -- exists only */
    /* toSeq -- size && exists */
    /* toSet -- exists only */
    /* toVector -- size && exists */
    /* withFilter(p).size == filter(p).size */
    /* withFilter(p).exists(q) iff filter(p).exists(q) */
    
  /* Iterator */
    /* hasNext iff nonEmpty */
    /* hasNext => Try{ next }.isSuccess */
    /* (i ++ j).size == i.size + j.size */
    /* (i ++ j).exists(p) iff i.exists(p) || j.exists(p) */
    /* CountedSet((i ++ j) next i.size times) == CountedSet(i) */
    /* k = i++j; k.next i.size times; CountedSet(k next j.size times) == CountedSet(j) */
    /* buffered -- size && exists */
    /* collect(pf) has same size && exists as filter(pf.isDefinedAt).map(pf) */
    /* contains(x) iff exists(_ == x) */
    /* corresponds(copy-of-self)(identity) == true */
    /* j.size != i.size => i.corresponds(j)(_ => true) == false */
    /* i.corresponds(j)((_, _) => false) iff i.nonEmpty == j.nonEmpty == false */
    /* drop(n).size == 0 max (size-n) */
    /* p(dropWhile(p).next) == false or dropWhile(p).hasNext == false */
    /* (i,j) = duplicate => i.corresponds(j)(identity) */
    /* filterNot(p) size && exists as filter(!p) */
    /* grouped(n).map(_.size).sum == size */
    /* grouped(n).drop(m).size < n => grouped(n).drop(m+1).hasNext == false */
    /*! indexOf only makes sense on Iterators from a source with stable order */
    /*! indexWhere only makes sense on Iterators from a source with stable order */
    /* isEmpty iff !nonEmpty iff hasNext = false */
    /* length == size */
    /* padTo(a, n).corresponds(i.take(n))(identity) if n < size */
    /* padTo(a, n).drop(size).next == a while hasNext */
    /* padTo(a, n).size == n */
    /* (i,j) = partition(p); i.corresponds(filter(p))(identity) && j.corresponds(filterNot(p))(identity) */
    /* patch(n, j, m).take(n).corresponds(take(n))(identity) */
    /* patch(n, j, m).drop(n).take(j.size).corresponds(j)(identity) */
    /* patch(n, j, m).drop(n+j.size).corresponds(i.drop(n+m))(identity) */
    /* i.sameElements(j) iff i.corresponds(j)(identity) */
    /*! Test implementation of scanLeft and scanRight */
    /* slice(n,m).size = 0 max (m-n) */
    /* slice(n,m).corresponds(drop(n).take(m-n))(identity) */
    /* span(p)._1.forall(p) == true */
    /* span(p)._2.take(1).exists(p) == false */
    /* (i,j) = span(p); i.corresponds(take(i.size))(identity) && j.corresponds(drop(i.size))(identity) */
    /* take(n).size = (size min n) */
    /* takeWhile(p).forall(p) == true */
    /* takeWhile(p).size + dropWhile(p).size == size */
    /* i.zip(j).size == i.size min j.size */
    /* i.zip(j).map(_._1).corresponds(i.take(i.zip(j).size))(identity) */
    /* i.zip(j).map(_._2).corresponds(j.take(i.zip(j).size))(identity) */
    /* i.zipAll(j,a,b).size == i.size max j.size */
    /* i.zipAll(j,a,b).map(_._1).corresponds(i.padTo(i.zipAll(j,a,b).size,a))(identity) */
    /* i.zipAll(j,a,b).map(_._2).corresponds(j.padTo(i.zipAll(j,a,b).size,b))(identity) */
    /* i.zipWithIndex.map(_._2).corresponds(0 until i.size)(identity) */
    /* i.zipWithIndex.map(_._2).corresponds(i)(identity) */
    
  /* Traversable */
    /* ++ like Iterator */
    /* a ++: b theSameAs a ++ b */
    /* collect like Iterator */
    /* copyToArray fragment theSameAs self */
    /* drop(n).copyToArray has first n elements the same */
    /* dropWhile(p) theSameAs drop(takeWhile(p).size) */
    /* filterNot like Iterator */
    /* flatten...? */
    /* 
    
  /* Invariant: if you are Iterable and have definite size, your iterator is the same size as you. */
  /* Invariant: if you are TraversableOnce and have definite size, you traverse as many elements as your size. */
  
}
