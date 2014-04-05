package laws

import collection.mutable.Builder

object Parsing {
  // Read what methods are needed (names appear between backticks), or remove the backticks
  val NeedReg = "`[^`]+`".r
  def readNeeds(s: String) = NeedReg.findAllIn(s).map(x => x.slice(1,x.length-1)).toSet
  def fixNeeds(s: String) = NeedReg.replaceAllIn(s, _.toString.drop(1).dropRight(1))
  
  // Find which variables to create, or what flags apply, or remove all that and just retain code
  def readCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(! _.last.isUpper).filter(_.length > 0).toSet | Set("x") else Set("x")
  }
  def readFlags(s: String): Set[String] = {
    val i = s.indexOf("...")
    if (i >= 0) s.take(i).split(" ").filter(_.last.isUpper).filter(_.length > 0).toSet else Set()
  }
  def fixCalls(s: String) = {
    val i = s.indexOf("...")
    if (i >= 0) s.drop(i+3).dropWhile(_ == ' ') else s
  }
  
  // Perform substitution of @-prefixed strings (caps only)
  val ReplReg = "@[A-Z]+".r
  def fixRepls(ss: Seq[String], mm: Map[String, Set[String]]): Seq[Seq[String]] = {
    val wildcards = ss.flatMap(s => ReplReg.findAllIn(s).map(_.tail)).toSet | Set("A", "CC", "ONE", "ZERO")
    var maps = Seq(Map[String,String]())
    wildcards.map(s => s -> mm(s)).foreach{ wc =>
      val (k,vs) = wc
      val us = vs.toList
      val old = maps
      maps = maps.map(m => (m + (k -> us.head)))
      if (us.tail.nonEmpty) {
        maps = maps ++ us.tail.flatMap(u => old.map(m => (m + (k -> u))))
      }
    }
    maps = maps.filter(_.size > 0)
    if (maps.isEmpty) Seq(ss)
    else maps.map(m => ss.map{ s => 
      val t = ReplReg.replaceAllIn(s, rm => m(rm.toString.tail))
      if (t contains '@') ReplReg.replaceAllIn(t, rm => m(rm.toString.tail)) else t
    })
  }

  // Use double line breaks as a delimiter; might be a transformed version of the code,
  // so provides a "blank" method to detect where empty lines are.
  def groupDblLineBreak[A](ls: Seq[A], soFar: List[Seq[A]] = Nil)(blank: A => Boolean): List[Seq[A]] = {
    if (ls.isEmpty) soFar.map(_.dropWhile(blank).reverse.dropWhile(blank).reverse).filter(_.nonEmpty)
    else {
      val i = ls.sliding(2).takeWhile(!_.forall(blank)).size
      val (good, bad) = ls.splitAt(i+1)
      groupDblLineBreak(bad, good :: soFar)(blank)
    }
  }
  
  // Paired delimiters that we recognize
  val paired = "()[]{}<>".grouped(2).map(x => x(0) -> x(1)).toMap
  
  // Finds paired delimiters (matches outermost type only, nesting okay)
  // Will skip whitespace.  Otherwise delimiter must be bare (no preceding text),
  // or no delimiter is assumed and it just breaks on whitespace.
  def pairedIndices(s: String, i: Int = 0): Option[(Int,Int)] = {
    if (i >= s.length) None
    else paired.get(s(i)) match {
      case None => 
        if (s(i).isWhitespace) pairedIndices(s, i+1)
        else Some((i, s.indexWhere(_.isWhitespace, i+1)-1))
      case Some(r) =>
        val l = s(i)
        var depth = 1
        var j = i+1
        while (j < s.length && depth > 0) {
          val c = s(j)
          if (c==r) depth -= 1
          else if (c==l) depth += 1
          j += 1
        }
        if (depth==0) Some((i,j-1)) else None
    }
  }
  
  // Splits on whitespace but respects delimiters (outermost type only, but may be embedded in a symbol)
  // Will also respect single and double-quoted strings 'foo bar' and "baz quux".
  def delimSplit(s: String, from: Int = 0, found: Builder[String,Vector[String]] = Vector.newBuilder[String]): Seq[String] = {
    var i = from
    while (i < s.length && s(i).isWhitespace) i += 1
    if (i >= s.length) found.result
    else {
      var j = i
      while (j < s.length && !s(j).isWhitespace) {
        if (paired.contains(s(j)) || s(j) == '"' || s(j) == '\'') {
          var k = j+1
          val cl = s(j)
          val cr = paired.get(s(j)).getOrElse(cl)
          var depth = 1
          while (k < s.length && depth > 0) {
            val c = s(k)
            if (c==cr) depth -= 1
            else if (c==cl) depth += 1
            k += 1
          }
          j = k
        }
        else j += 1
      }
      found += s.substring(i,j)
      delimSplit(s, j, found)
    }
  }
}

// Encapsulates a named substitution pattern
// E.g. Substitution("$X", "<<", ">>") would be able to transform "$X(3)" into "$X<<3>>"
case class Substitution(key: String, lhs: String, rhs: String) {
  import Parsing._
  // Perform this substitution on string s (and add prefix)
  def sub(s: String, prefix: String): String = if (s.isEmpty) prefix else {
    val i = s.indexOfSlice(key)
    if (i < 0) prefix + s
    else {
      pairedIndices(s, i+key.length) match {
        case None => throw new IllegalArgumentException(s"Could not match $key in $s")
        case Some((il, ir)) =>
          if (ir < 0) throw new IllegalArgumentException(s"Could not find right limit of parameter for $key in $s")
          val (i0,i1) = if (paired contains s(il)) (il+1,ir) else (il,ir+1)
          sub(s.drop(ir+1), prefix + s.substring(0,i) + lhs + s.slice(i0,i1) + rhs)
      }
    }
  }
  // Perform substitution without adding a prefix
  def sub(s: String): String = sub(s,"")
}
object Substitution {
  import Parsing._
  // Make a substitution from a string where the variable part should be inserted in place of $
  def from(key: String, subst: String) = {
    val i = subst.indexOfSlice(" $ ")
    if (i < 0) throw new IllegalArgumentException("Could not find \" $ \" in " + subst)
    val j = subst.indexOfSlice(" $ ",i+3)
    if (j >= 0) throw new IllegalArgumentException("Found more than one \" $ \" in " + subst)
    new Substitution(key, subst.substring(0,i), subst.substring(i+3))
  }
}
