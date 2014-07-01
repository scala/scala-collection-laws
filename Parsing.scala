package laws

import annotation.tailrec
import collection.mutable.Builder

object Parsing {
  // Numbered lines, either bare or of the the form left-sep-right
  // left is spaces and alphabetic only, plus !
  // sep is a nonempty separator that does not start with whitespace or an alphabetic character
  // right is a list of things, split however one wishes.
  case class Line(left: String, sep: String, rights: Vector[String], index: Int) {
    lazy val whole = if (isSplit) left + sep + right else left
    lazy val right = if (isSplit) rights.mkString("\n") else ""
    lazy val indent = " "*left.length
    def isSplit = sep.nonEmpty
    def isWhite = !isSplit && Line.AllWhite.pattern.matcher(left).matches
    
    // Try to merge this with a compatible following line (same separator, following line has whitespace instead of left text)
    def merge(l: Line): Option[Line] =
      if (!(isSplit && l.isSplit && sep == l.sep && indent == Line.detab(l.left))) None
      else Some(new Line(left, sep, rights ++ l.rights, index))
    
    // Remove any text with a trailing double slash (whole lines only)
    def decomment = 
      if (isSplit) this
      else new Line({ val i = left indexOf "//"; if (i<0) left else left take i }, sep, rights, index)
      
    // Trim whitespace (only from right side if we're not split)
    def trimmed =
      if (isSplit) new Line(left.trim, sep, rights.map(_.trim), index)
      else new Line(left.reverse.dropWhile(_.isWhitespace).reverse, sep, rights, index)
      
    // Try to parse a single line into one with a (default) delimiter
    def parsed = if (isSplit) this else Line.apply(left, index)
  }
  object Line {
    def mkSepRegex(sep: String) = ("""^((?:\s|\w|!)+)(""" + java.util.regex.Pattern.quote(sep) + """)(.*)""").r
    val JustTab = """\t""".r
    val AllWhite = """^\s*$""".r
    val ArrowRegex = mkSepRegex("-->")
    val DotsRegex = mkSepRegex("...")
    
    val emptyRight = Vector("")
    val empty = new Line("", "", emptyRight, -1)
    
    // Convert tabs to eight spaces
    def detab(s: String) = JustTab.replaceAllIn("\t","        ")
    
    // Produce a bare line (just text, numbered)
    def bare(s: String, i: Int) = new Line(s, "", emptyRight, i)
    
    // Try to parse the line
    def apply(s: String, i: Int): Line = s match {
      case ArrowRegex(l, s, r) => new Line(l, s,  Vector(r),  i)
      case DotsRegex(l, s, r)  => new Line(l, s,  Vector(r),  i)
      case x                   => new Line(x, "", emptyRight, i)
    }
    
    // Parse the line with custom separators
    def custom(s: String, i: Int, firstSep: String, moreSeps: String*): Line = {
      for (sep <- (firstSep +: moreSeps)) {
        val CustomRegex = mkSepRegex(sep)
        s match {
          case CustomRegex(l, s, r) => return new Line(l, s, Vector(r), i)
          case _ =>
        }
      }
      Line.bare(s, i)
    }
    
    // Parse the line with default and custom separators
    def apply(s: String, i: Int, firstSep: String, moreSeps: String*): Line = {
      val x = custom(s, i, firstSep, moreSeps: _*)
      if (!x.isSplit) apply(s,i) else x
    }
  }
  
  // Encapsulates operations on blocks of lines (comments, merging, etc.)
  case class Lines(lines: Vector[Line]) {
    @inline def underlying = lines
    @inline def isEmpty = lines.isEmpty
    
    // Apply some function to get a new Lines
    def ap(f: Vector[Line] => Vector[Line]) = new Lines(f(lines))
    def map(f: Line => Line) = ap(_.map(f))
    
    // Chop lines up into groups whenever they are blanks empty (whitespace-only) lines in a row
    // Only works on unparsed lines; may not give what you expect if you have trimmed or decommented
    def tokenize(blanks: Int): Vector[Lines] = if (blanks < 0) Vector(this) else {
      lines.scanLeft( (0, 0, Line.empty) ){ (acc, line) =>
        val (blank, group, _) = acc
        if (line.isWhite) (blank + 1, if (blank+1 == blanks) group+1 else group, line)
        else (0, group, line)
      }.drop(1).
        groupBy(_._2).          // Group by group number
        toVector.sortBy(_._1).  // Keep original order
        map(_._2.map(_._3)).    // Only keep lines
        map(v => v.dropWhile(_.isWhite).reverse.dropWhile(_.isWhite).reverse).   // Trim white lines from both ends
        map(v => new Lines(v))  // Pack into Lines class
    }
    
    // Remove comments (unsplit lines only)
    def decomment = map(_.decomment)
    
    // Find separators for any lines that haven't already been parsed
    def parsed = map(_.parsed)
    
    // glue together lines with continuation pattern: ... or --> at same position but with whitespace before
    def compact = ap(ls => (Vector.empty[Line] /: ls){ (v,line) => 
      v.lastOption.flatMap(_ merge line) match {
        case Some(newline) => v.updated(v.length-1, newline)
        case None          => v :+ line
      }
    })
    
    // Throw away all empty lines after trimming whitespace
    def trim = map(_.trimmed).ap(_.filter(! _.isWhite))
  }
  object Lines {
    import scala.util.Try
    
    val empty = new Lines(Vector.empty[Line])
    
    def apply(f: java.io.File): Option[Lines] =
      Try{ io.Source.fromFile(f) }.
      map{ src => src -> Try { new Lines(src.getLines.toVector.zipWithIndex.map{ case (s,n) => Line.bare(s,n+1) }) } }.
      flatMap{ case (s,t) => Try{ s.close }; t }.toOption
      
    def apply(s: String): Option[Lines] = apply(new java.io.File(s))
    
    def parsed(f: java.io.File, blanks: Int): Option[Vector[Lines]] = apply(f).map(_.tokenize(blanks).map(_.decomment.parsed.compact.trim).filter(_.lines.length > 0))
    def parsed(s: String, blanks: Int): Option[Vector[Lines]] = parsed(new java.io.File(s), blanks)
    
    def parsed(f: java.io.File): Option[Lines] = apply(f).map(_.decomment.parsed.compact.trim)
    def parsed(s: String): Option[Lines] = parsed(new java.io.File(s))
  }

  case class Test(line: Line, params: Set[String], must: Set[String], mustnt: Set[String]) {
    lazy val methods = Test.BacktickRegex.findAllIn(line.right).map(Test.pickOutBackticked).toSet
    lazy val code = Test.BacktickRegex.replaceAllIn(line.right, Test.matchOutBackticked)
    def validateMethods(p: String => Boolean) = methods.forall(p)
    def validateFlags(p: String => Boolean) = must.forall(p) && !mustnt.exists(p)
  }
  object Test {
    import scala.util._
    
    val BacktickRegex = "`[^`]+`".r
    val pickOutBackticked = (s: String) => s.split("\\\\").mkString("\\").drop(1).dropRight(1)
    val matchOutBackticked = (m: scala.util.matching.Regex.Match) => if (m.matched == null) "" else pickOutBackticked(m.matched)
    
    def parse(line: Line): Either[String, Test] = {
      if (!line.isSplit) Right(new Test(line, Set("x"), Set(), Set()))
      else if (line.sep != "...") Left(s"Wrong separator on line ${line.index}: ${line.sep}")
      else {
        val (params, rest1) = line.left.split("\\s+").filter(_.length > 0).partition(_.forall(_.isLower))
        val (must, rest2) = rest1.partition(_.forall(_.isUpper))
        val (mustnt, uhoh) = rest2.partition(s => (s startsWith "!") && (s drop 1).forall(_.isUpper))
        if (uhoh.nonEmpty) Left(s"Syntax error in line ${line.index}: identifier neither parameter (lower-case) nor flag (upper): ${uhoh.mkString(" ")}")
        else Right(new Test(line, params.toSet | Set("x"), must.toSet, mustnt.map(_.drop(1)).toSet))
      }
    }
    
    def unapply(l: Line): Option[Test] = parse(l).right.toOption
  }
  
  case class Tests(params: Set[String], tests: Vector[Test]) {
    @inline def underlying = tests
    @inline def isEmpty = tests.isEmpty
    lazy val codes = tests.map(_.code)
    def validatedMethods(p: String => Boolean) = {
      val filt = tests.filter(_.validateMethods(p))
      if (filt.length == tests.length) this
      else new Tests(params, filt)
    }
    def validatedFlags(p: String => Boolean) = {
      val filt = tests.filter(_.validateFlags(p))
      if (filt.length == tests.length) this
      else new Tests(params, filt)
    }
  }
  object Tests {
    import scala.util._
    
    def parseFrom(lines: Lines): Either[Vector[String], Vector[Tests]] = {
      val tests = lines.underlying.map(Test.parse)
      val wrong = tests.collect{ case Left(msg) => msg }
      if (wrong.nonEmpty) Left(wrong)
      else Right(
        tests.collect{ case Right(test) => test }.
          groupBy(x => x.params.toList.sorted).                    // Group same parameters
          map{ case (k,v) => k.mkString(s" ${k.length} ") -> v }.  // Convert parameters to nice form for sorting
          toVector.sortBy(_._1).map(_._2).                         // Put them in order
          map(ts => new Tests(ts.head.params, ts))
      )
    }
    
    def apply(f: java.io.File): Either[Vector[String], Vector[Tests]] =
      Lines.parsed(f).map(parseFrom).getOrElse(Left(Vector(s"Couldn't read ${f.getPath}")))
      
    def apply(s: String): Either[Vector[String], Vector[Tests]] = apply(new java.io.File(s))
  }
  // Find which variables to create, what flags to apply, and what the (raw unsubstituted) code is
  /*
  case class TestLine(rawcode: String, params: Set[String], flags: Set[String], index: Int) extends Numbered {
    // Set of methods that are required to run this test
    lazy val methods = {
      TestLine.BacktickRegex.
        findAllIn(rawcode).
        map(_.split("\\\\").mkString("\\").drop(1).dropRight(1)). 
        toSet
    }
  }
  object TestLine {
    def unapply(l: Line): Option[TestLine] =
      if (l.sep != "...") None
      else
      case Flagged(flags, code) =>
        val (lower, upper) = flags.trim.split("\\s+").filter(_.nonEmpty).toSet.partition(_.last.isLower)
        if (!upper.forall(_.last.isUpper)) None
        else Some(new TestLine(code, Set("x") | lower, upper))
      case _ => Some(new TestLine(s, Set("x"), Set[String]()))
    }
  }
  */
  
  // Perform substitution of @-prefixed strings (caps only)
  val ReplReg = "@[A-Z]+".r
  def allSubstitutions(ss: Seq[String], mm: Map[String, Set[String]]): Seq[Seq[String]] = {
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
    
  
  
  // Paired delimiters that we recognize
  val paired = "()[]{}<>".grouped(2).map(x => x(0) -> x(1)).toMap
  
  // Finds paired delimiters (matches outermost type only, nesting okay)
  // Will skip whitespace.  Otherwise delimiter must be bare (no preceding text),
  // or no delimiter is assumed and it just breaks on whitespace.
  def pairedIndicesOf(s: String, i: Int = 0): Option[(Int,Int)] = {
    if (i >= s.length) None
    else paired.get(s(i)) match {
      case None => 
        if (s(i).isWhitespace) pairedIndicesOf(s, i+1)
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


  // Encapsulates a named substitution pattern
  // E.g. Substitution("$X", "<<", ">>") would be able to transform "$X(3)" into "<<3>>"
  case class Substitution(key: String, lhs: String, rhs: String) {
    // Perform this substitution on string s (and add prefix)
    def sub(s: String, prefix: String): String = if (s.isEmpty) prefix else {
      val i = s.indexOfSlice(key)
      if (i < 0) prefix + s
      else {
	pairedIndicesOf(s, i+key.length) match {
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
    // Make a substitution from a string where the variable part should be inserted in place of $
    def from(key: String, subst: String) = {
      val i = subst.indexOfSlice(" $ ")
      if (i < 0) throw new IllegalArgumentException("Could not find \" $ \" in " + subst)
      val j = subst.indexOfSlice(" $ ",i+3)
      if (j >= 0) throw new IllegalArgumentException("Found more than one \" $ \" in " + subst)
      new Substitution(key, subst.substring(0,i), subst.substring(i+3))
    }
  }  
}

