package scasm

import java.io._
import java.util.zip._
import scala.util._
import org.objectweb.asm
import asm.{signature => sig}

object Scasm {
  trait Resulting[A] {
    def clear(): Unit
    def result(): A
  }
  
  def visit[A](name: String, newVisitor: => (asm.ClassVisitor with Resulting[A])): A = {
    val cr = new asm.ClassReader(name)
    val visitor = newVisitor
    cr.accept(visitor, 0)
    visitor.result
  }
  def visit[A](bytes: Array[Byte], newVisitor: => (asm.ClassVisitor with Resulting[A])): A = {
    val cr = new asm.ClassReader(bytes)
    val visitor = newVisitor
    cr.accept(visitor, 0)
    visitor.result
  }
  def visit[A](f: File, newVisitor: => (asm.ClassVisitor with Resulting[A])): A = {
    val fis = new FileInputStream(f)
    try {
      val cr = new asm.ClassReader(fis)
      val visitor = newVisitor
      cr.accept(visitor, 0)
      visitor.result
    } finally { fis.close }
  }
  
  case class Family(me: String, parent: Option[String], faces: Array[String])
  
  class FamilyVisitor extends asm.ClassVisitor(asm.Opcodes.ASM5) with Resulting[Family] {
    private[this] var found: Option[Family] = None
    override def visit(version: Int, access: Int, name: String, sigs: String, sup: String, face: Array[String]) {
      found = Some(Family(name, Option(sup).filter(_.length > 0), face match { case null => Array[String](); case x => x }))
    }
    def clear() { found = None }
    def result() = {
      val ans = found.get
      found = None
      ans
    }
  }
  
  def family(name: String) = visit(name, new FamilyVisitor)
  def family(bytes: Array[Byte]) = visit(bytes, new FamilyVisitor)
  def family(f: File) = visit(f, new FamilyVisitor)
  
  class AnnotationPeeker(p: String => Boolean) extends asm.ClassVisitor(asm.Opcodes.ASM5) with Resulting[Option[String]] {
    private[this] var myName: Option[String] = None
    private[this] var hasNamedAnnotation: Boolean = false
    override def visit(version: Int, access: Int, name: String, sigs: String, sup: String, face: Array[String]) {
      myName = Some(name)
    }
    override def visitAnnotation(desc: String, visible: Boolean) = {
      if (p(desc)) hasNamedAnnotation = true
      null
    }
    def clear() { myName = None; hasNamedAnnotation = false }
    def result() = {
      val ans = myName.filter(_ => hasNamedAnnotation)
      clear()
      ans
    }
  }
  
  class FieldPeeker(p: String => Boolean, scalaOnly: Boolean = false) extends asm.ClassVisitor(asm.Opcodes.ASM5) with Resulting[Option[(String,String)]] {
    private[this] var myName: Option[String] = None
    private[this] var staticValue: Option[Object] = None
    private[this] var hasNamedField: Boolean = false
    private[this] var isScala: Boolean = !scalaOnly
    override def visit(version: Int, access: Int, name: String, sigs: String, sup: String, face: Array[String]) {
      myName = Some(name)
    }
    override def visitField(access: Int, name: String, desc: String, sig: String, value: Object) = {
      if (p(name)) {
        hasNamedField = true
        staticValue = Option(value)
      }
      null
    }
    override def visitAttribute(att: asm.Attribute) {
      att.`type` match { case "Scala" | "ScalaSig" => isScala = true; case _ => }
    }
    def clear() { myName = None; staticValue = None; hasNamedField = false }
    def result(): Option[(String, String)] = {
      val ans = for (n <- myName if hasNamedField && isScala; v <- staticValue) yield (n, v.toString)
      clear()
      ans
    }
  }
  
  case class Spawn(name: String, ret: String, from: String, params: Array[String])
  
  class ExtractMyType(throwaway: sig.SignatureVisitor) extends sig.SignatureVisitor(asm.Opcodes.ASM5) {
    private[this] var myType: Option[String] = Some("")
    override def visitArrayType() = { myType = myType.map(_ + "[ "); this }
    override def visitBaseType(c: Char) { myType = myType.map(_ + c + " ") }
    override def visitClassBound() = { myType = myType.map(_ + "<: "); this }
    override def visitClassType(s: String) { myType = myType.map(_ + "{" + s + " ") }
    override def visitEnd() { myType = myType.map(_ + "} ") }
    override def visitExceptionType() = throwaway
    override def visitFormalTypeParameter(s: String) { myType = myType.map(_ + "type="+ s + " ") }
    override def visitInnerClassType(s: String) {}
    override def visitInterface() = throwaway
    override def visitInterfaceBound() = throwaway
    override def visitParameterType() = { myType = myType.map(_ + "P "); this }
    override def visitReturnType() = { myType = myType.map(_ + "R "); this }
    override def visitSuperclass() = throwaway
    override def visitTypeArgument() { myType = myType.map(_ + "A ") }
    override def visitTypeArgument(c: Char) = { myType = myType.map(_ + "T"+c+" "); this }
    override def visitTypeVariable(s: String) { myType = myType.map(_ + "tn="+s+" ") }
    def result = myType
  }
  class ExtractParams extends sig.SignatureVisitor(asm.Opcodes.ASM5) {
    private[this] val throwaway = new sig.SignatureVisitor(asm.Opcodes.ASM5) {}
    private[this] val ret = new ExtractMyType(throwaway)
    private[this] var params: List[ExtractMyType] = Nil
    override def visitArrayType() = throwaway
    override def visitClassBound() = throwaway
    override def visitExceptionType() = throwaway
    override def visitInterface() = throwaway
    override def visitInterfaceBound() = throwaway
    override def visitParameterType() = { val p = new ExtractMyType(throwaway); params = p :: params; p }
    override def visitReturnType() = ret
    override def visitSuperclass() = throwaway
    override def visitTypeArgument(wc: Char) = throwaway
    def result = Spawn(null, ret.result.getOrElse(""), null, params.reverse.map(_.result).flatten.toArray)
  }
  object ExtractParams {
    def apply(s: String) = {
      val sr = new sig.SignatureReader(s)
      val sv = new ExtractParams
      sr.accept(sv)
      sv.result
    }
    def simple(s: String) = {
      val t = asm.Type.getType(s)
      Spawn(null, t.getReturnType.getDescriptor, null, t.getArgumentTypes.map(_.getDescriptor))
    }
  }
  
  class ExtractMethods extends asm.ClassVisitor(asm.Opcodes.ASM5) with Resulting[Array[Spawn]] {
    private[this] var myName: Option[String] = None
    private[this] var myMeth: List[Spawn] = Nil
    override def visit(version: Int, access: Int, name: String, sigs: String, sup: String, face: Array[String]) {
      myName = Some(name)
    }
    override def visitMethod(access: Int, name: String, desc: String, sigs: String, ex: Array[String]) = {
      val mv = super.visitMethod(access, name, desc, sigs, ex)
      if (sigs == null) myMeth = ExtractParams.simple(desc).copy(name = name) :: myMeth
      else myMeth = ExtractParams(sigs).copy(name = name) :: myMeth
      mv
    }
    def clear() { myName = None; myMeth = Nil }
    def result() = {
      val ans = myMeth.toArray.map(_.copy(from = myName.get)).sortBy(_.name)
      clear
      ans
    }
  }
  
  def methods(name: String) = visit(name, new ExtractMethods)
  def methods(bytes: Array[Byte]) = visit(bytes, new ExtractMethods)
  def methods(f: File) = visit(f, new ExtractMethods)
  
  def findField(name: String, bytes: Array[Byte], pr: String => Unit, scalaOnly: Boolean = false) {
    val fp = new FieldPeeker(_ == name, scalaOnly)
    visit(bytes, fp) match {
      case Some((who, what)) => pr(who + (if (what.length > 0) " = " + what + "." else ""))
      case _ =>
    }
  }
  
  def classBytes(as: Array[String]) = new Iterator[Array[Byte]] {
    private var i = 0
    private var myData: Array[Byte] = null
    private var resource: Either[(Array[File], Int), (ZipFile,java.util.Enumeration[ZipEntry])] = null

    private def check[A](t: Try[A]): Try[A] = {
      if (t.isFailure) i += 1
      t
    }
    private def rdFile(f: File) = Try {
      val fis = new FileInputStream(f)
      try {
        val bb = java.nio.ByteBuffer.allocate( (f.length min (Int.MaxValue-1)).toInt )
        fis.getChannel.read(bb)
        val data = new Array[Byte](bb.remaining)
        val n = bb.get(data)
        myData = data  // Make sure we actually read it all before we assign
      }
      finally { fis.close }
    }

    @annotation.tailrec def hasNext: Boolean = {
      if (myData != null) true
      else if (i >= as.length) false
      else resource match {
        case null =>
          val f = new File(as(i))
          if (!f.exists) { i += 1; hasNext }
          else if (as(i).endsWith(".class")) {
            rdFile(f)
            i += 1
            hasNext
          }
          else if (as(i).endsWith(".jar")) {
            check(Try {
              val zf = new ZipFile(f)
              try {
                val e = zf.entries().asInstanceOf[java.util.Enumeration[ZipEntry]]
                resource = Right((zf,e))
              }
              finally { if (resource == null) zf.close }
            })
            hasNext
          }
          else if (f.isDirectory) {
            check(Try {
              val fs = f.listFiles.filter(_.getName.endsWith(".class"))
              Left((fs,0))
            })
            hasNext
          }
          else { i += 1; hasNext }
        case Left((af,j)) =>
          if (j >= af.length) { resource = null; i += 1; hasNext }
          else {
            resource = Left((af,j+1))
            rdFile(af(j))
            hasNext
          }
        case Right((zf,e)) =>
          var ze: ZipEntry = null
          def good = (ze != null && ze.getName.endsWith(".class"))
          while (e.hasMoreElements && !good) ze = e.nextElement
          if (!good) { zf.close; resource = null; i += 1; hasNext }
          else {
            Try {
              val zis = zf.getInputStream(ze)
              try {
                val data = new Array[Byte]((ze.getSize min (Int.MaxValue-1)).toInt)
                var n, m = 0
                while (n < data.length && m != -1) {
                  m = zis.read(data, n, data.length-n)
                  if (m >= 0) n += m
                }
                if (n == data.length) myData = data
              }
              finally { zis.close }
            }
            hasNext
          }
      }
    }
    
    def next = if (!hasNext) throw new NoSuchElementException("Next on empty iterator") else {
      val data = myData
      myData = null
      data
    }
  }

  def main(argsAndOps: Array[String]) {
    val ops = argsAndOps.takeWhile(_ != "--").filter(_.startsWith("--")).map{s =>
      val i = s.indexOf('=');
      if (i < 0) s.drop(2) -> ""
      else s.substring(2,i) -> s.substring(i+1)
    }.toMap
    val args = argsAndOps.takeWhile(_ != "--").filter(! _.startsWith("--")) ++ argsAndOps.dropWhile(_ != "--").drop(1)
    
    def isNo(s: String) = s.toLowerCase match { case "n" | "no" | "false" => true; case _ => false }
    
    val output = ops.get("output")
    val allow: String => Boolean = ops.get("noAnonFunctions").filterNot(isNo) match {
      case None => _ => true
      case _ => s => !((s contains "$$anonfun") && s.endsWith(" = 0."))
    }
    
    def writeWith[A](f: (String => Unit) => A): Try[A] = output match {
      case None => Success(f((s: String) => if (allow(s)) println(s)))
      case Some(s) => Try {
        val fos = new FileOutputStream(s)
        try {
          val pw = new PrintWriter(fos)
          try { f((s: String) => if (allow(s)) pw.println(s)) } finally { pw.close }
        }
        finally (fos.close)
      }
    }
    
    val scalaOnly = ops.get("scalaOnly").map(_.toLowerCase match { case "n" | "no" | "false" => false; case _ => true }).getOrElse(false)
    
    ops.get("hasField").foreach{ field =>
      if (field.length > 0) writeWith(wr => classBytes(args).foreach(bs => findField(field, bs, wr, scalaOnly)))
    }
  }
}


