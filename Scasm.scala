package scasm

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
  def visit[A](f: java.io.File, newVisitor: => (asm.ClassVisitor with Resulting[A])): A = {
    val fis = new java.io.FileInputStream(f)
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
  def family(f: java.io.File) = visit(f, new FamilyVisitor)
  
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
  def methods(f: java.io.File) = visit(f, new ExtractMethods)
}
