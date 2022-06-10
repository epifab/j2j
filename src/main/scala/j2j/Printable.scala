package j2j

trait Printable {

  def prettyPrint(indent: Int): String

  override def toString: String = prettyPrint(0)

  protected def newLine(indent: Int): String = "\n" + ("  " * indent)

}
