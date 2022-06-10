package j2j.props

trait Matcher[A, B] extends ((A, B) => Boolean) {
  def apply(a: A, b: B): Boolean
  def name: String
}
