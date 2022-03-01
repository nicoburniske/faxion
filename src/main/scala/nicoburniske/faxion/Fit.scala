package nicoburniske.faxion

trait Fit[+A] {
  // Design a distinct ordered list.
  def components(): Seq[Article]
}
