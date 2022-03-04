package nicoburniske.faxion.model

trait Fit[+A] {
  // Design a distinct ordered list.
  def components(): Seq[Article]
}
