package nicoburniske.faxion.model

sealed trait Article

case object Top       extends Article
case object Bottom    extends Article
case object Outerwear extends Article
case object Footwear  extends Article
