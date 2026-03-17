package com.example

case class LegoPart(
  partNumber: String,
  name: String,
  categories: List[Category],
  sequenceNumber: Int = 0,
  altNumbers: Set[String] = Set.empty,
  imageUrl: Option[String] = None,
  imageWidth: Option[String] = None,
  imageHeight: Option[String] = None
) extends Ordered[LegoPart] {
  def compare(that: LegoPart): Int = {
    val aCats = this.categories.map(_.number.toInt)
    val bCats = that.categories.map(_.number.toInt)
    val minLen = math.min(aCats.length, bCats.length)
    val cmp = (0 until minLen).view.map(i => aCats(i).compare(bCats(i))).find(_ != 0).getOrElse(aCats.length.compare(bCats.length))
    if (cmp != 0) cmp
    else {
      val seqCmp = this.sequenceNumber.compare(that.sequenceNumber)
      if (seqCmp != 0) seqCmp
      else this.partNumber.compare(that.partNumber)
    }
  }
}