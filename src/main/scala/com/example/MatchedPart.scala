package com.example

case class MatchedPart(coloredPart: ColoredPart, legoPart: Option[LegoPart], categoriesGuessed: Boolean = false) extends Ordered[MatchedPart] {
  def compare(that: MatchedPart): Int = {
    val legoCmp = (this.legoPart, that.legoPart) match {
      case (Some(lp1), Some(lp2)) => lp1.compare(lp2)
      case (Some(_), None) => -1
      case (None, Some(_)) => 1
      case (None, None) => 0
    }
    if (legoCmp != 0) legoCmp
    else {
      val colorCmp = this.coloredPart.color.compare(that.coloredPart.color)
      if (colorCmp != 0) colorCmp
      else this.coloredPart.quantity.compare(that.coloredPart.quantity)
    }
  }
}
