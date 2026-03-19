package com.wolfskeep

case class Category(number: String, name: String, parent: Option[Category]) {
  lazy val hierarchy: List[Category] = parent.map(_.hierarchy).getOrElse(Nil) :+ this
}