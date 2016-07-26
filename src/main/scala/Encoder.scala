package com.nathanbreitsch.runlengthencoding

object Encoder{

  type Encoding = List[(Char, Int)]
  val emptyEncoding: Encoding = List[(Char,Int)]()

  def encode(text: String): String = if (text.isEmpty) "" else {

    def agg(acc: Encoding, next: Char): Encoding = {
      if(acc.isEmpty) List((next, 1))
      else if(next == acc.head._1) (acc.head._1, acc.head._2 + 1) :: acc.tail
      else (next, 1) :: acc
    }

    text
      .foldLeft(emptyEncoding)(agg)
      .reverse
      .flatMap(x => x._2.toString + x._1.toString)
      .mkString
  }

  def decode(text: String): String = {
    val regex = """\d+[a-zA-Z]""".r
    regex.findAllIn(text).map {
      x: String => {
        val c: Char = x.last
        val countString: String = x.reverse.tail.reverse.toString
        val count: Int = countString.toInt
        c.toString * count
      }
    }
  } mkString("")
}
