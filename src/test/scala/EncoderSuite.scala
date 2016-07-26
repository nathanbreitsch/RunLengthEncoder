package com.nathanbreitsch.runlengthencoding

import org.scalatest._

class EncoderTest extends FlatSpec with Matchers {

  it should "encode empty string as empty string" in {
    Encoder.encode("") should equal("")
  }

  it should "encode homogenous string correctly" in {
    Encoder.encode("AAA") should equal("3A")
  }

  it should "encode more complicated string" in {
    Encoder.encode("HORSE") should equal("1H1O1R1S1E")
  }

  it should "encode even more complex string" in {
    Encoder.encode("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB") should equal("12W1B12W3B24W1B")
  }

  it should "it decodes an encoded simple string" in {
    Encoder.decode("3A") should equal("AAA")
  }

  it should "it decodes a more complicated string" in {
    Encoder.decode("12W1B12W3B24W1B") should equal("WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB")
  }

}
