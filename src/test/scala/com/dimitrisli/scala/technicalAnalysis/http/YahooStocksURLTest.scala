package com.dimitrisli.scala.technicalAnalysis.http

import org.scalatest.FlatSpec
import org.scalatest.matchers.ClassicMatchers

class YahooStocksURLTest extends FlatSpec with ClassicMatchers {
  "A Yahoo Stock URL parameter" should "include the ID" in {
    assert (ID.param === "s")
  }

  "A Yahoo Stock URL Builder" should "build a URL string using builder pattern" in {
    val period = new YahooPeriodBuilder().withId("GOOG")
      .withFromDay(1).withFromMonth(1).withFromYear(2013)
      .withToDay(1).withToMonth(10).withToYear(2013).withInterval("d").build

    val url = period.getURL

    assert(url === "http://ichart.yahoo.com/table.csv?a=0&b=1&c=2013&d=9&e=1&f=2013&g=d&ignore=.csv&s=GOOG")
  }

  it should "be able to provide partial input still constructing the URL" in {
    val period = new YahooPeriodBuilder().withId("GOOG")
      .withFromDay(1).withFromMonth(1).withFromYear(2013)
      .withToDay(1).withToYear(2013).withInterval("d").build

    val url = period.getURL

    assert(url === "http://ichart.yahoo.com/table.csv?a=0&b=1&c=2013&d=&e=1&f=2013&g=d&ignore=.csv&s=GOOG")
  }


}