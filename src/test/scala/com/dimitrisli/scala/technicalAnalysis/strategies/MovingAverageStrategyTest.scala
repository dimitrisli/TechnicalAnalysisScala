package com.dimitrisli.scala.technicalAnalysis.strategies

import org.scalatest.FlatSpec
import org.scalatest.matchers.ClassicMatchers
import com.dimitrisli.scala.technicalAnalysis.http.{StockHistoricalDataRetriever, YahooPeriodBuilder}
import com.dimitrisli.scala.technicalAnalysis.model.Cash


class MovingAverageStrategyTest extends FlatSpec with ClassicMatchers {

  val period = new YahooPeriodBuilder().withId("GOOG")
    .withFromDay(1).withFromMonth(1).withFromYear(2013)
    .withToDay(1).withToMonth(10).withToYear(2013).withInterval("d").build

  val url = period.getURL

  val stockInPeriodRange = StockHistoricalDataRetriever.fetch(url)

  "A Moving Average Strategy of 30 days" should "result in no money spent if not enough to buy stocks" in {
    assert(math.abs( MovingAverageStrategy(30).performStrategy(new Cash(100), stockInPeriodRange)-100.0)<0.0001)
  }

  it should "result in 1032.69 cash when investing 1000 for GOOG over a course of 9 months starting YTD on 2013" in {
    assert( MovingAverageStrategy(30).performStrategy(new Cash(1000), stockInPeriodRange) === 1032.69)
  }

  it should "result in 10409.08 cash when investing 10000 for GOOG over a course of 9 months starting YTD on 2013" in {
    assert(MovingAverageStrategy(30).performStrategy(new Cash(10000), stockInPeriodRange) === 10409.08)
  }
}