package com.dimitrisli.scala.technicalAnalysis

import http.{YahooPeriodBuilder, StockHistoricalDataRetriever}
import model.Cash
import strategies.MovingAverageStrategy

object Main {

  def main(args:Array[String]){

    val period = new YahooPeriodBuilder()
                  .withId("GOOG")
                  .withFromDay(1).withFromMonth(1).withFromYear(2013)
                  .withToDay(1).withToMonth(10).withToYear(2013)
                  .withInterval("d").build

    val url = period.getURL

    val stockTimeSeries = StockHistoricalDataRetriever.fetch(url)

    val myCash = new Cash(1000)

    print( MovingAverageStrategy(30).performStrategy(myCash, stockTimeSeries) )
  }
}
