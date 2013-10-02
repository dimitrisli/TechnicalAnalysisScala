package com.dimitrisli.scala.technicalAnalysis.model

import org.joda.time.LocalDate

case class StockDayTrade(val stockSymbol:String, val date:LocalDate, val close:Double, val volume:Long)
