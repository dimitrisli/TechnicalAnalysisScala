package com.dimitrisli.scala.technicalAnalysis.http

import scala.collection.immutable.TreeMap
import io.Source
import scala.collection.mutable.ListBuffer
import org.joda.time.LocalDate
import com.dimitrisli.scala.technicalAnalysis.model.StockDayTrade

case class YahooURLParams(param:String)
  object ID extends YahooURLParams("s")
  object FROM_DAY extends YahooURLParams("b")
  object FROM_MONTH extends YahooURLParams("a")
  object FROM_YEAR extends YahooURLParams("c")
  object TO_DAY extends YahooURLParams("e")
  object TO_MONTH extends YahooURLParams("d")
  object TO_YEAR extends YahooURLParams("f")
  object INTERVAL extends YahooURLParams("g")
  object IGNORE extends YahooURLParams("ignore")

class YahooPeriodBuilder {

  var id:Option[String] = None
  var fromDay:Option[Int] = None
  var fromMonth:Option[Int] = None
  var fromYear:Option[Int] = None
  var toDay:Option[Int] = None
  var toMonth:Option[Int] = None
  var toYear:Option[Int] = None
  var interval:Option[String] = None

  def withId(id: String)= {this.id = Some(id); this}
  def withFromDay(fromDay: Int) = {this.fromDay = Some(fromDay); this}
  def withFromMonth(fromMonth: Int) = {this.fromMonth = Some(fromMonth-1); this}
  def withFromYear(fromYear: Int) = {this.fromYear = Some(fromYear); this}
  def withToDay(toDay: Int) = {this.toDay = Some(toDay); this}
  def withToMonth(toMonth: Int) = {this.toMonth = Some(toMonth-1); this}
  def withToYear(toYear: Int) = {this.toYear = Some(toYear); this}
  def withInterval(interval: String) = {this.interval = Some(interval); this}

  def build = new YahooStocksURL(this)
}

class YahooStocksURL(yahooPeriodBuilder: YahooPeriodBuilder){

  val id:Option[String] = yahooPeriodBuilder.id
  val fromDay:Option[Int] = yahooPeriodBuilder.fromDay
  val fromMonth:Option[Int] = yahooPeriodBuilder.fromMonth
  val fromYear:Option[Int] = yahooPeriodBuilder.fromYear
  val toDay:Option[Int] = yahooPeriodBuilder.toDay
  val toMonth:Option[Int] = yahooPeriodBuilder.toMonth
  val toYear:Option[Int] = yahooPeriodBuilder.toYear
  val interval:Option[String] = yahooPeriodBuilder.interval

  def getURL = "http://ichart.yahoo.com/table.csv?" +
                  TreeMap(ID.param -> id.getOrElse(""),
                    FROM_MONTH.param -> fromMonth.getOrElse(""),
                    FROM_DAY.param -> fromDay.getOrElse(""),
                    FROM_YEAR.param -> fromYear.getOrElse(""),
                    TO_MONTH.param -> toMonth.getOrElse(""),
                    TO_DAY.param -> toDay.getOrElse(""),
                    TO_YEAR.param -> toYear.getOrElse(""),
                    INTERVAL.param -> interval.getOrElse(""),
                    IGNORE.param -> ".csv")
                  .map(pair=>pair._1+"="+pair._2).mkString("&")
}



object StockHistoricalDataRetriever {

  def printStdoutHistoricalData(url:String) = Source.fromURL(url).getLines().mkString("\n")

  def fetch(url:String):List[StockDayTrade] = {
    val stocks = ListBuffer[StockDayTrade]()
    for (dailyNAV <- Source.fromURL(url).getLines().drop(1)) {
      dailyNAV.split(",") match {
        case Array(date,_,_,_,close,volume,_) =>  {
          date.split("-") match {
            case Array(year,month,day) =>
              stocks += new StockDayTrade(url.split("s=")(1), new LocalDate(year.toInt,month.toInt,day.toInt), close.toDouble,volume.toLong)
          }
        }
      }
    }
    stocks.sortWith((s1:StockDayTrade, s2:StockDayTrade) => s1.date.compareTo(s2.date) == -1).toList
  }

}