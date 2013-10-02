package com.dimitrisli.scala.technicalAnalysis.model

import com.dimitrisli.scala.technicalAnalysis.utils.NumberUtils

case class Portfolio (val stockSymbol:String, val volume:Int, value:Double) extends Asset with Immutable {

  def getAmount = volume * value

  def newValueUpward(newValue:Double) = new Portfolio(stockSymbol, volume, newValue) with UpwardTrend
  def newValueDownward(newValue:Double) = new Portfolio(stockSymbol, volume, newValue) with DownwardTrend
  def newValue(newValue:Double) = new Portfolio(stockSymbol, volume, newValue)

  def sellOff = new Portfolio(stockSymbol, 0, value)

  def buyStocks(cash:Cash) =  {
    val noStocks = (cash.getAmount / value).toInt
    val cashRemainder = new Cash(if(noStocks == 0) 0 else NumberUtils.roundDouble(cash.getAmount % value))
    ( new Portfolio(stockSymbol, noStocks, value) with UpwardTrend,  cashRemainder )
  }
}
