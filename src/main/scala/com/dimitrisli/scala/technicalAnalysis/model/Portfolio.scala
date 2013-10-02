package com.dimitrisli.scala.technicalAnalysis.model

case class Portfolio (val stock:String, val volume:Int, nav:Double) extends Asset with Immutable {

  def getAmount = volume * nav

  def newNav(newNav: Double) = new Portfolio(stock, volume, newNav)

  def sellOff = new Portfolio(stock, 0, nav)

  def buyStocks(cash:Cash) = (new Portfolio(stock, (cash.getAmount / nav).toInt, nav), new Cash(cash.getAmount % nav))
}
