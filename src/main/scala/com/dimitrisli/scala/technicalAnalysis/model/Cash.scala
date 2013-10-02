package com.dimitrisli.scala.technicalAnalysis.model

case class Cash (val amount:Double) extends Asset with Immutable {

  def getAmount = amount

  def +(newCash: Cash) = new Cash(this.amount + newCash.amount)
}