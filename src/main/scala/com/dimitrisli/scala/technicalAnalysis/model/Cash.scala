package com.dimitrisli.scala.technicalAnalysis.model

import com.dimitrisli.scala.technicalAnalysis.utils.NumberUtils

case class Cash (val amount:Double) extends Asset with Immutable {

  def getAmount = amount

  def +(newCash: Cash) = new Cash(NumberUtils.roundDouble(this.amount + newCash.amount))
}
