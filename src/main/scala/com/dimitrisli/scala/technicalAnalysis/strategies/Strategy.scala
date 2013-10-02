package com.dimitrisli.scala.technicalAnalysis.strategies

import com.dimitrisli.scala.technicalAnalysis.model.{StockDayTrade, Cash, Asset, Portfolio}

trait Strategy {

  def performStrategy(cash:Cash, stockNAVs:List[StockDayTrade]):Double
}
