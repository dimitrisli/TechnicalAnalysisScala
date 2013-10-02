package com.dimitrisli.scala.technicalAnalysis.utils

import com.google.common.collect.EvictingQueue
import com.dimitrisli.scala.technicalAnalysis.model.StockDayTrade
import scala.collection.JavaConverters._

object MAUtils {

  def avg(movingAverage: EvictingQueue[StockDayTrade]) =
    NumberUtils.roundDouble(movingAverage.asScala.toList.map(_.close).sum/movingAverage.size())
}
