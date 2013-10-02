package com.dimitrisli.scala.technicalAnalysis.utils

object NumberUtils {

  def roundDouble(double: Double) = BigDecimal(double).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
