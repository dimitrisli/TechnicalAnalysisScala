package com.dimitrisli.scala.technicalAnalysis.strategies

import com.dimitrisli.scala.technicalAnalysis.model.{Cash, Portfolio, StockDayTrade}
import com.google.common.collect.EvictingQueue
import scala.collection.JavaConverters._

class MovingAverageStrategy(val days:Int) extends Strategy {

  def performStrategy(cash: Cash, stockInPeriodRange:List[StockDayTrade]):Double = {
    val movingAverage = EvictingQueue.create[StockDayTrade](days)

    def performStrategyAcc(portfolio:Portfolio, cash:Cash, stockInRemainingPeriodRange:List[StockDayTrade]):Double =
      stockInRemainingPeriodRange match {

      //finished processing timeseries
      case _::Nil => {
        println("done. Portfolio: "+portfolio+", Cash: "+cash)
        portfolio.getAmount + cash.getAmount
      }

      //need more data to compile the MA
      case stockDayTrade::tail if(movingAverage.size()!=days) => {
        movingAverage.add(stockDayTrade)
        performStrategyAcc(portfolio, cash, tail)
      }
      //upward trend. keep it up
      case stockDayTrade1::stockDayTrade2::tail
        if(movingAverage.asScala.toList.map(_.close).sum/days < stockDayTrade1.close
        && movingAverage.asScala.toList.map(_.close).sum/days < stockDayTrade2.close)=>{

            println("upward trend: "+"s1:"+stockDayTrade1.close+", s2:"+ stockDayTrade2.close +", MA:" + movingAverage.asScala.toList.map(_.close).sum/days + ", Portfolio:"+portfolio +", Cash: "+cash)
            movingAverage.add(stockDayTrade1)
            performStrategyAcc(portfolio.newNav(stockDayTrade1.close), cash, stockDayTrade2::tail)
        }

      //downward trend. let it be
      case stockDayTrade1::stockDayTrade2::tail
        if(movingAverage.asScala.toList.map(_.close).sum/days > stockDayTrade1.close
        && movingAverage.asScala.toList.map(_.close).sum/days > stockDayTrade2.close)=>{

        println("downward trend: "+"s1:"+stockDayTrade1.close+", s2:"+ stockDayTrade2.close +", MA:" + movingAverage.asScala.toList.map(_.close).sum/days+ ", Portfolio:"+portfolio +", Cash: "+cash)
            movingAverage.add(stockDayTrade1)
            performStrategyAcc(portfolio.newNav(stockDayTrade1.close), cash, stockDayTrade2::tail)
      }

      //Timeseries crossed MA downwards. Time to sell
      case stockDayTrade1::stockDayTrade2::tail
        if(movingAverage.asScala.toList.map(_.close).sum/days < stockDayTrade1.close
          && movingAverage.asScala.toList.map(_.close).sum/days > stockDayTrade2.close)=>{

        println("time to sell: "+"s1:"+stockDayTrade1.close+", s2:"+ stockDayTrade2.close +", MA:" + movingAverage.asScala.toList.map(_.close).sum/days+ ", Portfolio:"+portfolio +", Cash: "+cash)
            movingAverage.add(stockDayTrade1)
            performStrategyAcc(portfolio.sellOff.newNav(stockDayTrade1.close),
                                cash + new Cash(portfolio.getAmount),stockDayTrade2::tail)
      }

      //Timeseries crossed MA upwards. Time to buy
      case stockDayTrade1::stockDayTrade2::tail
        if(movingAverage.asScala.toList.map(_.close).sum/days > stockDayTrade1.close
          && movingAverage.asScala.toList.map(_.close).sum/days < stockDayTrade2.close)=>{

        println("time to buy: "+"s1:"+stockDayTrade1.close+", s2:"+ stockDayTrade2.close +", MA:" + movingAverage.asScala.toList.map(_.close).sum/days)
            movingAverage.add(stockDayTrade1)
            portfolio.newNav(stockDayTrade1.close).buyStocks(cash) match {
              case (newPortfolio, remainingCash) => {
                performStrategyAcc(newPortfolio, cash + remainingCash, stockDayTrade2::tail)
              }
            }
        }

    }

    //get into the nested method accum recursion
    performStrategyAcc(new Portfolio(stockInPeriodRange.head.stockSymbol, 0, stockInPeriodRange.head.close), cash,
                       stockInPeriodRange)
  }
}
