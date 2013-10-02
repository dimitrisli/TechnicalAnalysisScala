package com.dimitrisli.scala.technicalAnalysis.strategies

import com.dimitrisli.scala.technicalAnalysis.model.{UpwardTrend, DownwardTrend}
import com.google.common.collect.EvictingQueue
import com.dimitrisli.scala.technicalAnalysis.model.StockDayTrade
import com.dimitrisli.scala.technicalAnalysis.model.Cash
import com.dimitrisli.scala.technicalAnalysis.model.Portfolio
import com.dimitrisli.scala.technicalAnalysis.utils.MAUtils

class MovingAverageStrategy(val days:Int) extends Strategy {

  def performStrategy(cash: Cash, stockInPeriodRange:List[StockDayTrade]):Double = {

    val movingAverageQueue = EvictingQueue.create[StockDayTrade](days)

    def performStrategyAcc(portfolio:Portfolio, cash:Cash, stockInRemainingPeriodRange:List[StockDayTrade]):Double =
      stockInRemainingPeriodRange match {

        //finished processing timeseries
        case _::Nil => {
          println("done. "+portfolio+", "+cash)
          portfolio.getAmount + cash.getAmount
        }

        //need more data to compile the MA
        case stockDayTrade::tail if(movingAverageQueue.size()!=days) => {
          movingAverageQueue.add(stockDayTrade)
          performStrategyAcc(portfolio, cash, tail)
        }

        //above MA
        case stockDayTrade::tail
          if(MAUtils.avg(movingAverageQueue) < stockDayTrade.close) => {
            portfolio match {
              //upward trend. keep it up
              case portfolioUpwardTrend: UpwardTrend =>  {
                println("upward trend: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue) + ", "+portfolio +", "+cash)
                movingAverageQueue.add(stockDayTrade)
                performStrategyAcc(portfolioUpwardTrend.newValueUpward(stockDayTrade.close), cash, tail)
              }
              //time to buy
              case portfolioDownwardTrend: DownwardTrend => {
                println("time to buy: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue) + ", "+portfolio +", "+cash)
                movingAverageQueue.add(stockDayTrade)
                portfolioDownwardTrend.newValueUpward(stockDayTrade.close).buyStocks(cash) match {
                  case (newPortfolio, remainingCash) => {
                    performStrategyAcc(newPortfolio, remainingCash, tail)
                  }
                }
              }
              //starting up. Let's buy some stocks
              case _ => {
                println("starting up: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue) + ", "+portfolio +", "+cash)
                movingAverageQueue.add(stockDayTrade)
                portfolio.newValueUpward(stockDayTrade.close).buyStocks(cash) match {
                  case (newPortfolio, remainingCash) => {
                    performStrategyAcc(newPortfolio, remainingCash, tail)
                  }
                }
              }
            }

        }

        //below MA
        case stockDayTrade::tail
          if(MAUtils.avg(movingAverageQueue) > stockDayTrade.close) => {

          portfolio match {
            //time to sell
            case portfolioUpwardTrend: UpwardTrend =>  {
              println("time to sell: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue)+ ", "+portfolio +", "+cash)
              movingAverageQueue.add(stockDayTrade)
              val newPortfolio = portfolioUpwardTrend.sellOff.newValueDownward(stockDayTrade.close)
              performStrategyAcc(newPortfolio, cash + new Cash(portfolioUpwardTrend.getAmount), tail)
            }
            //downward trend. Let it be
            case portfolioDownwardTrend: DownwardTrend => {
              println("downward trend: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue)+ ", "+portfolio +", "+cash)
              movingAverageQueue.add(stockDayTrade)
              performStrategyAcc(portfolioDownwardTrend.newValueDownward(stockDayTrade.close), cash, tail)
            }
            //starting up. Let's buy some stocks
            case _ => {
              println("starting up: "+"s:"+stockDayTrade.close+", MA:" + MAUtils.avg(movingAverageQueue) + ", "+portfolio +", "+cash)
              movingAverageQueue.add(stockDayTrade)
              portfolio.newValueUpward(stockDayTrade.close).buyStocks(cash) match {
                case (newPortfolio, remainingCash) => {
                  performStrategyAcc(newPortfolio, remainingCash, tail)
                }
              }
            }
          }
        }
      }

    //get in the nested method accum recursion
    performStrategyAcc(new Portfolio(stockInPeriodRange.head.stockSymbol, 0, stockInPeriodRange.head.close), cash,
      stockInPeriodRange)
  }
}

object MovingAverageStrategy {

  def apply(days:Int) = new MovingAverageStrategy(days)
}