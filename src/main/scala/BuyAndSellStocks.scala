import scala.annotation.tailrec

class BuyAndSellStocks {
  def maxProfit(prices: Array[Int]): Int = {
    @tailrec
    def findMaxProfit(minPrice: Int, remainingPrices: List[Int], maxProfit: Int): Int = {
      remainingPrices match {
        case Nil => maxProfit
        case currPrice :: tail =>
          val updatedMinPrice = Math.min(currPrice, minPrice)
          val updatedMaxProfit = Math.max(currPrice - minPrice, maxProfit)
          findMaxProfit(updatedMinPrice, tail, updatedMaxProfit)
      }
    }

    if (prices.length < 2) 0
    else findMaxProfit(prices.head, prices.tail.toList, 0)
  }

}
