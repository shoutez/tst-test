case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String,
                      rateCode: String,
                      price: BigDecimal)

case class BestGroupPrice(cabinCode: String,
                          rateCode: String,
                          price: BigDecimal,
                          rateGroup: String)


object Rates {
  /**
   *  Retrieves the best price for all rate groups
   * @param rates List of rates
   * @param prices List of prices
   * @return List of BestGroupPrice
   */
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]) = {
    val results = rates.iterator.map { rate => {
      val groupPrices = prices.filter(cabinPrice => cabinPrice.rateCode == rate.rateCode)
        .iterator.map(cabinPrice =>
        BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rate.rateGroup))
      groupPrices.toSeq
    }
    }
    results.toSeq.flatten
      .groupBy(group => s"${group.cabinCode}-${group.rateGroup}")
      .map(entry => entry._2.minBy(_.price))
      .toSeq.sortBy(_.price)
  }
}