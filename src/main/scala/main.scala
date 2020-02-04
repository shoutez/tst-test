


object Hello extends App {


  val rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior"),
  )

  val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
  );

  println("Problem 1")
  println("\nRates:")
  println(rates)
  println("\nPrices:")
  println(prices)
  println("\nOutput:")
  Rates.getBestGroupPrices(rates, prices).foreach(group => println(group))

  val promotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2"))
  )

  println("\nProblem 2")
  println("\nAll Promotions:")
  println(promotions)

  println("\nAll promotions combinations:")
  println(Promotions.allCombinablePromotions(promotions))

  println("\nPromotions P1")
  println(Promotions.combinablePromotions("P1", promotions))
  println("\nPromotions P3")
  println(Promotions.combinablePromotions("P3", promotions))

}