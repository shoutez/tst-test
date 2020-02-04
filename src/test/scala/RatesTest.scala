import org.scalatest._

class MySpec extends FunSpec {
  describe("Rates") {
    it("should return the lowest group price when rates provided but not enough prices") {
      val rates = Seq(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
      );

      val prices = Seq(
        CabinPrice("CA", "M1", 200.00),
        CabinPrice("CA", "M2", 250.00)
      )

      val result = Rates.getBestGroupPrices(rates, prices)
      assert(result.length == 1)
      assert(result(0).cabinCode == "CA")
      assert(result(0).price == 200)
      assert(result(0).rateCode == "M1")
      assert(result(0).rateGroup == "Military")
    }

    it("should return the lowest group price with less rates more prices") {
      val rates = Seq(
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
      );

      val prices = Seq(
        CabinPrice("CA", "S1", 225.00),
        CabinPrice("CA", "S2", 260.00),
        CabinPrice("CB", "S1", 245.00),
        CabinPrice("CB", "S2", 270.00)
      )

      val result = Rates.getBestGroupPrices(rates, prices)
      assert(result.length == 2)
      assert(result(0).cabinCode == "CA")
      assert(result(0).price == 225)
      assert(result(0).rateCode == "S1")
      assert(result(0).rateGroup == "Senior")

      assert(result(1).cabinCode == "CB")
      assert(result(1).price == 245)
      assert(result(1).rateCode == "S1")
      assert(result(1).rateGroup == "Senior")
    }
  }
}