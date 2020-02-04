import org.scalatest._

class PromotionsTest extends FunSpec {
  describe("Promotions") {
    it("All Combinable Promotions") {
      val promotions = Seq(
        Promotion("1", Seq("2")),
        Promotion("2", Seq("1")),
        Promotion("3", Seq())
      )

      val result = Promotions.allCombinablePromotions(promotions)
      assert(result.length == 2)
      assert(result.map(_.promotionCodes.contains("3")).reduce((a, b) => a && b))
      assert(result.filter(_.promotionCodes.contains("1")).length == 1)
      assert(result.filter(_.promotionCodes.contains("2")).length == 1)
    }

    it("Get All Combinable Promotions without exclusions") {
      val promotions = Seq(
        Promotion("1", Seq()),
        Promotion("2", Seq()),
        Promotion("3", Seq())
      )

      val result = Promotions.allCombinablePromotions(promotions)
      assert(result.length == 1)
      assert(result(0).promotionCodes.length == 3)
    }
  }

  describe("Combinable Promotion") {
    it("Combinable with item not in promotions") {
      val promotions = Seq(
        Promotion("1", Seq()),
        Promotion("2", Seq("3")),
        Promotion("3", Seq("2"))
      )

      val result = Promotions.combinablePromotions("6", promotions)
      assert(result.length == 0)
    }

    it("Combinable with item without exclusion") {
      val promotions = Seq(
        Promotion("1", Seq()),
        Promotion("2", Seq("3")),
        Promotion("3", Seq("2"))
      )

      val result = Promotions.combinablePromotions("1", promotions)
      assert(result.length == 2)
      assert(result.map(_.promotionCodes.contains("1")).reduce((a, b) => a && b))
    }

    it("Combinable with item with some exclusion") {
      val promotions = Seq(
        Promotion("1", Seq("4", "5")),
        Promotion("2", Seq()),
        Promotion("3", Seq()),
        Promotion("4", Seq("1")),
        Promotion("5", Seq("1"))
      )

      val result = Promotions.combinablePromotions("1", promotions)
      assert(result.length == 1)
      assert(result.map(_.promotionCodes.contains("1")).reduce((a, b) => a && b))
      assert(result.filter(v => v.promotionCodes.contains("4") || v.promotionCodes.contains("5")).length == 0)
    }
  }
}
