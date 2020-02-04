case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

object Promotions {
  /**
   *  Gets all the possible promo combinations for a sequence of promotions.
   *  The approach taken on this exercise was to use the method "combinablePromotions"
   *  on each on of the entries. Once all the the results are calculated, we use
   *  different data structures (e.g.: Maps) and algorithms (e.g.: sort) for filtering
   *  out duplicates. This algorithm may not be the most efficient one, but it is
   *  done is a fully functional way.
   *
   * @param allPromotions all promotions
   * @return Sequence with all promo combos
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allPromotions.flatMap(promo => combinablePromotions(promo.code, allPromotions))
      .map(promo => (promo.promotionCodes.mkString(","), promo))
      .toMap.values.toSeq

  /**
   *  Generates all promotions which can be combined with the provided promo code.
   * @param promotionCode Main Promotion code
   * @param allPromotions All promotions
   * @return Sequence of PromotionCombo
   */
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val promotionCombos = allPromotions.find(_.code == promotionCode).map(promotion => {
      val difference = allPromotions.filter(_.code != promotionCode)
      combinablePromotionsHelper(Seq(promotion), difference)
    }).getOrElse(Seq.empty)
      .map(promotions => PromotionCombo(promotions.map(_.code).sorted))

    promotionCombos.filterNot(combo => {
      val comboSet = combo.promotionCodes.toSet
      val possibleSuperSets = promotionCombos
        .filter(superSet => superSet.promotionCodes.length > combo.promotionCodes.length)
        .map(promo => promo.promotionCodes.toSet)
        .map(superSet => comboSet subsetOf superSet)
      possibleSuperSets.contains(true)
    })
  }

  private def combinablePromotionsHelper(currentCombination: Seq[Promotion],
                                 allPromotions: Seq[Promotion]): Seq[Seq[Promotion]] = allPromotions match {
    case Seq() => Seq(currentCombination)
    case first :: rest => {
      val mainPromo = currentCombination.head
      val combinableWithMainPromo = !mainPromo.notCombinableWith.contains(first.code)
      val currentCombinationTail = currentCombination.tail;
      val notCombinableWithTail = currentCombinationTail.filter(_.notCombinableWith.contains(first.code))

      if (!combinableWithMainPromo) {
        combinablePromotionsHelper(currentCombination, rest)
      } else if (combinableWithMainPromo && notCombinableWithTail.nonEmpty) {

        val combinableWithTail = currentCombination.filterNot(_.notCombinableWith.contains(first.code)) :+ first
        combinablePromotionsHelper(combinableWithTail, rest)
          .concat(combinablePromotionsHelper(currentCombination, rest))
          .filter(_.nonEmpty)
      } else {
        combinablePromotionsHelper(currentCombination :+ first, rest)
      }
    }
  }
}