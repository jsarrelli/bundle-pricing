package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._

import scala.annotation.tailrec

case class BundlePricingCalculator(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {

  /**
   * A sorted list of promotions, ordered by the highest discount percentage in descending order. A promotion is
   * considered better than another if it offers a larger discount percentage.
   */
  protected[calculator] val promotionsSortedByBestDiscount =
    bundlePromotions.sortBy(bundlePromotion => -getDiscountPercentage(bundlePromotion))

  /**
   * Calculates the optimal combination of promotions that can be applied to the given cart to achieve the lowest
   * possible price.
   *
   * @param cart
   *   The cart containing items to evaluate for applicable promotions.
   * @return
   *   The lowest possible price after applying the best combination of promotions.
   */
  def calculateBestPurchase(cart: Cart): Price = {
    val cartItemsAsMap       = cart.cartItems.map(cartItem => cartItem.catalogItem -> cartItem.quantity).toMap
    val applicablePromotions = promotionsSortedByBestDiscount.collect(meetsBundleCondition(cartItemsAsMap))

    @tailrec
    def applyPromotions(partialPurchase: PartialPurchase): PartialPurchase =
      applicablePromotions.collectFirst(meetsBundleCondition(partialPurchase.getItems)) match {
        case None            => partialPurchase
        case Some(promotion) => applyPromotions(partialPurchase.applyPromotion(promotion))
      }

    val bestPurchase = applyPromotions(PartialPurchase(PurchaseItems.apply(cartItemsAsMap)))

    bestPurchase.totalPrice
  }

  /**
   * Verifies whether the cart contains all the required items and their respective quantities to qualify for the given
   * bundle promotion.
   *
   * @param cartItems
   *   A map representing the items in the cart and their quantities.
   * @return
   *   A defined partial function if the condition is met, or undefined if not.
   */
  protected[calculator] def meetsBundleCondition(
    cartItems: Map[CatalogItem, Quantity]
  ): PartialFunction[BundlePromotion, BundlePromotion] = {
    case bundlePromotion
        if bundlePromotion.cartItems.forall(bundleCartItem =>
          cartItems.get(bundleCartItem.catalogItem).exists(_.value >= bundleCartItem.quantity.value)
        ) =>
      bundlePromotion
  }

  /**
   * Calculates the discount percentage for the given promotion.
   *
   * @param bundlePromotion
   *   The promotion for which the discount percentage is to be determined.
   * @return
   *   The discount percentage as a double.
   */
  protected[calculator] def getDiscountPercentage(bundlePromotion: BundlePromotion) = {
    val realPrice       = bundlePromotion.cartItems.map(item => item.catalogItem.unitPrice.value * item.quantity.value).sum
    val discountedPrice = bundlePromotion.totalDiscountedPrice.value
    ((realPrice - discountedPrice) / realPrice.toDouble) * 100
  }
}
