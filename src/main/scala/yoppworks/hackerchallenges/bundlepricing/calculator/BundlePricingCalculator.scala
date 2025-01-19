package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._

import scala.annotation.tailrec

case class BundlePricingCalculator(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {
  protected[calculator] val promotionsSortedByBestDiscount = bundlePromotions.sortBy(bundlePromotion => -getDiscountPercentage(bundlePromotion))

  def calculateBestPurchase(cart: Cart): Price = {
    val cartItemsAsMap = cart.cartItems.map(cartItem => cartItem.catalogItem -> cartItem.quantity).toMap
    val applicablePromotions = promotionsSortedByBestDiscount.collect(meetsBundleCondition(cartItemsAsMap))

    @tailrec
    def iterate(partialPurchase: PartialPurchase): PartialPurchase =
      applicablePromotions.collectFirst(meetsBundleCondition(partialPurchase.getItems)) match {
        case None => partialPurchase
        case Some(promotion) => iterate(partialPurchase.applyPromotion(promotion))
      }

    val bestPurchase = iterate(PartialPurchase(List.empty, PurchaseItems.apply(cartItemsAsMap)))

    bestPurchase.totalPrice
  }


  protected[calculator] def meetsBundleCondition(cartItems: Map[CatalogItem, Quantity]): PartialFunction[BundlePromotion, BundlePromotion] = {
    case bundlePromotion
      if bundlePromotion.cartItems.forall(bundleCartItem =>
        cartItems.get(bundleCartItem.catalogItem).exists(_.value >= bundleCartItem.quantity.value)
      ) =>
      bundlePromotion
  }

  protected[calculator] def getDiscountPercentage(bundlePromotion: BundlePromotion) = {
    val realPrice = bundlePromotion.cartItems.map(item => item.catalogItem.unitPrice.value * item
      .quantity.value).sum
    val discountedPrice = bundlePromotion.totalDiscountedPrice.value
    (((realPrice - discountedPrice) / realPrice.toDouble) * 100)
  }
}
