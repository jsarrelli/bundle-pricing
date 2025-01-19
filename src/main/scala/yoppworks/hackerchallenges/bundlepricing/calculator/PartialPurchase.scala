package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{BundlePromotion, CatalogItem, Price, Quantity}

protected[calculator] case class PartialPurchase(appliedPromotions: List[BundlePromotion], items: PurchaseItems) {
  def applyPromotion(promotion: BundlePromotion): PartialPurchase =
    copy(appliedPromotions :+ promotion, items = PurchaseItems(takeItemsFromCart(promotion, getItems)))

  def getItems: Map[CatalogItem, Quantity] = items.items

  def totalPrice: Price = Price(appliedPromotions.map(_.totalDiscountedPrice).map(_.value).sum + items.totalPrice)

  def takeItemsFromCart(
    bundlePromotion: BundlePromotion,
    cartItems: Map[CatalogItem, Quantity],
  ): Map[CatalogItem, Quantity] =
    bundlePromotion.cartItems.foldLeft(cartItems)((accum, bundleCartItem) =>
      accum.get(bundleCartItem.catalogItem) match {
        case Some(quantity) if quantity.value < bundleCartItem.quantity.value  =>
          throw new Exception(s"Unable to apply bundle $bundlePromotion, not enough items")
        case Some(quantity) if quantity.value == bundleCartItem.quantity.value => accum - bundleCartItem.catalogItem
        case Some(quantity)                                                    =>
          accum + (bundleCartItem.catalogItem -> Quantity(quantity.value - bundleCartItem.quantity.value))
        case None                                                              => throw new Exception("Unable to apply bundle, no item present")
      }
    )
}
