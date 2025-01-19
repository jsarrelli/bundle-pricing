package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{BundlePromotion, CatalogItem, Price, Quantity}
import yoppworks.hackerchallenges.bundlepricing.calculator.PartialPurchase.{InsufficientQuantityException, ProductNotFound}

protected[calculator] case class PartialPurchase(
  purchaseItems: PurchaseItems,
  appliedPromotions: List[BundlePromotion] = List.empty,
) {
  def applyPromotion(promotion: BundlePromotion): PartialPurchase =
    copy(
      appliedPromotions = appliedPromotions :+ promotion,
      purchaseItems = PurchaseItems(extractItemsForPromotion(promotion)),
    )

  /**
   * Applies the given bundle promotion by subtracting the required items and quantities from the cart.
   *
   * @param bundlePromotion
   *   The bundle promotion to be applied.
   * @return
   *   A new cart with the items subtracted based on the promotion's requirements.
   * @throws ProductNotFound
   *   If any required product for the promotion is missing from the cart.
   * @throws InsufficientQuantityException
   *   If the cart does not contain sufficient quantity for a required product.
   */
  protected[calculator] def extractItemsForPromotion(bundlePromotion: BundlePromotion): Map[CatalogItem, Quantity] =
    bundlePromotion.cartItems.foldLeft(purchaseItems.items)((partialCart, bundleCartItem) =>
      partialCart.get(bundleCartItem.catalogItem) match {
        case Some(quantity) if quantity.value < bundleCartItem.quantity.value  =>
          throw InsufficientQuantityException(bundleCartItem.catalogItem, bundleCartItem.quantity, quantity)
        case Some(quantity) if quantity.value == bundleCartItem.quantity.value =>
          partialCart - bundleCartItem.catalogItem
        case Some(quantity)                                                    =>
          partialCart + (bundleCartItem.catalogItem -> Quantity(quantity.value - bundleCartItem.quantity.value))
        case None                                                              => throw ProductNotFound(bundleCartItem.catalogItem)
      }
    )

  def getItems: Map[CatalogItem, Quantity] = purchaseItems.items

  def totalPrice: Price = Price(
    appliedPromotions.map(_.totalDiscountedPrice).map(_.value).sum + purchaseItems.totalPrice
  )

}

object PartialPurchase {
  sealed trait PartialPurchaseException extends Exception

  case class ProductNotFound(item: CatalogItem) extends PartialPurchaseException

  case class InsufficientQuantityException(item: CatalogItem, required: Quantity, actual: Quantity)
      extends PartialPurchaseException
}
