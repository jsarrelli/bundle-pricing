package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{BundlePromotion, CatalogItem, Price, Quantity}
import yoppworks.hackerchallenges.bundlepricing.calculator.PartialPurchase.{InsufficientQuantityException, ProductNotFound}

protected[calculator] case class PartialPurchase(purchaseItems: PurchaseItems, appliedPromotions: List[BundlePromotion] = List.empty) {
  def applyPromotion(promotion: BundlePromotion): PartialPurchase =
    copy(appliedPromotions = appliedPromotions :+ promotion, purchaseItems = PurchaseItems
    (takeItemsFromCart(promotion)))

  def getItems: Map[CatalogItem, Quantity] = purchaseItems.items

  def totalPrice: Price = Price(appliedPromotions.map(_.totalDiscountedPrice).map(_.value).sum + purchaseItems.totalPrice)

  protected[calculator] def takeItemsFromCart(bundlePromotion: BundlePromotion): Map[CatalogItem,
    Quantity] =
    bundlePromotion.cartItems.foldLeft(purchaseItems.items)((accum, bundleCartItem) =>
      accum.get(bundleCartItem.catalogItem) match {
        case Some(quantity) if quantity.value < bundleCartItem.quantity.value => throw
          InsufficientQuantityException(bundleCartItem.catalogItem, bundleCartItem.quantity, quantity)
        case Some(quantity) if quantity.value == bundleCartItem.quantity.value => accum - bundleCartItem.catalogItem
        case Some(quantity) => accum + (bundleCartItem.catalogItem -> Quantity(quantity.value - bundleCartItem.quantity.value))
        case None => throw ProductNotFound(bundleCartItem.catalogItem)
      }
    )

}


object PartialPurchase {
  sealed trait PartialPurchaseException extends Exception

  case class ProductNotFound(item: CatalogItem) extends PartialPurchaseException

  case class InsufficientQuantityException(item: CatalogItem, required: Quantity, actual: Quantity) extends
    PartialPurchaseException
}
