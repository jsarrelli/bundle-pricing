package yoppworks.hackerchallenges.bundlepricing.calculator

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{CatalogItem, Quantity}

protected[calculator] case class PurchaseItems(items: Map[CatalogItem, Quantity]) {
  def totalPrice: Int = items.map { case (item, quantity) =>
    item.unitPrice.value * quantity.value
  }.sum
}
