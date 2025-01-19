package yoppworks.hackerchallenges.bundlepricing.calculator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{CartItem, CatalogItem, Price, Quantity}
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions.{BundleDiscountOnItemUnitPrice, BundleTotalPriceDiscount, MaybeDiscountedItem}
import yoppworks.hackerchallenges.bundlepricing.calculator.PartialPurchase.{InsufficientQuantityException, ProductNotFound}

class PartialPurchaseSpec extends AnyWordSpec with Matchers {

  val appleCatalogItem     = CatalogItem("Apple", Price(199))
  val margarineCatalogItem = CatalogItem("Margarine", Price(250))
  val breadCatalogItem     = CatalogItem("Bread", Price(300))
  val bananaCatalogItem    = CatalogItem("Banana", Price(210))
  val cheeseCatalogItem    = CatalogItem("Cheese", Price(400))
  val milkCatalogItem      = CatalogItem("Milk", Price(350))

  // 1 apple 1.99 , 2 apples 2.15
  val promotion1 =
    BundleTotalPriceDiscount(
      Seq(CartItem(appleCatalogItem, Quantity(2))),
      totalPrice = Price(215),
    )

  // 1 bread + 2 margarines, the 2nd margarine is free
  val promotion2 =
    BundleDiscountOnItemUnitPrice(
      Seq(
        MaybeDiscountedItem(CartItem(appleCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        // 2nd margarine Free!
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = Some(Price(0))),
      )
    ) // 550

  val bunldes = List(promotion1, promotion2)

  "extractItemsForPromotion" should {
    "subtract the required items for a single promotion from the cart and return the updated cart" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(2))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val result          = partialPurchase.extractItemsForPromotion(promotion1)
      result shouldBe Map(
        breadCatalogItem     -> Quantity(1),
        appleCatalogItem     -> Quantity(3),
        margarineCatalogItem -> Quantity(2),
      )
    }

    "subtract multiple items for a promotion from the cart and return the updated cart" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(2))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val result          = partialPurchase.extractItemsForPromotion(promotion2)
      result shouldBe Map(appleCatalogItem -> Quantity(4))
    }

    "throw ProductNotFound when a required product for the promotion is missing from the cart" in {
      val purchaseItems   = PurchaseItems(Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5)))
      val partialPurchase = PartialPurchase(purchaseItems)
      val ex              = intercept[ProductNotFound](partialPurchase.extractItemsForPromotion(promotion2))
      ex shouldBe ProductNotFound(margarineCatalogItem)
    }

    "throw InsufficientQuantityException when the cart does not have enough quantity of a required product" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(1))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val ex              = intercept[InsufficientQuantityException](partialPurchase.extractItemsForPromotion(promotion2))
      ex shouldBe InsufficientQuantityException(
        item = margarineCatalogItem,
        required = Quantity(2),
        actual = Quantity(1),
      )
    }
  }

  "totalPrice" should {
    "calculate the total price of all items in the cart when no promotions are applied" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(2))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val result          = partialPurchase.totalPrice
      val expected        = (breadCatalogItem.unitPrice.value * 1 + appleCatalogItem.unitPrice.value * 5
        + margarineCatalogItem.unitPrice.value * 2)
      result.value shouldBe expected
    }

    "calculate the total price of applied promotions when there are no remaining isolated items" in {
      val purchaseItems   = PurchaseItems(Map.empty)
      val partialPurchase = PartialPurchase(purchaseItems, List(promotion1, promotion2))
      val result          = partialPurchase.totalPrice
      val expected        = (promotion1.totalDiscountedPrice.value + promotion2.totalDiscountedPrice.value)
      result.value shouldBe expected
    }

    "calculate the total price by combining item prices and applied promotions" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(2))
      )
      val partialPurchase = PartialPurchase(purchaseItems, List(promotion1, promotion2))
      val result          = partialPurchase.totalPrice
      val expected        =
        (promotion1.totalDiscountedPrice.value + promotion2.totalDiscountedPrice.value) + (breadCatalogItem.unitPrice.value * 1 + appleCatalogItem.unitPrice.value * 5
          + margarineCatalogItem.unitPrice.value * 2)
      result.value shouldBe expected
    }
  }

  "applyPromotion" should {
    "apply a promotion, deduct the required items from the cart, and return the updated cart with the applied promotion" in {
      val purchaseItems   = PurchaseItems(
        Map(breadCatalogItem -> Quantity(1), appleCatalogItem -> Quantity(5), margarineCatalogItem -> Quantity(2))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val result          = partialPurchase.applyPromotion(promotion2)
      result shouldBe PartialPurchase(
        purchaseItems = PurchaseItems(Map(appleCatalogItem -> Quantity(4))),
        appliedPromotions = List(promotion2),
      )
    }
  }

}
