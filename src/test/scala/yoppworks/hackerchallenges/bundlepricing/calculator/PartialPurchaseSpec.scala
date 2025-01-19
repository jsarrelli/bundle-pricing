package yoppworks.hackerchallenges.bundlepricing.calculator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain.{CartItem, CatalogItem, Price, Quantity}
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions.{BundleDiscountOnItemUnitPrice, BundleTotalPriceDiscount, MaybeDiscountedItem}
import yoppworks.hackerchallenges.bundlepricing.calculator.PartialPurchase.{InsufficientQuantityException, ProductNotFound}

class PartialPurchaseSpec extends AnyWordSpec with Matchers {

  val appleCatalogItem = CatalogItem("Apple", Price(199))
  val margarineCatalogItem = CatalogItem("Margarine", Price(250))
  val breadCatalogItem = CatalogItem("Bread", Price(300))
  val bananaCatalogItem = CatalogItem("Banana", Price(210))
  val cheeseCatalogItem = CatalogItem("Cheese", Price(400))
  val milkCatalogItem = CatalogItem("Milk", Price(350))

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
    ) //550

  val bunldes = List(promotion1, promotion2)
  "takeItemsFromCart" should {
    "take one item from cart returning the cart with subtracted items" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(2)))
      val partialPurchase = PartialPurchase(purchaseItems)
      val result = partialPurchase.takeItemsFromCart(promotion1)
      result shouldBe Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(3),
        margarineCatalogItem -> Quantity(2)
      )
    }

    "take multiple items from cart returning the cart with subtracted items" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(2)))
      val partialPurchase = PartialPurchase(purchaseItems)
      val result = partialPurchase.takeItemsFromCart(promotion2)
      result shouldBe Map(appleCatalogItem -> Quantity(4))
    }
    "failed if bundled product is not present on items" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val ex = intercept[ProductNotFound](partialPurchase.takeItemsFromCart(promotion2))
      ex shouldBe ProductNotFound(margarineCatalogItem)
    }
    "failed if quantity is not enough" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(1))
      )
      val partialPurchase = PartialPurchase(purchaseItems)
      val ex = intercept[InsufficientQuantityException](partialPurchase.takeItemsFromCart(promotion2))
      ex shouldBe InsufficientQuantityException(item = margarineCatalogItem, required = Quantity(2), actual = Quantity(1))
    }
  }

  "totalPrice" should {
    "calculate total price of purchase items if no promotions applied" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(2)))
      val partialPurchase = PartialPurchase(purchaseItems)
      val result = partialPurchase.totalPrice
      val expected = (breadCatalogItem.unitPrice.value * 1 + appleCatalogItem.unitPrice.value * 5
        + margarineCatalogItem.unitPrice.value * 2)
      result.value shouldBe expected
    }
    "calculate total price of bundle promotions if there is no isolated items left" in {
      val purchaseItems = PurchaseItems(Map.empty)
      val partialPurchase = PartialPurchase(purchaseItems, List(promotion1, promotion2))
      val result = partialPurchase.totalPrice
      val expected = (promotion1.totalDiscountedPrice.value + promotion2.totalDiscountedPrice.value)
      result.value shouldBe expected
    }
    "calculate total price for items and applied promotions" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(2)))
      val partialPurchase = PartialPurchase(purchaseItems, List(promotion1, promotion2))
      val result = partialPurchase.totalPrice
      val expected = (promotion1.totalDiscountedPrice.value + promotion2.totalDiscountedPrice
        .value) + (breadCatalogItem.unitPrice.value * 1 + appleCatalogItem.unitPrice.value * 5
        + margarineCatalogItem.unitPrice.value * 2)
      result.value shouldBe expected
    }
  }

  "applyPromotion" should {
    "apply promotion taking items from the cart" in {
      val purchaseItems = PurchaseItems(Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
        margarineCatalogItem -> Quantity(2)))
      val partialPurchase = PartialPurchase(purchaseItems)
      val result = partialPurchase.applyPromotion(promotion2)
      result shouldBe PartialPurchase(
        purchaseItems = PurchaseItems(Map(appleCatalogItem -> Quantity(4))),
        appliedPromotions = List(promotion2)
      )
    }
  }

}
