package yoppworks.hackerchallenges.bundlepricing.calculator

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions.{BundleDiscountOnItemUnitPrice, BundleTotalPriceDiscount, MaybeDiscountedItem}

class BundlePricingCalculatorSpec extends AnyWordSpec with Matchers {

  val appleCatalogItem     = CatalogItem("Apple", Price(199))
  val margarineCatalogItem = CatalogItem("Margarine", Price(250))
  val breadCatalogItem     = CatalogItem("Bread", Price(300))
  val bananaCatalogItem    = CatalogItem("Banana", Price(210))
  val cheeseCatalogItem    = CatalogItem("Cheese", Price(400))
  val milkCatalogItem      = CatalogItem("Milk", Price(350))

  val catalogExample = Seq(appleCatalogItem, margarineCatalogItem, breadCatalogItem)

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
        MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        // 2nd margarine Free!
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = Some(Price(0))),
      )
    ) // 550

  // 1 apple 1.99 , 4 apples 4.10
  val promotion3 =
    BundleTotalPriceDiscount(
      Seq(CartItem(appleCatalogItem, Quantity(4))),
      totalPrice = Price(410),
    )

  // 2 apples + 1 bread + 2 margarines, the 2 apples are free
  val promotion4 =
    BundleDiscountOnItemUnitPrice(
      Seq(
        MaybeDiscountedItem(CartItem(appleCatalogItem, Quantity(2)), optionalUnitPriceOverride = Some(Price(0))),
        MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
        MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(2)), optionalUnitPriceOverride = None),
      )
    ) // 800

  val currentBundles          = Seq(promotion1, promotion2, promotion3, promotion4)
  val bundlePricingCalculator = BundlePricingCalculator(catalogExample, currentBundles)

  "calculateBestPurchase" should {

    "select the best bundle promotion when multiple apply, choosing the 4-apple discount instead of applying the 2-apple discount twice" in {
      val cart   = Cart(Seq(CartItem(appleCatalogItem, Quantity(4))))
      val result = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(410)
    }

    "apply multiple promotions optimally, combining the 4-apple discount with the 2-apple discount" in {
      val cart   = Cart(Seq(CartItem(appleCatalogItem, Quantity(6))))
      val result = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(410 + 215)
    }

    "apply multiple promotions optimally, combining promotions 1, 2, and 3" in {
      val cart   = Cart(
        Seq(
          CartItem(breadCatalogItem, Quantity(1)),
          CartItem(appleCatalogItem, Quantity(6)),
          CartItem(margarineCatalogItem, Quantity(2)),
        )
      )
      val result = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(410 + 550 + 215) // 1175
    }

    "apply a complex promotion involving bread, margarine, and apples, charging the remaining items at full price" in {
      val cart   = Cart(
        Seq(
          CartItem(breadCatalogItem, Quantity(1)),
          CartItem(appleCatalogItem, Quantity(5)),
          CartItem(margarineCatalogItem, Quantity(2)),
        )
      )
      val result = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(550 + 410 + 199) // 1159
    }

    "choose the best deal among multiple promotions, applying the 2-apple promotion and charging the banana separately" in {
      val promotion1              = BundleTotalPriceDiscount(
        Seq(CartItem(appleCatalogItem, Quantity(2))),
        totalPrice = Price(215),
      )
      val promotion2              = BundleTotalPriceDiscount(
        Seq(CartItem(appleCatalogItem, Quantity(1)), CartItem(bananaCatalogItem, Quantity(1))),
        totalPrice = Price(350),
      )
      val cart                    = Cart(
        Seq(
          CartItem(appleCatalogItem, Quantity(2)),
          CartItem(bananaCatalogItem, Quantity(1)),
        )
      )
      val currentBundles          = Seq(promotion1, promotion2)
      val bundlePricingCalculator = BundlePricingCalculator(catalogExample, currentBundles)
      val result                  = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(215 + 210) // 425
    }

    "select the best deal between two promotions for the same product" in {
      val promotion1              = BundleTotalPriceDiscount(
        Seq(CartItem(appleCatalogItem, Quantity(2))),
        totalPrice = Price(215),
      )
      val promotion2              = BundleTotalPriceDiscount(
        Seq(CartItem(appleCatalogItem, Quantity(3))),
        totalPrice = Price(500),
      )
      val cart                    = Cart(Seq(CartItem(appleCatalogItem, Quantity(3))))
      val currentBundles          = Seq(promotion1, promotion2)
      val bundlePricingCalculator = BundlePricingCalculator(catalogExample, currentBundles)
      val result                  = bundlePricingCalculator.calculateBestPurchase(cart)
      result shouldBe Price(215 + 199) // 414
    }
  }

  "meetsBundleCondition" should {

    "return true if the cart contains all necessary products and quantities for a promotion" in {
      val cart   = Map(
        breadCatalogItem     -> Quantity(1),
        appleCatalogItem     -> Quantity(5),
        margarineCatalogItem -> Quantity(2),
      )
      val bundle = BundleDiscountOnItemUnitPrice(
        Seq(
          MaybeDiscountedItem(CartItem(appleCatalogItem, Quantity(2)), optionalUnitPriceOverride = Some(Price(0))),
          MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
          MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(2)), optionalUnitPriceOverride = None),
        )
      )
      val result = bundlePricingCalculator.meetsBundleCondition(cart).isDefinedAt(bundle)
      result shouldBe true
    }

    "return false if the cart does not contain all necessary products for a promotion" in {
      val cart   = Map(
        breadCatalogItem -> Quantity(1),
        appleCatalogItem -> Quantity(5),
      )
      val bundle = BundleDiscountOnItemUnitPrice(
        Seq(
          MaybeDiscountedItem(CartItem(appleCatalogItem, Quantity(2)), optionalUnitPriceOverride = Some(Price(0))),
          MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
          MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(3)), optionalUnitPriceOverride = None),
        )
      )
      val result = bundlePricingCalculator.meetsBundleCondition(cart).isDefinedAt(bundle)
      result shouldBe false
    }

    "return false if the cart contains all necessary products but insufficient quantities for a promotion" in {
      val cart   = Map(
        breadCatalogItem     -> Quantity(1),
        appleCatalogItem     -> Quantity(5),
        margarineCatalogItem -> Quantity(2),
      )
      val bundle = BundleDiscountOnItemUnitPrice(
        Seq(
          MaybeDiscountedItem(CartItem(appleCatalogItem, Quantity(2)), optionalUnitPriceOverride = Some(Price(0))),
          MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
          MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(3)), optionalUnitPriceOverride = None),
        )
      )
      val result = bundlePricingCalculator.meetsBundleCondition(cart).isDefinedAt(bundle)
      result shouldBe false
    }
  }

  "promotionsSortedByBestDiscount" should {

    "sort promotions in descending order of their discount percentages" in {
      val currentBundles          = Seq(promotion1, promotion2, promotion3, promotion4)
      val bundlePricingCalculator = BundlePricingCalculator(catalogExample, currentBundles)
      val result                  = bundlePricingCalculator.promotionsSortedByBestDiscount
      result shouldBe Seq(promotion3, promotion1, promotion4, promotion2) // 48%, 45%, 33%, 31%
    }
  }

  "getDiscountPercentage" should {

    "calculate the correct discount percentage for each bundle promotion" in {
      val bundlePricingCalculator = BundlePricingCalculator(catalogExample, currentBundles)
      bundlePricingCalculator.getDiscountPercentage(promotion1).toInt shouldBe 45
      bundlePricingCalculator.getDiscountPercentage(promotion2).toInt shouldBe 31
      bundlePricingCalculator.getDiscountPercentage(promotion3).toInt shouldBe 48
      bundlePricingCalculator.getDiscountPercentage(promotion4).toInt shouldBe 33
    }
  }

}
