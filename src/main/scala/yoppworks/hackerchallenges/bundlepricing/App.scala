package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._
import yoppworks.hackerchallenges.bundlepricing.BundlePromotions.{BundleDiscountOnItemUnitPrice, BundleTotalPriceDiscount, MaybeDiscountedItem}

object App {
  def main(args: Array[String]): Unit = {
    // build a test catalog
    val appleCatalogItem = CatalogItem("Apple", Price(199))
    val margarineCatalogItem = CatalogItem("Margarine", Price(250))
    val breadCatalogItem = CatalogItem("Bread", Price(300))
    val catalogExample = Seq(appleCatalogItem, margarineCatalogItem, breadCatalogItem)

    // set the bundle deals
    val currentBundles = Seq(
      // 1 apple 1.99 , 2 apples 2.15
      BundleTotalPriceDiscount(
        Seq(CartItem(appleCatalogItem, Quantity(2))),
        totalPrice = Price(215)
      ),
      // 1 bread + 2 margarines, the 2nd margarine is free
      BundleDiscountOnItemUnitPrice(
        Seq(
          MaybeDiscountedItem(CartItem(breadCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
          MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = None),
          // 2nd margarine Free!
          MaybeDiscountedItem(CartItem(margarineCatalogItem, Quantity(1)), optionalUnitPriceOverride = Some(Price(0)))
        )
      )
    )

    // ps = pricingService
    val ps = new BundlePricingService(catalogExample, currentBundles)

    // get the cart price with bundle applied
    println("Price of the cart using bundles: " + ps.bundleCartToLowestPrice(Cart(Seq(CartItem(appleCatalogItem, Quantity(2))))))
  }
}
