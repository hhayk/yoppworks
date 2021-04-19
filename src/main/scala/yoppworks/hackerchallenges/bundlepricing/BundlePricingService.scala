package yoppworks.hackerchallenges.bundlepricing

import yoppworks.hackerchallenges.bundlepricing.BundlePricingDomain._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class BundlePricingService(catalog: Seq[CatalogItem], bundlePromotions: Seq[BundlePromotion]) {
  /**
   * Group cart item to bundles to get the lowest possible cart price
   *
   * @return
   * Success: cart price in cents, example Price(2250) => $22.50
   * Failure: InvalidCartException if the cart isn't valid (contains an item which doesn't exist in catalog)
   */
  def bundleCartToLowestPrice(cart: Cart): Try[Price] = {
    type CQ = Map[CatalogItem, Quantity]

    val cartItemsToMap = (cartItems: Seq[CartItem]) => cartItems.view.map(a => a.catalogItem -> a.quantity).toMap
    val mapsDiff = (m1: CQ, m2: CQ) => {
      m1
        .map {
          case (catalogItem, quantity) => {
            m2.keySet
              .find(_.name == catalogItem.name)
              .fold(catalogItem -> quantity.value)(m2CatalogItem => {
                val diff = quantity.value - m2(m2CatalogItem).value
                catalogItem -> diff
              })
          }
        }
        .collect {
          case (catalogItem, quantityValue) if quantityValue > 0 => (catalogItem, Quantity(quantityValue))
        }
    }

    if (isCartValid(cart)) {
      @tailrec
      def calc(cartItemsMap: CQ, prices: Seq[Price]): Seq[Price] = {
        if (cartItemsMap.isEmpty) prices
        else {
          val (newCartItemsMap, price) = evaluate(cartItemsMap, bundlePromotions)
          calc(newCartItemsMap, prices :+ price)
        }
      }

      def evaluate(cartItemsMap: CQ, bundlePromotions: Seq[BundlePromotion]): (CQ, Price) = {
        val bundlePromotionPrices = bundlePromotions
          .map(bundlePromotion => {
            val bundleApplicable = bundlePromotion.cartItems.forall(bundleCartItem => {
              cartItemsMap.keySet
                .find(_.name == bundleCartItem.catalogItem.name)
                .fold(false)(key => cartItemsMap(key).value >= bundleCartItem.quantity.value)
            })

            if (bundleApplicable) {
              val bundlePromotionMap = cartItemsToMap(bundlePromotion.cartItems)
              val diffMap = mapsDiff(cartItemsMap, bundlePromotionMap)

              (diffMap, bundlePromotion.totalDiscountedPrice, false)
            } else {
              val emptyMap: CQ = Map()

              (emptyMap, Price(0), true)
            }
          })
          .filterNot { case (_, _, shouldIgnore) => shouldIgnore }
          .map{ case (map, price, _) => (map, price)}

        Try(bundlePromotionPrices.minBy { case (_, price) => price.value })
          .toOption match {
            case Some(value) => value
            case None => {
              val (catalogItem, quantity) = cartItemsMap.head // Guaranteed to be, on other case `calc` recursion would be over
              val newMap = cartItemsMap - catalogItem
              val price = Price(catalogItem.unitPrice.value * quantity.value)

              (newMap, price)
            }
        }
      }

      val prices = calc(cartItemsToMap(cart.cartItems), Nil)
      Success(Price(prices.map(_.value).sum))
    } else {
      Failure(InvalidCartException)
    }
  }

  def isCartValid(cart: Cart): Boolean = cart.cartItems.forall(cartItem => catalog.exists(_.name == cartItem.catalogItem.name))

}