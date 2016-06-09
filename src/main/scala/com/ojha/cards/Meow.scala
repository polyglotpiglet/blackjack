package com.ojha.cards

import com.ojha.cards.Meow.Rawr.EvaluateOrder

//import cats.std.string._
//import cats.syntax.eq._

/**
  * Created by alexbate on 08/06/16.
  */
object Meow extends App {



  sealed trait Product
  case class BasicProduct(id: Int, price: Double) extends Product
  case class DiscountedProduct(product: Product, discount: Double) extends Product
  case object OutOfStock extends Product

  sealed trait Order
  case class StandardOrder(products: List[Product]) extends Order
  case object CancelledOrder extends Order
  case class ComplexOrder(orders: List[Order]) extends Order


  trait Evaluate[-A, T] {
    def evaluate(a: A): T
  }

  object Rawr {

    implicit object EvaluateProduct extends Evaluate[Product, Double] {
      override def evaluate(p: Product): Double = p match {
        case OutOfStock => 0
        case BasicProduct(_, price) => price
        case DiscountedProduct(discounted, discount) => evaluate(discounted) * (1 -discount)
      }
    }

    implicit object EvaluateOrder extends Evaluate[Order, Double] {
      override def evaluate(o: Order): Double = o match {
        case CancelledOrder => 0
        case StandardOrder(products) => products.foldLeft(0d) {
          case (acc, p) => acc + EvaluateProduct.evaluate(p)
        }
        case ComplexOrder(orders) => orders.foldLeft(0.0){
          case (acc, o) => acc + EvaluateOrder.evaluate(o)
        }

      }
    }
  }

//  object Evaluate {
////    implicit class EvaluateOps[-A, T] (a: A)(implicit ev: Evaluate[A, T]) {
//    //      def evaluate = ev.evaluate(a)
//    //    }
//
//    import EvaluateOrder._
//
//    implicit class EvaluateOps[-A, T] (a: A) {
//      def evaluate = implicitly[Evaluate[A, T]].evaluate(a)
//    }
//  }
//
//  val o1 = StandardOrder(BasicProduct(10, 10.2) :: Nil)
//  val o2 = StandardOrder(DiscountedProduct(product = BasicProduct(11, 1.0), discount = 0.2) :: Nil)
//  val o3 = CancelledOrder
//
//  val order = ComplexOrder(o1 :: o2 :: o3 :: Nil)
//
////  import EvaluateOrder._
//  import Evaluate._
//  println(order.evaluate)


}
