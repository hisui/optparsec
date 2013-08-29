package jp.segfault.optparsec

import scala.util.Try
import scala.language.implicitConversions
import scala.language.postfixOps

object OptParsec {

  import HLists.{:: => *:, _}
  import Comments._

  case class ^[A,B](a:A, b:B)

  implicit class StringOps(val raw:String) {

    def unary_- :Opt[Boolean] = {
      val E = "-" + raw
      new Match ({
        case E::tl => Out( true, tl)
        case    tl => Out(false, tl)
      })
    } as new -(raw)
  }

  implicit def toOpt(raw:Symbol):Opt[String] = {
    val E = raw.name
    new Match({ case E :: tl => Out(E, tl) })
  }

  case class Out[+A](raw:A, next:List[String]) {
    def map[B](f: A => B):Out[B] = copy(f(raw))
  }

  trait Opt[A] extends (Seq[String] => Stream[A]) { lhs =>

    override def apply(args:Seq[String]):Stream[A] = {
      parse(args.toList).map(_.raw)
    }

    def parse(l:List[String]):Stream[Out[A]]

    def build:Comment = Uncommented

    def ## (text:String):Opt[A] = lhs as new `#`(lhs.build, text)

    def as[T <: Comment](f: => T):Opt[A] = new Opt[A] {
      override def parse(l:List[String]) = lhs.parse(l)
      override def build = f
    }

    def map[B](f: A => B):Opt[B] = new Opt[B] {
      def parse(l:List[String]):Stream[Out[B]] = lhs.parse(l).map(_.map(f))
    } as lhs.build

    def |[B <: X,X >: A](rhs:Opt[B]):Opt[X] = new Opt[X] {
      def parse(l:List[String]):Stream[Out[X]] =
        ( lhs.parse(l).asInstanceOf[Stream[Out[X]]] append
          rhs.parse(l).asInstanceOf[Stream[Out[X]]]
        ) 
    } as new |(lhs.build, rhs.build)

    def ? :Opt[Option[A]] = new Opt[Option[A]] {
      def parse(l:List[String]):Stream[Out[Option[A]]] =
        lhs.parse(l).map(_.map(Option(_))) append Stream(Out(None, l))
    } as new ?(lhs.build)

    def ~>[B](rhs:Opt[B]):Opt[B] = lhs / rhs map (_.b)

    def <~[B](rhs:Opt[B]):Opt[A] = lhs / rhs map (_.a)

    def %[B](rhs:Opt[B])(implicit wt: A =:= Boolean):Opt[B] = new Opt[B] {
      override def parse(l:List[String]):Stream[Out[B]] =
        lhs.parse(l).flatMap(e => if (wt(e.raw)) rhs.parse(e.next) else Stream())
    }

    def /[B](rhs:Opt[B]):Opt[A^B] = new Opt[A^B] {
      override def parse(l:List[String]):Stream[Out[A^B]] =
        for {
          a <- lhs.parse(l)
          b <- rhs.parse(a.next)
        } yield Out(^(a.raw, b.raw), b.next)
    } as new /(lhs.build, rhs.build)

    def *[B](rhs:Opt[B]):Opt[A^B] =
      lhs./(rhs) | rhs./(lhs).flip as new *(lhs.build, rhs.build)

    def tuple[B <: HList,C <: HList,T <: Product]
    (implicit ev0:Listed[A,B], ev1:Reverse[B,`[]`,C], ev2:C ToTuple T):Opt[T] = lhs.map(e => ev0(e).reverse.tuple)

    def flip[X,Y](implicit wt: Opt[A] <:< Opt[X^Y]):Opt[Y^X] = wt(lhs).map { case x^y => ^(y, x) }

  }

  private[optparsec] class Match[A](f: PartialFunction[List[String],Out[A]]) extends Opt[A] {

    def parse(l:List[String]):Stream[Out[A]] = Stream(l).collect(f)
  }

  def value[A](f: String => A) = new Match[A]({ case e :: tl => Out(f(e), tl) })

  trait LowPriorityImplicits {

    implicit def case1[A] = new Listed[A,A*:`[]`] {
      def apply(a:A):A*:`[]` = a :: `[]`
    }
  }

  trait Listed[A,Z <: HList] {
    def apply(o:A):Z
  }

  object Listed extends LowPriorityImplicits {

    implicit def case0[A,B,Z <: HList]
    (implicit ev:Listed[A,Z]) = new Listed[A^B,B*:Z] {
      def apply(o:A^B):B*:Z = o.b :: ev(o.a)
    }
  }

  type Cast[T] = String => Try[T]

  def of[T](implicit ev:Cast[T]):Opt[T] = value(ev(_).get)

  implicit val cast0:Cast[String] = e => Try(e)
  implicit val cast1:Cast[Int]    = e => Try(e.toInt)



}

object Comments {

  trait Comment

  case object Uncommented extends Comment

  case class ?(e:Comment) extends Comment

  case class |(a:Comment, b:Comment) extends Comment

  case class /(a:Comment, b:Comment) extends Comment

  case class *(a:Comment, b:Comment) extends Comment

  case class `#`(a:Comment, text:String) extends Comment

  case class -(name:String) extends Comment

}
