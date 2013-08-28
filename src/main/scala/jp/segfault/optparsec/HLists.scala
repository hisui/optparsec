package jp.segfault.optparsec

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.higherKinds

object HLists {

  import N._

  trait HList {
    def foldl[X](value:X)(f: (X, Any) => X):X = value
  }

  object HList {
  }

  object `[]` extends HList {
    override def toString() = "[]"
  }

  type `[]` = `[]`.type

  case class ::[+A,+B <: HList](head:A, tail:B) extends HList {

    override def toString() = tail.foldl("[" + head)(_ + "," + _ ) + "]"

    override def foldl[X]
    (value:X)(f: (X, Any) => X):X = tail.foldl(f(value, head))(f)
  }

  trait HLMap[F[_,_],X <: HList,Z <: HList] {
    def apply(f: Any => Any, o:X):Z
  }

  object HLMap {
    implicit def case0[F[_,_]] = new HLMap[F,`[]`,`[]`] {
      def apply(f: Any => Any, o:`[]`):`[]` = `[]`
    }

    implicit def case1[F[_,_],H1,T1 <: HList,H2,T2 <: HList]
    (implicit ev:HLMap[F,T1,T2], ev2:F[H1,H2]) = new HLMap[F,H1::T1,H2::T2] {
      def apply(f: Any => Any, o:H1::T1):H2::T2 = f(o.head).asInstanceOf[H2] :: ev(f, o.tail)
    }
  }

  trait Zip[A <: HList,B <: HList,Z <: HList] {
    def apply(a:A, b:B):Z
  }

  object Zip {
    implicit def case0[H,T <: HList] = new Zip[`[]`,H::T,`[]`] {
      def apply(a:`[]`, b:H::T):`[]` = `[]`
    }
    implicit def case1[H,T <: HList] = new Zip[H::T,`[]`,`[]`] {
      def apply(a:H::T, b:`[]`):`[]` = `[]`
    }
    implicit def case2 = new Zip[`[]`,`[]`,`[]`] {
      def apply(a:`[]`, b:`[]`):`[]` = `[]`
    }
    implicit def case3[H1,T1 <: HList,H2,T2 <: HList,T3 <: HList]
    (implicit zip:Zip[T1,T2,T3]) = new Zip[H1::T1,H2::T2,(H1,H2)::T3] {
      def apply(a:H1::T1, b:H2::T2):(H1,H2)::T3 = (a.head, b.head) :: zip(a.tail, b.tail)
    }
  }

  trait Reverse[Y,X <: HList,Z <: HList] {
    def apply(o:Y, acc:X):Z
  }

  object Reverse {
    implicit def case0[X <: HList] = new Reverse[`[]`,X,X] {
      def apply(o:`[]`, acc:X):X = acc
    }

    implicit def case1[H,T <: HList,X <: HList,Z <: HList]
    (implicit ev:Reverse[T,H::X,Z]) = new Reverse[H::T,X,Z] {
      def apply(o: H::T, acc:X):Z = ev(o.tail, o.head :: acc)
    }
  }

  trait Append[Y <: HList,X <: HList,Z <: HList] {
    def apply(a:Y, b:X):Z
  }

  object Append {
    implicit def case0[X <: HList] = new Append[`[]`,X,X] {
      def apply(a:`[]`, b:X):X = b
    }

    implicit def case1[H,T <: HList,X <: HList,Z <: HList]
    (implicit ev:Append[T,X,Z]) = new Append[H::T,X,H::Z] {
      def apply(a: H::T, b:X):H::Z = a.head :: ev(a.tail, b)
    }
  }

  trait Head[X <: HList,Z] {
    type Out = Z
    def apply(o:X):Z
  }

  object Head {
    implicit def case0[H,T <: HList] = new Head[H::T,H] {
      override type Out = H
      def apply(o: H::T):H = o.head
    }
  }

  trait Tail[X <: HList,Z <: HList] {
    type Out = Z
    def apply(o:X):Z
  }

  object Tail {
    implicit def case0[H,T <: HList] = new Tail[H::T,T] {
      def apply(o: H::T):T = o.tail
    }
  }

  trait Last[X <: HList,Z] {
    type Out = Z
    def apply(o:X):Z
  }

  object Last {
    implicit def case0[A <: HList,B <: HList,C]
    (implicit reverse:Reverse[A,`[]`,B], head:Head[B,C]) = new Last[A,C] {
      override type Out = C
      def apply(o:A):C = head(reverse(o, `[]`))
    }
  }

  trait Take[X <: HList,N <: Num,Z <: HList] {
    type Out = Z
    def apply(o:X):Z
  }

  object Take {
    implicit def case0[Z <: HList] = new Take[Z,`0`,`[]`] {
      def apply(o:Z):`[]` = `[]`
    }
    implicit def case1[H,T <: HList,Z <: HList,I <: Num,J <: Num]
    (implicit ev1: (I - `1`) as J, ev2:Take[T,J,Z]) = new Take[H::T,I,H::Z] {
      def apply(o: H::T): H::Z = o.head :: ev2(o.tail)
    }
  }

  trait Drop[X <: HList,N <: Num,Z <: HList] {
    type Out = Z
    def apply(o:X):Z
  }

  object Drop {
    implicit def case0[X <: HList] = new Drop[X,`0`,X] {
      def apply(o:X):X = o
    }
    implicit def case1[H,T <: HList,Z <: HList,I <: Num,J <: Num]
    (implicit ev1: (I - `1`) as J, ev2:Drop[T,J,Z]) = new Drop[H::T,I,Z] {
      def apply(o: H::T):Z = ev2(o.tail)
    }
  }

  trait Len[X <: HList,Z <: Num] {
    def apply(o:X):Int
  }

  object Len {
    implicit def case0 = new Len[`[]`,`0`] {
      def apply(o:`[]`):Int = 0
    }
    implicit def case1[H,T <: HList,I <: Num,J <: Num]
    (implicit ev1:Len[T,J], ev2: (J + `1`) as I) = new Len[H::T,I] {
      def apply(o: H::T):Int = ev1(o.tail) + 1
    }
  }

  trait ToTuple[X,Z <: Product] {
    type In  = X
    type Out = Z
    def apply(o:X):Z
  }

  trait ToHList[X,Z <: HList] {
    type In  = X
    type Out = Z
    def apply(o:X):Z
  }

  class MapCurried[A <: HList,F[_,_]](val raw:A) extends AnyVal {
    def apply[B <: HList](f:Any => Any)(implicit map:HLMap[F,A,B]):B = map(f, raw)
  }

  implicit class HListOps[A <: HList](val raw:A) extends AnyVal {

    def ::[B](e:B) = new `::`(e, raw)

    def tuple[B <: Product](implicit ev:A ToTuple B):B = ev(raw)

    def reverse[Z <: HList](implicit reverse:Reverse[A,`[]`,Z]):Z = reverse(raw, `[]`)

    def ++ [B <: HList,Z <: HList](b:B)(implicit append:Append[A,B,Z]):Z = append(raw, b)

    def tail[B <: HList](implicit tail:Tail[A,B]):B = tail(raw)

    def head[B](implicit head:Head[A,B]):B = head(raw)

    def last[B](implicit last:Last[A,B]):B = last(raw)

    def take[N <: Num](implicit f:Take[A,N,_ <: HList]):f.Out = f(raw).asInstanceOf[f.Out] // explicit casting for IDEA

    def drop[N <: Num](implicit f:Drop[A,N,_ <: HList]):f.Out = f(raw).asInstanceOf[f.Out] // explicit casting for IDEA

    def size(implicit len:Len[A,_]) = len(raw)

    def map[F[_,_]]:MapCurried[A,F] = new MapCurried(raw)

    def zip[B <: HList,Z <: HList](b:B)(implicit zip:Zip[A,B,Z]):Z = zip(raw, b)

  }

  implicit class TupleOps[A <: Product](val raw:A) extends AnyVal {

    def hlist[B <: HList](implicit ev:A ToHList B):B = ev(raw)

    def inverse[B <: HList,C <: HList,D <: Product]
    (implicit ev1: A ToHList B, ev2:Reverse[B,`[]`,C], ev3: C ToTuple D):D = ev3(ev2(ev1(raw), `[]`))

    def :+ [B,C <: HList,D <: HList,E <: Product]
    (rhs:B)(implicit ev1: A ToHList C, ev2:Append[C,B::`[]`,D], ev3: D ToTuple E):E = ev3(ev1(raw) ++ (rhs::`[]`))

    def +: [B,C <: HList,D <: HList,E <: Product]
    (lhs:B)(implicit ev1: A ToHList C, ev2:Append[B::`[]`,C,D], ev3: D ToTuple E):E = ev3((lhs::`[]`) ++ ev1(raw))

    def _n[B <: HList,C <: HList,D <: Product]
    (implicit ev1: A ToHList B, ev2:Last[B,C], ev3: C ToTuple D):D = ev3(ev1(raw).last)

    def rshift[X <: HList,B <: HList,C <: Product]
    (implicit ev1: A ToHList X, wt1: X <:< (Any::B), ev2: B ToTuple C):C = ev2(wt1(ev1(raw)).tail)

    def lshift[X <: HList,B <: HList,C <: HList,D <:HList,E <: Product]
    (implicit
       ev1: A ToHList B
     , ev2:Reverse[B,`[]`,X], wt1: X <:< (Any::C)
     , ev3:Reverse[C,`[]`,D]
     , ev4: D ToTuple E):E = ev4(wt1(ev1(raw).reverse).tail.reverse)

  }

  implicit def tuple1[A] = new ToTuple[A::`[]`.type,Tuple1[A]] {
    def apply(z:In):Out = z match { case a::_ => Tuple1(a) }
  }

  implicit def hlist1[A] = new ToHList[Tuple1[A],A::`[]`] {
    def apply(z:In):Out = z._1::`[]`
  }

  if (false) (2 to 22).foreach { i =>
    print(
      """
        |implicit def tuple<#>[<A,>] = new ToTuple[<A::>::`[]`,(<A,>)] {
        |  def apply(z:In):Out = z match { case <a::>::`[]` => (<a,>) }
        |}
      """.stripMargin
        .replace(  "<#>", i + "")
        .replace( "<A,>", (0 until i).map('A' + _ toChar).mkString( ","))
        .replace( "<a,>", (0 until i).map('a' + _ toChar).mkString( ","))
        .replace("<A::>", (0 until i).map('A' + _ toChar).mkString("::"))
        .replace("<a::>", (0 until i).map('a' + _ toChar).mkString("::")))
  }

  implicit def tuple2[A,B] = new ToTuple[A::B::`[]`,(A,B)] {
    def apply(z:In):Out = z match { case a::b::`[]` => (a,b) }
  }

  implicit def tuple3[A,B,C] = new ToTuple[A::B::C::`[]`,(A,B,C)] {
    def apply(z:In):Out = z match { case a::b::c::`[]` => (a,b,c) }
  }

  implicit def tuple4[A,B,C,D] = new ToTuple[A::B::C::D::`[]`,(A,B,C,D)] {
    def apply(z:In):Out = z match { case a::b::c::d::`[]` => (a,b,c,d) }
  }

  implicit def tuple5[A,B,C,D,E] = new ToTuple[A::B::C::D::E::`[]`,(A,B,C,D,E)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::`[]` => (a,b,c,d,e) }
  }

  implicit def tuple6[A,B,C,D,E,F] = new ToTuple[A::B::C::D::E::F::`[]`,(A,B,C,D,E,F)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::`[]` => (a,b,c,d,e,f) }
  }

  implicit def tuple7[A,B,C,D,E,F,G] = new ToTuple[A::B::C::D::E::F::G::`[]`,(A,B,C,D,E,F,G)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::`[]` => (a,b,c,d,e,f,g) }
  }

  implicit def tuple8[A,B,C,D,E,F,G,H] = new ToTuple[A::B::C::D::E::F::G::H::`[]`,(A,B,C,D,E,F,G,H)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::`[]` => (a,b,c,d,e,f,g,h) }
  }

  implicit def tuple9[A,B,C,D,E,F,G,H,I] = new ToTuple[A::B::C::D::E::F::G::H::I::`[]`,(A,B,C,D,E,F,G,H,I)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::`[]` => (a,b,c,d,e,f,g,h,i) }
  }

  implicit def tuple10[A,B,C,D,E,F,G,H,I,J] = new ToTuple[A::B::C::D::E::F::G::H::I::J::`[]`,(A,B,C,D,E,F,G,H,I,J)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::`[]` => (a,b,c,d,e,f,g,h,i,j) }
  }

  implicit def tuple11[A,B,C,D,E,F,G,H,I,J,K] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::`[]`,(A,B,C,D,E,F,G,H,I,J,K)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::`[]` => (a,b,c,d,e,f,g,h,i,j,k) }
  }

  implicit def tuple12[A,B,C,D,E,F,G,H,I,J,K,L] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l) }
  }

  implicit def tuple13[A,B,C,D,E,F,G,H,I,J,K,L,M] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m) }
  }

  implicit def tuple14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n) }
  }

  implicit def tuple15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) }
  }

  implicit def tuple16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) }
  }

  implicit def tuple17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) }
  }

  implicit def tuple18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) }
  }

  implicit def tuple19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s) }
  }

  implicit def tuple20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t) }
  }

  implicit def tuple21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u) }
  }

  implicit def tuple22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] = new ToTuple[A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::V::`[]`,(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V)] {
    def apply(z:In):Out = z match { case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::u::v::`[]` => (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v) }
  }


  if (false) (2 to 22).foreach { i =>
    print(
      """
        |implicit def hlist<#>[<,>] = new ToHList[(<,>),<A::>::`[]`] {
        |  def apply(z:In):Out = <_::>::`[]`
        |}
      """.stripMargin
        .replace(  "<#>", i + "")
        .replace(  "<,>", (0 until i).map('A' + _ toChar).mkString( ","))
        .replace("<A::>", (0 until i).map('A' + _ toChar).mkString("::"))
        .replace("<_::>", (1 to i).map("z._" + _).mkString("::")))
  }

  implicit def hlist2[A,B] = new ToHList[(A,B),A::B::`[]`] {
    def apply(z:In):Out = z._1::z._2::`[]`
  }

  implicit def hlist3[A,B,C] = new ToHList[(A,B,C),A::B::C::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::`[]`
  }

  implicit def hlist4[A,B,C,D] = new ToHList[(A,B,C,D),A::B::C::D::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::`[]`
  }

  implicit def hlist5[A,B,C,D,E] = new ToHList[(A,B,C,D,E),A::B::C::D::E::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::`[]`
  }

  implicit def hlist6[A,B,C,D,E,F] = new ToHList[(A,B,C,D,E,F),A::B::C::D::E::F::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::`[]`
  }

  implicit def hlist7[A,B,C,D,E,F,G] = new ToHList[(A,B,C,D,E,F,G),A::B::C::D::E::F::G::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::`[]`
  }

  implicit def hlist8[A,B,C,D,E,F,G,H] = new ToHList[(A,B,C,D,E,F,G,H),A::B::C::D::E::F::G::H::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::`[]`
  }

  implicit def hlist9[A,B,C,D,E,F,G,H,I] = new ToHList[(A,B,C,D,E,F,G,H,I),A::B::C::D::E::F::G::H::I::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::`[]`
  }

  implicit def hlist10[A,B,C,D,E,F,G,H,I,J] = new ToHList[(A,B,C,D,E,F,G,H,I,J),A::B::C::D::E::F::G::H::I::J::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::`[]`
  }

  implicit def hlist11[A,B,C,D,E,F,G,H,I,J,K] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K),A::B::C::D::E::F::G::H::I::J::K::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::`[]`
  }

  implicit def hlist12[A,B,C,D,E,F,G,H,I,J,K,L] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L),A::B::C::D::E::F::G::H::I::J::K::L::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::`[]`
  }

  implicit def hlist13[A,B,C,D,E,F,G,H,I,J,K,L,M] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M),A::B::C::D::E::F::G::H::I::J::K::L::M::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::`[]`
  }

  implicit def hlist14[A,B,C,D,E,F,G,H,I,J,K,L,M,N] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N),A::B::C::D::E::F::G::H::I::J::K::L::M::N::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::`[]`
  }

  implicit def hlist15[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::`[]`
  }

  implicit def hlist16[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::`[]`
  }

  implicit def hlist17[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::`[]`
  }

  implicit def hlist18[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::z._18::`[]`
  }

  implicit def hlist19[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::z._18::z._19::`[]`
  }

  implicit def hlist20[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::z._18::z._19::z._20::`[]`
  }

  implicit def hlist21[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::z._18::z._19::z._20::z._21::`[]`
  }

  implicit def hlist22[A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V] = new ToHList[(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V),A::B::C::D::E::F::G::H::I::J::K::L::M::N::O::P::Q::R::S::T::U::V::`[]`] {
    def apply(z:In):Out = z._1::z._2::z._3::z._4::z._5::z._6::z._7::z._8::z._9::z._10::z._11::z._12::z._13::z._14::z._15::z._16::z._17::z._18::z._19::z._20::z._21::z._22::`[]`
  }

}
