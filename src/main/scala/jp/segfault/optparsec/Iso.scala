package jp.segfault.optparsec

trait Iso[A,B] { self =>

  def apply(a:A):B

  def unapply(b:B):Option[A]

  def in:Iso[A,Tuple1[B]] = Iso(a => Tuple1(apply(a)), b => unapply(b._1))

  def let[T](f: ((A) => B, B => Option[A]) => T):T = f(apply, unapply)

  def inverse:Iso[B,A] = Iso(e => self.unapply(e).get, e => Some(self(e)))

  def compose[C](iso:Iso[B,C]):Iso[A,C] =
    Iso((iso.apply _).compose(self.apply), e => iso.unapply(e).flatMap(self.unapply))

}

object Iso {

  def id[T]:Iso[Tuple1[T],T] = Iso(e => e._1, e => Some(Tuple1(e)))

  def in[T]:Iso[Tuple1[T],Tuple1[T]] = id[T].in

  def of[A,B](implicit ev:Iso[A,B]):Iso[A,B] = ev

  def apply[A,B](app: A => B, unapp: B => Option[A]):Iso[A,B] =
    new Iso[A,B] {

      def apply(a:A):B = app(a)

      def unapply(b:B):Option[A] = unapp(b)
    }
}
