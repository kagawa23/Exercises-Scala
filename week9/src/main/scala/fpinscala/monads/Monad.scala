// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package week9.src.main.scala.fpinscala.monads

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A,B] (fa: F[A]) (f: A => B) :F[B]

  def distribute[A,B] (fab: F[(A,B)]): (F[A],F[B]) =
    (map (fab) (_._1), map (fab)(_._2))

  def codistribute[A,B] (e :Either[F[A],F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map (fa) (Left(_))
    case Right(fb) => map (fb) (Right(_))
  }

}

object Functor {

  val ListFunctor = new Functor[List] {
    def map[A,B] (as: List[A]) (f: A => B): List[B] = as.map (f)
  }

  // Exercise 10

   val OptionFunctor = new Functor[Option] {
     def map[A,B] (op: Option[A]) (f: A=> B): Option[B] = op.map(f)
   }

}

trait Monad[F[_]] {

  def unit[A]  (a: => A): F[A]
  def flatMap[A,B] (ma: F[A]) (f: A => F[B]) :F[B]

  def map[A,B] (ma: F[A]) (f: A => B) :F[B] =
    flatMap (ma) (a => unit (f(a)))

  def map2[A, B, C] (ma: F[A], mb: F[B]) (f: (A,B) => C): F[C] =
    flatMap (ma) (a => map (mb) (b => f(a,b)))

  // Exercise 13 (CB11.3)
// inspiration from week3
//  def sequence[A](aos: List[Option[A]]): Option[List[A]] = {
//    aos.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
//  }

   def sequence[A] (lfa: List[F[A]]): F[List[A]] = {
     lfa.foldRight[F[List[A]]](unit(List[A]()))((x,y) => map2(x, y)(_::_))
   } //Sequence takes a list of monads and merges them into one, which generates a list.

  // traverse seems to simply sequence results of mapping.  I do not think that
  // it appeared in our part. You can uncomment it once you have sequence.
   def traverse[A,B] (la: List[A]) (f: A => F[B]): F[List[B]] = sequence(la.map (f))

  // Exercise 14 (CB11.4)

   def replicateM[A] (n: Int, ma: F[A]): F[List[A]] =
     sequence(List.tabulate(n)(_ => ma))

  def join[A] (mma: F[F[A]]): F[A] = flatMap (mma) (ma => ma)

  // Exercise 15 is solved in MonadSpec.scala

  // Exercise 16 (CB11.7)

   def compose[A,B,C] (f: A => F[B], g: B => F[C]): A => F[C] =
     a => flatMap(f(a))(g)

}

object Monad {

  // Exercise 12 (CB11.1)

   val optionMonad = new Monad[Option] {
     def unit [A] (a: => A) :  Option[A] = Option(a)

     override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap(f)
   }

   val listMonad = new Monad[List] {
     override def unit[A](a: => A): List[A] = List(a)

     override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma.flatMap(f)
   }

}

//object Test extends App {
//  val m1: Monad[List] = m1.replicateM(3, List[A](1))
//}
