package week5

trait RNG {
  def nextInt: (Int, RNG)

}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

   def nonNegativeInt(rng: RNG): (Int, RNG) = {
     val (n, rng2) = rng.nextInt
     if (n >= 0) (n, rng2)
     else (math.abs(n+1), rng2)
   }

  // Exercise 2 (CB 6.2)

   def double(rng: RNG): (Double, RNG) = {
     val (n, rng2) = rng.nextInt
     (n/(Int.MaxValue +1).toDouble, rng2)
   }

  // Exercise 3 (CB 6.3)

   def intDouble(rng: RNG): ((Int, Double), RNG) = {
     val (a, rng2) = rng.nextInt
     val (b, rng3) = double(rng2)
     ((a,b), rng3)
   }

   def doubleInt(rng: RNG): ((Double, Int), RNG) = {
     val (a, rng2) = double(rng)
     val (b, rng3) = rng2.nextInt
     ((a,b), rng3)
   }

   def double3(rng: RNG): ((Double, Double, Double), RNG) = {
     val (a, rng2) = double(rng)
     val (b, rng3) = double(rng2)
     val (c, rng4) = double(rng3)
     ((a,b,c), rng4)

   }

   def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List(), rng)
    else {
      val (h, rng2) = rng.nextInt
      val (t, rng3) = ints(count-1)(rng2)
      (h::t, rng3)
    }
   }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

   def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

   val _double: Rand[Double] = map(nonNegativeInt)(i => i/((Int.MaxValue +1).toDouble))
  // Exercise 6 (CB 6.6)

   def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng)
      (f(a,b), rng3)
    }


  // this is given in the book

   def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

   val randIntDouble: Rand[(Int, Double)] = both(int, double)

   val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)


   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((x,xs) => map2(x,xs)(_::_))

//  List.fill(n)(x)
   def _ints(count: Int): Rand[List[Int]] = {
     sequence(List.fill(count)(int))
   }

  // Exercise 8 (6.8)

//  def flatMap[B] (f: A=>Option[B]) : Option[B] = map(f) getOrElse None

   def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
     rng => {
       val (a, r1) = f(rng)
       g(a)(r1) // We pass the new state along
     }

   def nonNegativeLessThan(n: Int): Rand[Int] = {
     flatMap(nonNegativeInt)(i => {
       val mod = i%n
       if (i + (n-1) - mod >= 0) unit(mod)
       else nonNegativeLessThan(n)
     })
   }

}

import State._
import week5.RNG.Simple

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
  // This is given in the book:

   def modify[S](f: S => S): State[S, Unit] = for {
     s <- get // Gets the current state and assigns it to `s`.
     _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
   } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = {
    val (x,y) = s.run(seed)
    Stream.cons(x, state2stream(s)(y))

  }

  // Exercise 11

  val random_integers = state2stream(State.random_int)(Simple(42)).take(10).toList


}


// vim:cc=80:foldmethod=indent:foldenable
