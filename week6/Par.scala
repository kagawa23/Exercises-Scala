package week6

import java.util.concurrent._

import scala.language.implicitConversions

// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.
// Exercise 7.2.
object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // Exercise 7.1 function shouldn't require that the two Par`inputs have the same type.

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  // Exercise 7.2. In order to respect timeouts, we'd need a new `Future`
  // implementation that records the amount of time spent evaluating one future,
  // then subtracts that time from the available time allocated for evaluating
  // the other future.
  def map2NewFuture[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
  (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    NewFuture(af, bf, f)
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(
    new Callable[A] {
      def call: A = a(es).get
    }
  )

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // Exercise 1 (CB7.4) convert any function A => B to one that evaluates its
  // result asynchronously
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  // map as shown in the book
  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
  map2(pa, unit(()))((a, _) => f(a))

  // Exercise 2 (CB7.5) Write this function, called sequence . No additional
  // primitives are required. Do not call run. One possible implementation
  // will be very similar in structure to a function
  // we've implemented previously, for `Option`.
  def sequence1[B](ps: List[Par[B]]): Par[List[B]] = {
    ps.foldRight[Par[List[B]]](unit(List()))((x, y) => map2(x, y)(_ :: _))

  }

  //forking the recursive step off to a new logical thread,
  def sequence2[B](as: List[Par[B]]): Par[List[B]] = {
    as match {
      case Nil => unit(Nil)
      case h :: t => {
        val tailParList: Par[List[B]] = sequence2(t)
        val parTail: Par[List[B]] = fork(tailParList)
        var concatenatedList = map2(h, parTail)(_ :: _)
        concatenatedList
      }
    }
  }

  // dividing the list in half, and running both halves in parallel.
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = {
    fork {
      if (as.isEmpty) {
        unit(Vector())
      }
      else if (as.length == 1) {
        val onePar: Par[IndexedSeq[A]] = map(as.head)(a => Vector(a))
        onePar
      }
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }
  }

  def sequence3[A](as: List[Par[A]]): Par[List[A]] = {
    val indexedSeq: Par[IndexedSeq[A]] = sequenceBalanced(as.toIndexedSeq)
    val parList: Par[List[A]] = map(indexedSeq)(_.toList)
    parList
  }


  // Exercise 3 (CB7.6)
  // this is shown in the book:
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence1(fbs)
  }

  //  filters elements of a list in parallel.
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[List[A]]] = as.map(asyncF((item: A) => {
      if (f(item)) {
        List(item)
      } else {
        List()
      }
    }))
    val pars: Par[List[List[A]]] = sequence1(fbs)
    val parsList: Par[List[A]] = map(pars)(_.flatten)
    parsList
  }

  // Exercise 4: implement map3 using map2
  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D)
  :Par[D] = {
    val firstPar: Par[(C) => D] = map2(pa, pb)((a, b) => (c: C) => f(a, b, c))
    map2(firstPar, pc)(_ (_))
  }

  // shown in the book
  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = {
    p1(e).get == p2(e).get
  }

  def choiceBook[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    es =>
      if (run(es)(cond).get) {
        t(es) //Notice we are blocking on the result of cond.
      }
      else {
        f(es)
      }
  }

  // Exercise 5 (CB7.11)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val chosenPar: Int = run(es)(n).get
      choices(chosenPar)(es)
    }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val parList: List[Par[A]] = List(t, f)
    val intPar: Par[Int] = es =>
      if (run(es)(cond).get) {
        unit(0)(es) //We are blocking on the result of cond.
      }
      else {
        unit(1)(es)
      }
    choiceN(intPar)(parList)
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(condition => if (condition == true) 0 else 1))(List(t, f))
  }

  // Exercise 6 (CB7.13)
  def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      val result: A = pa(es).get //We are blocking on the result of pa.
      f(result)(es)
    }
  }

  def choiceNviaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(intResult => choices(intResult))
  }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(cond)(condition => if (condition) t else f)
  }

  // Exercise 7 (CB7.14)
  //  Compare the type of join with the type of List.flatten (and the relation
  // of join to chooser against the relation of List.flatten to List.flatMap ).
  def join[A](a: Par[Par[A]]): Par[A] = {
    es => {
      val parA: Par[A] = run(es)(a).get()
      run(es)(parA)
    }
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    val parToJoin: Par[Par[B]] = map(pa)(f)
    join(parToJoin)
  }

  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] = {
    chooser(a)(par => par)
  }

  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  case class NewFuture[A, B, C](a: Future[A], b: Future[B],
                                f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }
}