package pb2

import pb2.context._
import cats._
import cats.mtl._

object execution extends ExecutionInstances

trait ExecutionInstances {

  implicit def monadBattle[F[_]](
    implicit
    _functor: Functor[F],
    _applicative: Applicative[F],
    _monad: Monad[F],
    applicativeAsk: ApplicativeAsk[F, Battle],
    monadState: MonadState[F, BattleState],
    functorTell: FunctorTell[F, List[BattleLog]],
  ): MonadBattle[F] =
    new Monad[F]
      with BattleConfig[F]
      with BattleStateful[F]
      with BattleLogging[F]
    {
      // Members declared in cats.Applicative
      def pure[A](x: A): F[A] = applicative.pure(x)

      // Members declared in cats.mtl.ApplicativeAsk
      val applicative: Applicative[F] = _applicative
      def ask: F[Battle] = applicativeAsk.ask
      def reader[A](f: Battle => A): F[A] = applicativeAsk.reader(f)

      // Members declared in cats.FlatMap
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = _monad.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => F[Either[A,B]]): F[B] = _monad.tailRecM(a)(f)

      // Members declared in cats.mtl.FunctorTell
      val functor: Functor[F] = _functor
      def tell(l: List[BattleLog]): F[Unit] = functorTell.tell(l)
      def tuple[A](ta: (List[BattleLog], A)): F[A] = functorTell.tuple(ta)
      def writer[A](a: A, l: List[BattleLog]): F[A] = functorTell.writer(a, l)

      // Members declared in cats.mtl.MonadState
      def get: F[BattleState] = monadState.get
      def inspect[A](f: BattleState => A): F[A] = monadState.inspect(f)
      def modify(f: BattleState => BattleState): F[Unit] = monadState.modify(f)
      val monad: Monad[F] = _monad
      def set(s: BattleState): F[Unit] = monadState.set(s)
    }
}
