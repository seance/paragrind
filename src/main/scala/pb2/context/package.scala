package pb2

import cats._
import cats.mtl._

package object context {

  type MonadBattle[F[_]] = Monad[F]
    with BattleConfig[F]
    with BattleStateful[F]
    with BattleLogging[F]

  type BattleConfig[F[_]] = ApplicativeAsk[F, Battle]
  type BattleStateful[F[_]] = MonadState[F, BattleState]
  type BattleLogging[F[_]] = FunctorTell[F, List[BattleLog]]
}
