package paragrind

import cats._
import cats.mtl._

package object context {
  type BattleConfig[F[_]] = ApplicativeAsk[F, Battle]
  type BattleStateful[F[_]] = MonadState[F, BattleState]
  type BattleLogging[F[_]] = FunctorTell[F, List[BattleLog]]
}
