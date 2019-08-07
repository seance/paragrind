package paragrind.context

import paragrind.domain._
import paragrind.control._

sealed trait BattleLog {
  val round: Int
}

case class ActionLog(round: Int, action: Action) extends BattleLog

sealed trait ResolutionLog extends BattleLog

sealed trait RangedLog extends ResolutionLog

case class RangedHitLog(
  round: Int,
  attacker: Character,
  defender: Character,
  attackRoll: Roll,
  defenseRoll: Roll,
  weapon: Weapon,
  damageMultiplier: Int,
  damageRaw: Int,
  damagePenetrated: Int,
  hitLocation: HitLocation,
  pain: Int,
  wounds: Set[Wound],
) extends RangedLog

case class RangedMissLog(
  round: Int,
  attacker: Character,
  defender: Character,
  attackRoll: Roll,
  defenseRoll: Roll,
) extends RangedLog

sealed trait SkirmishLog extends ResolutionLog

case class SkirmishesLog(
  round: Int,
  skirmishes: List[Skirmish]
) extends SkirmishLog

case class SkirmishHitLog(
  round: Int,
  attacker: Character,
  defender: Character,
  attackRoll: Roll,
  defenseRoll: Roll,
  weapon: Weapon,
  damageMultiplier: Int,
  damageRaw: Int,
  damagePenetrated: Int,
  hitLocation: HitLocation,
  pain: Int,
  wounds: Set[Wound],
) extends SkirmishLog

case class SkirmishMissLog(
  round: Int,
  protagonist: Character,
  antagonist: Character,
  protagonistRoll: Roll,
  antagonistRoll: Roll,
  missReason: SkirmishMissReason,
) extends SkirmishLog

sealed trait SkirmishMissReason
case object EqualRolls extends SkirmishMissReason
case object DefensiveAttacker extends SkirmishMissReason

sealed trait CastLog extends ResolutionLog
case class CastSuccessLog(round: Int, caster: Character, targets: List[Character], cast: Cast) extends CastLog
case class CastFailureLog(round: Int, caster: Character, targets: List[Character], cast: Cast) extends CastLog

case class MoveLog(
  round: Int,
  character: Character,
  from: Rank,
  to: Rank,
) extends ResolutionLog
