package pb2.domain

import java.security.SecureRandom
import collection.mutable

case class Character(
  name: String,
  stats: Stats = Stats(),
  skills: Skills = Skills(),
  gear: Gear = Gear(),
  casts: List[Cast] = List.empty,
)

case class Stats(
  critical: Int = 10,
  endurance: Int = 6,
  fortitude: Int = 6,
)

case class Skills(
  melee: Int = 0,
  throwing: Int = 0,
  marksman: Int = 0,
  evasion: Int = 0,
)

case class Gear(
  weaponMelee: Weapon = Weapon("Fists", 4),
  weaponThrowing: Option[Weapon] = None,
  weaponMarksman: Option[Weapon] = None,
  shield: Option[Shield] = None,
  armor: Option[Armor] = None,
)

case class Weapon(
  name: String,
  baseDamage: Int,
)

case class Shield(
  name: String,
  shieldValue: Int,
)

case class Armor(
  name: String,
  armorValue: Int,
)

sealed trait Cast {
  def apply(): Unit
}
case class NoCast() extends Cast {
  def apply() = ()
}

sealed trait HitLocation
case object Head extends HitLocation
case object Torso extends HitLocation
case object Arms extends HitLocation
case object Legs extends HitLocation

sealed trait Wound

sealed trait Critical extends Wound
case object SkullFracture extends Critical
case object Bleeding extends Critical
case object ArmFracture extends Critical
case object LegFracture extends Critical

sealed trait SuperCritical extends Wound
case object HeadSevered extends SuperCritical
case object TorsoCrushed extends SuperCritical
case object ArmSevered extends SuperCritical
case object LegSevered extends SuperCritical

case class Battlefield(ranks: List[Rank])

case class Rank(index: Int)

class Die(rng: SecureRandom, sides: Int) {

  def apply(modifiers: Int = 0): Roll = Roll(Normal, sides, rng.nextInt(sides) + 1, modifiers)

  def advantage(modifiers: Int = 0): Roll = {
    mutable.Buffer.fill(2)(apply(modifiers)).maxBy(_.natural).copy(mode=Advantage)
  }

  def disadvantage(modifiers: Int = 0): Roll = {
    mutable.Buffer.fill(2)(apply(modifiers)).minBy(_.natural).copy(mode=Disadvantage)
  }

  def maybeAdvantage(isAdvantage: Boolean, modifiers: Int = 0): Roll =
    if (isAdvantage) advantage(modifiers) else apply(modifiers)

  def maybeDisadvantage(isDisadvantage: Boolean, modifiers: Int = 0): Roll =
    if (isDisadvantage) disadvantage(modifiers) else apply(modifiers)
}

sealed trait RollMode
case object Normal extends RollMode
case object Advantage extends RollMode
case object Disadvantage extends RollMode

case class Roll(
  mode: RollMode,
  dieSides: Int,
  natural: Int,
  modifiers: Int,
) {
  lazy val modified: Int = natural + modifiers
}
