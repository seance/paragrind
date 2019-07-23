package pb2.rules

import pb2.domain._
import pb2.context._
import pb2.control._
import cats._
import cats.implicits._

class Rules[F[_]: Functor]()(
  implicit
  F1: BattleLogging[F],
) {
  import F1._

  val rng = new java.security.SecureRandom

  object d4 extends Die(rng, sides=4)

  object d20 extends Die(rng, sides=20)

  def resolveRanged(
    round: Int,
    ranged: RangedAction,
    sourceState: NpcState,
    targetState: NpcState,
  ): F[NpcState] = {

    val attacker = ranged.source.character
    val defender = ranged.target.character

    val attackModifiers =
      if (ranged.isMarksman) attacker.skills.marksman
      else attacker.skills.throwing

    val attackRoll = d20.guardDisadvantage(isRangedDisadvantage(sourceState), attackModifiers)

    val defenseModifiers = List(
      defender.skills.evasion,
      defender.gear.shield.map(_.shieldValue).getOrElse(0)
    ).sum

    val defenseRoll = d20.guardDisadvantage(isEvasionDisadvantage(targetState), defenseModifiers)

    if (attackRoll.modified <= defenseRoll.modified) {
      val missLog = RangedMissLog(round, attacker, defender, attackRoll, defenseRoll)
      return tell(List(missLog)) as NpcState()
    }

    val weapon =
      if (ranged.isMarksman) attacker.gear.weaponMarksman.get
      else attacker.gear.weaponThrowing.get

    val multiplier = getDamageMultiplier(attackRoll, defenseRoll)

    val damage = math.max(0, List(
      multiplier,
      if (ranged.isMarksman) attacker.gear.weaponMarksman.map(_.baseDamage).getOrElse(0)
      else attacker.gear.weaponThrowing.map(_.baseDamage).getOrElse(0),
    ).product)

    val penetrated = math.max(0, List(
      damage,
      -defender.gear.armor.map(_.armorValue).getOrElse(0),
      -defender.gear.shield.map(_.shieldValue).getOrElse(0),
    ).sum)

    val hitLocation = getHitLocation()
    val pain = math.ceil(penetrated.toDouble / defender.stats.fortitude.toDouble).toInt
    val wounds = getWounds(penetrated, hitLocation, defender)

    val hitLog = RangedHitLog(
      round, attacker, defender, attackRoll, defenseRoll, weapon,
      multiplier, damage, penetrated, hitLocation, pain, wounds,
    )

    tell(List(hitLog)) as NpcState(pain, wounds)
  }

  def resolveSkirmish(
    round: Int,
    skirmish: Skirmish,
    npcStates: Map[Npc, NpcState],
  ): F[PhaseResolution] = {

    def attackRolls(group: Set[Npc]) = group.map { n =>
      val state = npcStates(n)
      n -> (if (isAbleToSkirmish(state)) {
        d20.guardDisadvantage(isMeleeDisadvantage(state), n.character.skills.melee)
      } else {
        d20.guardDisadvantage(isEvasionDisadvantage(state), n.character.skills.evasion)
      })
    }.toMap

    val protagonistRolls = attackRolls(skirmish.protagonists)
    val antagonistRolls = attackRolls(skirmish.antagonists)

    val (pMaxNpc, pMaxMod) = protagonistRolls.maxBy { case (_, r) => r.modified }
    val (pMinNpc, pMinNat) = protagonistRolls.minBy { case (_, r) => r.natural }
    val (aMaxNpc, aMaxMod) = antagonistRolls.maxBy { case (_, r) => r.modified }
    val (aMinNpc, aMinNat) = antagonistRolls.minBy { case (_, r) => r.natural }

    if (pMaxMod.modified == aMaxMod.modified && pMaxMod.natural == aMaxMod.natural) {
      val missLog = SkirmishMissLog(round, pMaxNpc.character, aMaxNpc.character, pMaxMod, aMaxMod, EqualRolls)
      return tell(List(missLog)) as PhaseResolution()
    }

    val (attackerNpc, attackRoll, defenderNpc, defenseRoll) = () match {
      case _ if pMaxMod.modified > aMaxMod.modified => (pMaxNpc, pMaxMod, aMinNpc, aMaxMod)
      case _ if aMaxMod.modified > pMaxMod.modified => (aMaxNpc, aMaxMod, pMinNpc, pMaxMod)
      case _ if aMaxMod.natural > pMaxMod.natural => (pMaxNpc, pMaxMod, aMinNpc, aMaxMod)
      case _ => (aMaxNpc, aMaxMod, pMinNpc, pMaxMod)
    }

    if (!isAbleToSkirmish(npcStates(attackerNpc))) {
      val missLog = SkirmishMissLog(round, pMaxNpc.character, aMaxNpc.character, pMaxMod, aMaxMod, DefensiveAttacker)
      return tell(List(missLog)) as PhaseResolution()
    }

    val attacker = attackerNpc.character
    val defender = defenderNpc.character
    val weapon = attacker.gear.weaponMelee

    val multiplier = getDamageMultiplier(attackRoll, defenseRoll)

    val damage = math.max(0, List(
      multiplier,
      weapon.baseDamage,
    ).product)

    val penetrated = math.max(0, List(
      damage,
      -defender.gear.armor.map(_.armorValue).getOrElse(0),
      -defender.gear.shield.map(_.shieldValue).getOrElse(0),
    ).sum)

    val hitLocation = getHitLocation()
    val pain = math.ceil(penetrated.toDouble / defender.stats.fortitude.toDouble).toInt
    val wounds = getWounds(penetrated, hitLocation, defender)

    val hitLog = SkirmishHitLog(
      round, attacker, defender, attackRoll, defenseRoll, weapon,
      multiplier, damage, penetrated, hitLocation, pain, wounds,
    )

    tell(List(hitLog)) as PhaseResolution(Map(defenderNpc -> NpcState(pain, wounds)))
  }

  def isAbleToSkirmish(state: NpcState): Boolean = {
    state.action.forall {
      case _: RangedAction => false
      case _: CastAction => false
      case _: EvadeAction => false
      case _ => true
    }
  }

  def isRangedDisadvantage(state: NpcState): Boolean = {
    List(ArmFracture, LegFracture).exists(state.wounds.contains)
  }

  def isMeleeDisadvantage(state: NpcState): Boolean = {
    List(ArmFracture, LegFracture).exists(state.wounds.contains)
  }

  def isEvasionDisadvantage(state: NpcState): Boolean = {
    List(LegFracture).exists(state.wounds.contains)
  }

  def isIncapacitatingWounds(wounds: Set[Wound]): Boolean = {
    List(HeadSevered, TorsoCrushed, ArmSevered, LegSevered, SkullFracture).exists(wounds.contains)
  }

  def getDamageMultiplier(higher: Roll, lower: Roll): Int = {
    (higher.modified - lower.modified) match {
      case x if x > 10 => 3
      case x if x > 5 => 2
      case _ => 1
    }
  }

  def getHitLocation(): HitLocation = d4().natural match {
    case 1 => d4().natural match {
      case 1 | 2 => Head
      case 3 | 4 => Torso
    }
    case 2 => Torso
    case 3 => Arms
    case 4 => Legs
  }

  def getWounds(damage: Int, hitLocation: HitLocation, target: Character): Set[Wound] = {
    damage match {
      case superCrit if superCrit >= target.stats.critical * 3 => hitLocation match {
        case Head => Set(HeadSevered, Bleeding)
        case Torso => Set(TorsoCrushed)
        case Arms => Set(ArmSevered)
        case Legs => Set(LegSevered)
      }
      case crit if crit >= target.stats.critical => hitLocation match {
        case Head => Set(SkullFracture, Bleeding)
        case Torso => Set(Bleeding)
        case Arms => Set(ArmFracture)
        case Legs => Set(LegFracture)
      }
      case _ => Set()
    }
  }
}

object Rules {

  def isBleeding(state: NpcState): Boolean = {
    state.wounds.contains(Bleeding)
  }

  def isIncapacitated(npc: Npc, state: NpcState): Boolean = {
    (state.fatigue >= npc.character.stats.endurance) ||
      state.wounds.exists {
        case _: SuperCritical => true
        case SkullFracture => true
        case _ => false
      }
  }

  def getNpcRank(npc: Npc, battleState: BattleState): Rank = {
    battleState.positions(npc)
  }

  def getNextRank(rank: Rank, battlefield: Battlefield, isProtagonist: Boolean): Option[Rank] = {
    battlefield.ranks.get(if (isProtagonist) rank.index + 1 else rank.index - 1)
  }

  def getEnemiesOnRank(rank: Rank, battle: Battle, state: BattleState, isProtagonist: Boolean): List[Npc] = {
    state.positions.filter {
      case (npc, npcRank) => (npcRank.index == rank.index) && (
        if (isProtagonist) battle.antagonists.contains(npc)
        else battle.protagonists.contains(npc)
      )
    }.keys.toList
  }

  def getFrontRank(battle: Battle, state: BattleState, isProtagonist: Boolean): Rank = {
    val enemyPositions = state.positions.filter { case (npc, _) =>
      !isIncapacitated(npc, state.npcStates(npc)) && (
        if (isProtagonist) battle.antagonists.contains(npc)
        else battle.protagonists.contains(npc)
      )
    }
    if (enemyPositions.isEmpty) {
      return (
        if (isProtagonist) battle.battlefield.ranks.last
        else battle.battlefield.ranks.head
      )
    }
    if (isProtagonist) {
      enemyPositions.minBy { case (_, rank) => rank.index }._2
    } else {
      enemyPositions.maxBy { case (_, rank) => rank.index }._2
    }
  }

  def isBeyondFront(rank: Rank, frontRank: Rank, battle: Battle, isProtagonist: Boolean): Boolean = {
    if (isProtagonist) rank.index > frontRank.index
    else rank.index < frontRank.index
  }
}
