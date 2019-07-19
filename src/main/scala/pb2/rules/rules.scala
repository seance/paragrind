package pb2.rules

import pb2.domain._
import pb2.context._
import pb2.control._
import cats._
import cats.implicits._

object Rules {

  val rng = new java.security.SecureRandom

  object d4 extends Die(rng, sides=4)
  object d20 extends Die(rng, sides=20)

  def resolveRanged[F[_]: Functor](
    round: Int,
    ranged: RangedAction,
    sourceState: NpcState,
    targetState: NpcState,
  )(
    implicit
    F: BattleLogging[F],
  ): F[NpcState] = {

    val attacker = ranged.source.character
    val defender = ranged.target.character

    val attackModifiers =
      if (ranged.isMarksman) attacker.skills.marksman
      else attacker.skills.throwing

    val attackRoll = d20.maybeDisadvantage(isRangedDisadvantage(sourceState), attackModifiers)

    val defenseModifiers = List(
      defender.skills.evasion,
      defender.gear.shield.map(_.shieldValue).getOrElse(0)
    ).sum

    val defenseRoll = d20.maybeDisadvantage(isEvasionDisadvantage(targetState), defenseModifiers)

    if (attackRoll.modified <= defenseRoll.modified) {
      val missLog = RangedMissLog(round, attacker, defender, attackRoll, defenseRoll)
      F.tell(List(missLog)) as NpcState()
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

    F.tell(List(hitLog)) as NpcState(pain, wounds)
  }

  def resolveSkirmish[F[_]: Functor](
    round: Int,
    skirmish: Skirmish,
    npcStates: Map[Npc, NpcState],
  )(
    implicit
    F: BattleLogging[F],
  ): F[PhaseResolution] = {

    def attackRolls(group: Set[Npc]) = group.map { n =>
      val state = npcStates(n)
      n -> (if (isAbleToSkirmish(state)) {
        d20.maybeDisadvantage(isMeleeDisadvantage(state), n.character.skills.melee)
      } else {
        d20.maybeDisadvantage(isEvasionDisadvantage(state), n.character.skills.evasion)
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
      return F.tell(List(missLog)) as PhaseResolution()
    }

    val (attackerNpc, attackRoll, defenderNpc, defenseRoll) = () match {
      case _ if pMaxMod.modified > aMaxMod.modified => (pMaxNpc, pMaxMod, aMinNpc, aMaxMod)
      case _ if aMaxMod.modified > pMaxMod.modified => (aMaxNpc, aMaxMod, pMinNpc, pMaxMod)
      case _ if aMaxMod.natural > pMaxMod.natural => (pMaxNpc, pMaxMod, aMinNpc, aMaxMod)
      case _ => (aMaxNpc, aMaxMod, pMinNpc, pMaxMod)
    }

    if (!isAbleToSkirmish(npcStates(attackerNpc))) {
      val missLog = SkirmishMissLog(round, pMaxNpc.character, aMaxNpc.character, pMaxMod, aMaxMod, DefensiveAttacker)
      return F.tell(List(missLog)) as PhaseResolution()
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

    F.tell(List(hitLog)) as PhaseResolution(Map(defenderNpc -> NpcState(pain, wounds)))
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
    val rankIndex = battlefield.ranks.indexOf(rank)
    if (rankIndex >= 0) {
      battlefield.ranks.get(if (isProtagonist) rankIndex + 1 else rankIndex - 1)
    } else {
      None
    }
  }

  def getEnemiesOnRank(rank: Rank, battle: Battle, state: BattleState, isProtagonist: Boolean): List[Npc] = {
    state.positions.filter {
      case (npc, npcRank) => (npcRank eq rank) && (
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
    if (isProtagonist) {
      enemyPositions.minBy { case (_, rank) =>
          battle.battlefield.ranks.indexOf(rank)
      }._2
    } else {
      enemyPositions.maxBy { case (_, rank) =>
        battle.battlefield.ranks.indexOf(rank)
      }._2
    }
  }

  def isBeyondFront(rank: Rank, frontRank: Rank, battle: Battle, isProtagonist: Boolean): Boolean = {
    if (isProtagonist) battle.battlefield.ranks.indexOf(rank) > battle.battlefield.ranks.indexOf(frontRank)
    else battle.battlefield.ranks.indexOf(rank) < battle.battlefield.ranks.indexOf(frontRank)
  }
}
