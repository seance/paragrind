package pb2

import pb2.context._
import pb2.control._

object Transcriber {

  def transcribeSimulationResult(result: SimulationResult): String = {
    (
      s"Simulations ran: ${result.simulationCount}" ::
      s"Mean rounds taken: ${result.meanRoundsTaken}" ::
      s"Total party kills: ${result.totalPartyKills}" ::
      s"Character results:" ::
      result.characterResults.toList.map { case (character, r) =>
        s"fatigue=${r.totalFatigue} crits=${r.totalCriticals} superCrits=${r.totalSuperCriticals} incaps=${r.totalIncapacitations} dying=${r.totalDying}: ${character.name}"
      } :::
      Nil
    ).mkString("\n")
  }

  def transcribeLogs(battle: Battle, logs: List[BattleLog]): List[String] = {
    logs.groupBy(_.round).toList
      .sortBy { case (round, _) => round }
      .iterator.flatMap { case (round, roundLogs) =>
      val actionLogs = roundLogs.collect { case a: ActionLog => a }
      val skirmishesLogs = roundLogs.collect { case a: SkirmishesLog => a }
      val resolveLogs = roundLogs.filter {
        case _: ActionLog => false
        case _: SkirmishesLog => false
        case _ => true
      }

      s"-- Round $round --" ::
      "Actions:" ::
      actionLogs.flatMap(transcribeLog(battle)) :::
      "Skirmishes:" ::
      skirmishesLogs.flatMap(transcribeLog(battle)) :::
      "Resolutions:" ::
      resolveLogs.flatMap(transcribeLog(battle)) :::
      Nil
    }.toList
  }

  def transcribeLog(battle: Battle): BattleLog => List[String] = {
    case ActionLog(_, RangedAction(source, target, isMarksman)) => List(s"${source.character.name} ${if (isMarksman) "SHOOT" else "THROW"} ${target.character.name}")
    case ActionLog(_, SkirmishAction(source, target, _)) => List(s"${source.character.name} SKIRMISH ${target.character.name}")
    case ActionLog(_, MoveAction(npc, from, to, _)) => List(s"${npc.character.name} MOVE ranks ${battle.battlefield.ranks.indexOf(from)} -> ${battle.battlefield.ranks.indexOf(to)}")
    case ActionLog(_, EvadeAction(npc)) => List(s"${npc.character.name} EVADES")
    case ActionLog(_, NoAction(npc)) => List(s"${npc.character.name} NO ACTION")
    case x: RangedHitLog => List(s"${x.attacker.name} HIT ranged ${x.defender.name}: (${x.attackRoll.natural}+${x.attackRoll.modifiers}=${x.attackRoll.modified} vs ${x.defenseRoll.natural}+${x.defenseRoll.modifiers}=${x.defenseRoll.modified} => ${x.damageMultiplier}x ${x.weapon.name} => ${x.damageRaw} => ${x.damagePenetrated} @ ${x.hitLocation}) ${x.pain}/${x.wounds}")
    case x: RangedMissLog => List(s"${x.attacker.name} MISSED ranged ${x.defender.name} (${x.attackRoll.natural}+${x.attackRoll.modifiers}=${x.attackRoll.modified} vs ${x.defenseRoll.natural}+${x.defenseRoll.modifiers}=${x.defenseRoll.modified})")
    case x: SkirmishesLog => x.skirmishes.map(s => s"${s.protagonists.map(n => n.character.name)} vs ${s.antagonists.map(n => n.character.name)}")
    case x: SkirmishHitLog => List(s"${x.attacker.name} HIT melee ${x.defender.name}: (${x.attackRoll.natural}+${x.attackRoll.modifiers}=${x.attackRoll.modified} vs ${x.defenseRoll.natural}+${x.defenseRoll.modifiers}=${x.defenseRoll.modified} => ${x.damageMultiplier}x ${x.weapon.name} => ${x.damageRaw} => ${x.damagePenetrated} @ ${x.hitLocation}) ${x.pain}/${x.wounds}")
    case x: SkirmishMissLog => List(s"${x.protagonist.name} DREW melee with ${x.antagonist.name} (${x.protagonistRoll.natural}+${x.protagonistRoll.modifiers}=${x.protagonistRoll.modified} vs ${x.antagonistRoll.natural}+${x.antagonistRoll.modifiers}=${x.antagonistRoll.modified})")
    case x: MoveLog => List(s"${x.character.name} MOVED from rank ${x.from.index} to rank ${x.to.index}")
    case _ => List()
  }
}
