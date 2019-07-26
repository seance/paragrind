package paragrind

import paragrind.domain._
import paragrind.context._
import paragrind.rules._
import cats._
import cats.implicits._
import scala.collection.parallel.CollectionConverters._

object Simulation {

  def simulate(battle: Battle, count: Int): SimulationResult = {

    implicit def boolToInt(bool: Boolean): Int = if (bool) 1 else 0

    val battleThunks = List.fill(count)(Eval.later(runBattle(battle)))
    val battleResults = battleThunks.par.map(_.value)
    val battleNpcs = battle.protagonists ++ battle.antagonists
    val initialResult = SimulationResult(characterResults = battleNpcs.map(npc =>
      npc.character -> SimulationCharacterResult(),
    ).toMap)

    val simulationResult = battleResults.foldLeft(initialResult) { case (sr, br) =>
      SimulationResult(
        count,
        sr.meanRoundsTaken + br.roundsTaken,
        sr.totalPartyKills + br.isTotalPartyKill,
        sr.characterResults.map { case (character, scr) =>
          val bcr = br.characterResults(character)
          character -> SimulationCharacterResult(
            scr.totalFatigue + bcr.fatigue,
            scr.totalCriticals + bcr.isCriticals,
            scr.totalSuperCriticals + bcr.isSuperCriticals,
            scr.totalIncapacitations + bcr.isIncapacitated,
            scr.totalDying + bcr.isDying,
          )
        },
      )
    }

    simulationResult.copy(meanRoundsTaken = simulationResult.meanRoundsTaken / count)
  }

  def runBattle(battle: Battle): BattleResult = {
    import cats.data._
    import cats.mtl.implicits._

    type Stack[A] = RWS[Battle, List[BattleLog], BattleState, A]

    val initialState = setupInitialBattleState(battle)
    val rules = new Rules[Stack]()
    val resolver = new BattleResolver[Stack](rules)

    val result = resolver.resolve(maxRounds=15).run(battle, initialState)
    val (logs, finalState, _) = result.value

    analyzeFinalBattleState(battle, logs, finalState)
  }

  def setupInitialBattleState(battle: Battle): BattleState = {
    val positions = Map(
      battle.protagonists.map(_ -> battle.battlefield.ranks.head) ++
        battle.antagonists.map(_ -> battle.battlefield.ranks.last): _*
    )
    val npcStates = Map(
      (battle.protagonists ++ battle.antagonists).map(_ -> NpcState()): _*
    )
    BattleState(0, positions, npcStates)
  }

  def analyzeFinalBattleState(battle: Battle, logs: List[BattleLog], state: BattleState): BattleResult = {
    val npcStates = state.npcStates
    val isTpk = battle.protagonists.forall(p => Rules.isIncapacitated(p, npcStates(p)))

    BattleResult(state.round, isTpk, logs, npcStates.map { case (npc, npcState) =>
      val characterResult = BattleCharacterResult(
        npcState.fatigue,
        npcState.wounds.nonEmpty,
        npcState.wounds.collect {
          case c: SuperCritical => c
        }.nonEmpty,
        Rules.isIncapacitated(npc, npcState),
        npcState.wounds.collect {
          case s: SuperCritical => s
          case Bleeding => Bleeding
        }.nonEmpty,
      )
      npc.character -> characterResult
    })
  }
}

case class BattleResult(
  roundsTaken: Int,
  isTotalPartyKill: Boolean,
  logs: List[BattleLog],
  characterResults: Map[Character, BattleCharacterResult],
)

case class BattleCharacterResult(
  fatigue: Int,
  isCriticals: Boolean,
  isSuperCriticals: Boolean,
  isIncapacitated: Boolean,
  isDying: Boolean,
)

case class SimulationResult(
  simulationCount: Int = 0,
  meanRoundsTaken: Double = 0,
  totalPartyKills: Int = 0,
  characterResults: Map[Character, SimulationCharacterResult] = Map.empty,
)

case class SimulationCharacterResult(
  totalFatigue: Int = 0,
  totalCriticals: Int = 0,
  totalSuperCriticals: Int = 0,
  totalIncapacitations: Int = 0,
  totalDying: Int = 0,
)
