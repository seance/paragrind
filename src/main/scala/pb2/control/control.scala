package pb2.control

import pb2.domain._
import pb2.context._
import pb2.rules._
import cats.implicits._

case class Npc(
  character: Character,
  control: NpcControl,
)

sealed trait NpcControl {
  def decideAction(self: Npc, battle: Battle, state: BattleState, isProtagonist: Boolean): Action

  def firstNotIncapacitated(npcs: List[Npc], state: BattleState): Option[Npc] = {
    npcs.find(npc => !Rules.isIncapacitated(npc, state.npcStates(npc)))
  }
}

class SkirmisherControl extends NpcControl {
  def decideAction(self: Npc, battle: Battle, state: BattleState, isProtagonist: Boolean): Action = {
    val currentRank = Rules.getNpcRank(self, state)
    val currentRankEnemies = Rules.getEnemiesOnRank(currentRank, battle, state, isProtagonist)
    val currentRankEnemy = firstNotIncapacitated(currentRankEnemies, state)

    lazy val skirmishCurrentRankEnemy = currentRankEnemy.map[Action] { target =>
      if (isProtagonist) SkirmishAction(self, target)
      else SkirmishAction(target, self)
    }

    lazy val moveToNextRank = {
      val nextRankOption = Rules.getNextRank(currentRank, battle.battlefield, isProtagonist)
      nextRankOption.map { nextRank =>
        MoveAction(self, currentRank, nextRank, isEager=true)
      }
    }

    lazy val noAction = NoAction(self)

    (
      skirmishCurrentRankEnemy <+>
      moveToNextRank
    ).getOrElse(noAction)
  }
}

class MarksmanControl extends NpcControl {
  def decideAction(self: Npc, battle: Battle, state: BattleState, isProtagonist: Boolean): Action = {
    val currentRank = Rules.getNpcRank(self, state)
    val nextRankOption = Rules.getNextRank(currentRank, battle.battlefield, isProtagonist)

    lazy val shootNextRankEnemy = nextRankOption.flatMap[Action] { nextRank =>
      val nextRankEnemies = Rules.getEnemiesOnRank(nextRank, battle, state, isProtagonist)
      val nextRankEnemy = firstNotIncapacitated(nextRankEnemies, state)
      nextRankEnemy.map(target => RangedAction(self, target, isMarksman=true))
    }

    lazy val evadeCurrentRankEnemy = {
      val currentRankEnemies = Rules.getEnemiesOnRank(currentRank, battle, state, isProtagonist)
      val currentRankEnemy = firstNotIncapacitated(currentRankEnemies, state)
      currentRankEnemy.map(_ => EvadeAction(self))
    }

    lazy val moveToNextRank = nextRankOption.map { nextRank =>
      MoveAction(self, currentRank, nextRank, isEager=false)
    }

    lazy val skirmishCurrentRankEnemy = {
      val meleeHigherThanEvasion = self.character.skills.melee >= self.character.skills.evasion
      val currentRankEnemies = Rules.getEnemiesOnRank(currentRank, battle, state, isProtagonist)
      val currentRankEnemy = firstNotIncapacitated(currentRankEnemies, state)

      meleeHigherThanEvasion.guard[Option] *> currentRankEnemy.map { target =>
        if (isProtagonist) SkirmishAction(self, target)
        else SkirmishAction(target, self)
      }
    }

    lazy val evadeSkirmish = EvadeAction(self)

    (
      shootNextRankEnemy <+>
      evadeCurrentRankEnemy <+>
      moveToNextRank <+>
      skirmishCurrentRankEnemy
    ).getOrElse(evadeSkirmish)
  }
}

class ThrowerControl extends NpcControl {
  def decideAction(self: Npc, battle: Battle, state: BattleState, isProtagonist: Boolean): Action = {
    val currentRank = Rules.getNpcRank(self, state)
    val currentRankEnemies = Rules.getEnemiesOnRank(currentRank, battle, state, isProtagonist)
    val currentRankEnemy = firstNotIncapacitated(currentRankEnemies, state)
    val nextRankOption = Rules.getNextRank(currentRank, battle.battlefield, isProtagonist)

    lazy val throwCurrentRankEnemy = currentRankEnemy.map[Action] { target =>
      RangedAction(self, target, isMarksman=false)
    }

    lazy val moveToNextRank = nextRankOption.map { nextRank =>
      MoveAction(self, currentRank, nextRank, isEager=false)
    }

    lazy val skirmishCurrentRankEnemy = {
      val meleeHigherThanEvasion = self.character.skills.melee >= self.character.skills.evasion
      meleeHigherThanEvasion.guard[Option] *> currentRankEnemy.map { target =>
        if (isProtagonist) SkirmishAction(self, target)
        else SkirmishAction(target, self)
      }
    }

    lazy val evadeSkirmish = EvadeAction(self)

    (
      throwCurrentRankEnemy <+>
      moveToNextRank <+>
      skirmishCurrentRankEnemy
    ).getOrElse(evadeSkirmish)
  }
}

class CasterControl extends NpcControl {
  def decideAction(self: Npc, battle: Battle, state: BattleState, isProtagonist: Boolean): Action =
    NoAction(self)
}

sealed trait Action
case class SkirmishAction(protagonist: Npc, antagonist: Npc) extends Action
case class RangedAction(source: Npc, target: Npc, isMarksman: Boolean) extends Action
case class CastAction(source: Npc, cast: Cast) extends Action
case class MoveAction(npc: Npc, from: Rank, to: Rank, isEager: Boolean) extends Action
case class EvadeAction(npc: Npc) extends Action
case class NoAction(npc: Npc) extends Action

case class Skirmish(
  protagonists: Set[Npc],
  antagonists: Set[Npc],
)
