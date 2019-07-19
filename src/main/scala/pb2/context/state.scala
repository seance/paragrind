package pb2.context

import pb2.domain._
import pb2.control._

case class BattleState(
  round: Int,
  positions: Map[Npc, Rank],
  npcStates: Map[Npc, NpcState],
)

case class BattlePhases(
 ranged: List[RangedAction] = List.empty,
 skirmishes: List[Skirmish] = List.empty,
 casts: List[CastAction] = List.empty,
 moves: List[MoveAction] = List.empty,
)

case class NpcState(
 fatigue: Int = 0,
 wounds: Set[Wound] = Set.empty,
 action: Option[Action] = None,
)

case class PhaseResolution(
  deltas: Map[Npc, NpcState] = Map.empty,
)
