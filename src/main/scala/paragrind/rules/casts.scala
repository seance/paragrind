package paragrind.rules

import paragrind.domain._
import paragrind.context._
import paragrind.control._
import cats.implicits._

object Casts {

  def targetCast(
    cast: Cast,
    self: Npc,
    battle: Battle,
    state: BattleState,
    isProtagonist: Boolean,
  ): Option[CastAction] = {
    getCastResolver(cast).target(cast, self, battle, state, isProtagonist)
  }

  def resolveEffect(action: CastAction, battle: Battle, state: BattleState): PhaseResolution = {
    getCastResolver(action.cast).resolve(action, battle, state)
  }

  def getCastResolver(cast: Cast): CastResolver = cast.effect match {
    case CrucibleOfPain => crucibleOfPain
  }

  // TODO dedupe
  def firstNotIncapacitated(npcs: List[Npc], state: BattleState): Option[Npc] = {
    npcs.find(npc => !Rules.isIncapacitated(npc, state.npcStates(npc)))
  }

  val crucibleOfPain: CastResolver = {
    val fortitudeDebuff = CharacterTransform.instance(name="Crucible of Pain", c => {
      val fortitude = math.max(1, c.stats.fortitude - 3)
      c.copy(stats=c.stats.copy(fortitude=fortitude))
    })
    CastResolver.instance(
      (cast, self, battle, state, isProtagonist) => {
        val enemies = if (isProtagonist) battle.antagonists else battle.protagonists
        val targetOption = firstNotIncapacitated(enemies, state)
        targetOption.map(target => CastAction(self, List(target), cast))
      },
      (action, battle, state) => {
        val newState = NpcState(fatigue=1, transforms=Set(fortitudeDebuff))
        PhaseResolution(action.targets.map(_ -> newState).toMap)
      },
    )
  }
}
