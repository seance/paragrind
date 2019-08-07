package paragrind

import paragrind.context._
import paragrind.control._
import paragrind.rules._
import cats._
import cats.implicits._

class BattleResolver[F[_]](
  rules: Rules[F],
)(
  implicit
  F: Monad[F],
  F1: BattleConfig[F],
  F2: BattleStateful[F],
  F3: BattleLogging[F],
) {
  import F1._
  import F2._
  import F3._

  def resolve(maxRounds: Int): F[Unit] = {
    F.untilM_ {
      resolveRound *>
      incrementRound
    }(isBattleFinished(maxRounds))
  }

  def isBattleFinished(maxRounds: Int): F[Boolean] = {
    for {
      battle <- ask
      state <- get
    } yield (
      battle.protagonists.forall(npc => Rules.isIncapacitated(npc, state.npcStates(npc))) ||
      battle.antagonists.forall(npc => Rules.isIncapacitated(npc, state.npcStates(npc))) ||
      state.round >= maxRounds
    )
  }

  def incrementRound(): F[Unit] = {
    modify(s => s.copy(round = s.round + 1))
  }

  def resolveRound(): F[Unit] = {
    for {
      round <- get.map(_.round)
      actions <- decideActions()
      phases = actionsToPhases(actions)
      _ <- tell(List(SkirmishesLog(round, phases.skirmishes)))
      _ <- resolvePhases(phases)
      _ <- resolveEndOfRound()
    } yield ()
  }

  def decideActions(): F[List[Action]] = {
    for {
      battle <- ask
      protagonistActions <- battle.protagonists.traverse(decideAction(_, isProtagonist=true))
      antagonistActions <- battle.antagonists.traverse(decideAction(_, isProtagonist=false))
    } yield protagonistActions ++ antagonistActions
  }

  def decideAction(npc: Npc, isProtagonist: Boolean): F[Action] = {
    for {
      battle <- ask
      battleState <- get
      round = battleState.round
      npcState = battleState.npcStates(npc)
      action =
        if (Rules.isIncapacitated(npc, npcState)) NoAction(npc)
        else npc.control.decideAction(npc, battle, battleState, isProtagonist)
      _ <- tell(List(ActionLog(round, action)))
      newState = npcState.copy(action = action.some)
      _ <- set(battleState.copy(npcStates = battleState.npcStates.updated(npc, newState)))
    } yield action
  }

  def actionsToPhases(actions: List[Action]): BattlePhases = {
    actions.foldLeft(BattlePhases()) { (phases, action) =>
      action match {
        case a: SkirmishAction => phases.copy(
          skirmishes = mergeSkirmish(phases.skirmishes, a)
        )
        case a: RangedAction => phases.copy(ranged = phases.ranged :+ a)
        case a: CastAction => phases.copy(casts = phases.casts :+ a)
        case a: MoveAction => phases.copy(moves = mergeMove(phases.moves, a))
        case _: EvadeAction => phases
        case _: NoAction => phases
      }
    }
  }

  def resolvePhases(phases: BattlePhases): F[Unit] = {
    for {
      rr <- presolvePhases[RangedAction](phases.ranged, presolveRanged)
      _  <- applyPhaseResolution(rr)
      sr <- presolvePhases[Skirmish](phases.skirmishes, presolveSkirmish)
      _  <- applyPhaseResolution(sr)
      cr <- presolvePhases[CastAction](phases.casts, presolveCast)
      _  <- applyPhaseResolution(cr)
      _  <- resolveMoves(phases.moves)
    } yield ()
  }

  def presolvePhases[A](actionables: List[A], presolver: A => F[PhaseResolution]): F[PhaseResolution] = {
    actionables.traverse(presolver).map(_.foldLeft(PhaseResolution())(mergePhaseResolution))
  }

  def cancelIfIncapacitated(npc: Npc, actionable: => F[PhaseResolution]): F[PhaseResolution] = {
    for {
      npcStates <- get.map(_.npcStates)
      noActionable = PhaseResolution().pure[F]
      result <-
        if (Rules.isIncapacitated(npc, npcStates(npc))) noActionable
        else actionable
    } yield result
  }

  def presolveRanged(ranged: RangedAction): F[PhaseResolution] = {
    cancelIfIncapacitated(ranged.source, for {
      round <- get.map(_.round)
      npcStates <- get.map(_.npcStates)
      sourceState = npcStates(ranged.source)
      targetState = npcStates(ranged.target)
      targetDelta <- rules.resolveRanged(round, ranged, sourceState, targetState)
    } yield PhaseResolution(Map(ranged.target -> targetDelta)))
  }

  def presolveSkirmish(skirmish: Skirmish): F[PhaseResolution] = {
    get >>= { battleState =>
      val round = battleState.round
      val npcStates = battleState.npcStates
      val activePs = skirmish.protagonists.filter(n => !Rules.isIncapacitated(n, npcStates(n)))
      val activeAs = skirmish.antagonists.filter(n => !Rules.isIncapacitated(n, npcStates(n)))

      if (activePs.isEmpty || activeAs.isEmpty) PhaseResolution().pure[F]
      else rules.resolveSkirmish(round, Skirmish(activePs, activeAs), npcStates)
    }
  }

  def presolveCast(cast: CastAction): F[PhaseResolution] = {
    cancelIfIncapacitated(cast.source, for {
      battle <- ask
      state <- get
      resolution <- rules.resolveCast(cast, battle, state)
    } yield resolution)
  }

  def resolveMoves(moves: List[MoveAction]): F[Unit] = {
    moves.traverse_(resolveMove)
  }

  def resolveMove(move: MoveAction): F[Unit] = {
    for {
      battle <- ask
      state <- get
      isProtagonist = battle.protagonists.contains(move.npc)
      frontRank = Rules.getFrontRank(battle, state, isProtagonist)
      isBeyondFront = Rules.isBeyondFront(move.to, frontRank, battle, isProtagonist)
      _ <- if (isBeyondFront) ().pure[F] else {
        set(state.copy(positions = state.positions.updated(move.npc, move.to))) *>
        tell(List(MoveLog(state.round, move.npc.character, move.from, move.to)))
      }
    } yield ()
  }

  def resolveEndOfRound(): F[Unit] = {
    resolveBleeding()
  }

  def resolveBleeding(): F[Unit] = {
    for {
      battleState <- get
      newStates = battleState.npcStates.view.mapValues {
        case s if Rules.isBleeding(s) => applyBleeding(s)
        case s => s
      }
      _ <- set(battleState.copy(npcStates = newStates.toMap))
    } yield ()
  }

  def mergeSkirmish(skirmishes: List[Skirmish], skirmish: SkirmishAction): List[Skirmish] = {
    val (protagonist, antagonist) =
      if (skirmish.isProtagonist) (skirmish.source, skirmish.target)
      else (skirmish.target, skirmish.source)

    val hasProtagonist = (s: Skirmish) => s.protagonists.contains(protagonist)
    val hasAntagonist = (s: Skirmish) => s.antagonists.contains(antagonist)

    lazy val joinSkirmishes = (for {
      s1 <- skirmishes if (hasProtagonist(s1) || hasAntagonist(s1))
      s2 <- skirmishes if (hasProtagonist(s2) || hasAntagonist(s2)) && (s1 != s2)
    } yield skirmishes.diff(List(s1, s2)) :+ Skirmish(
      s1.protagonists ++ s2.protagonists + protagonist,
      s2.antagonists ++ s2.antagonists + antagonist,
    )).headOption

    lazy val mergeWithAntagonists = for {
      s <- skirmishes.find(hasProtagonist)
    } yield skirmishes.diff(List(s)) :+ Skirmish(
      s.protagonists,
      s.antagonists + antagonist,
    )

    lazy val mergeWithProtagonists = for {
      s <- skirmishes.find(hasAntagonist)
    } yield skirmishes.diff(List(s)) :+ Skirmish(
      s.protagonists + protagonist,
      s.antagonists
    )

    lazy val appendNewSkirmish = skirmishes :+ Skirmish(
      Set(protagonist),
      Set(antagonist),
    )

    (
      joinSkirmishes <+>
      mergeWithProtagonists <+>
      mergeWithAntagonists
    ).getOrElse(appendNewSkirmish)
  }

  def mergeMove(moves: List[MoveAction], move: MoveAction): List[MoveAction] = {
    if (move.isEager) move +: moves else moves :+ move
  }

  def mergePhaseResolution(a: PhaseResolution, b: PhaseResolution): PhaseResolution = {
    PhaseResolution((a.deltas.toList ++ b.deltas.toList).groupBy(_._1).view.mapValues { xs =>
      xs.map(_._2).reduceLeft(mergeNpcState)
    }.toMap)
  }

  def mergeNpcState(a: NpcState, b: NpcState): NpcState = {
    NpcState(
      b.fatigue + a.fatigue,
      b.wounds ++ a.wounds,
      b.action <+> a.action,
      b.transforms ++ a.transforms,
    )
  }

  def applyPhaseResolution(pr: PhaseResolution): F[Unit] = {
    for {
      state <- get
      merged = mergePhaseResolution(PhaseResolution(state.npcStates), pr).deltas
      _ <- set(state.copy(npcStates = merged))
    } yield ()
  }

  def applyBleeding(state: NpcState): NpcState = {
    state.copy(fatigue = state.fatigue + 1)
  }
}
