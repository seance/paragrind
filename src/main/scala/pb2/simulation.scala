package pb2

import pb2.domain._
import pb2.context._
import pb2.control._
import pb2.rules._
import cats.implicits._

object Simulation {

  def runBattles(battle: Battle, count: Int): Unit = {
    val victories = (0 until count).collect {
      case _ if runBattle(battle) => true
    }.length

    println(s"$victories/$count victories")
  }

  def runBattle(battle: Battle): Boolean = {
    import cats.data._
    import cats.mtl.implicits._
    import execution._

    type Stack[A] = RWS[Battle, List[BattleLog], BattleState, A]

    val positions = Map(
      battle.protagonists.map(_ -> battle.battlefield.ranks.head) ++
      battle.antagonists.map(_ -> battle.battlefield.ranks.last): _*
    )
    val npcStates = Map(
      (battle.protagonists ++ battle.antagonists).map(_ -> NpcState()): _*
    )
    val initialState = BattleState(0, positions, npcStates)
    val (logs, finalState, _) = resolveBattle[Stack]().run(battle, initialState).value

    val isVictory = battle.antagonists.forall(npc => Rules.isIncapacitated(npc, finalState.npcStates(npc)))

    logs.groupBy(_.round).toList
      .sortBy{ case (round, _) => round }
      .foreach { case (round, roundLogs) =>
        val actionLogs = roundLogs.collect { case a: ActionLog => a }
        val resolveLogs = roundLogs.filter { case a: ActionLog => false ; case _ => true }
        println(s"\n-- Round $round --")
        println("Actions:")
        actionLogs.foreach(printLog(battle))
        println("Resolutions:")
        resolveLogs.foreach(printLog(battle))
    }

    isVictory
  }

  def printLog(battle: Battle): BattleLog => Unit = {
    case ActionLog(_, RangedAction(source, target, isMarksman)) => println(s"${source.character.name} ${if (isMarksman) "SHOOT" else "THROW" } ${target.character.name}")
    case ActionLog(_, SkirmishAction(protagonist, antagonist)) => println(s"${protagonist.character.name} SKIRMISH ${antagonist.character.name}")
    case ActionLog(_, MoveAction(npc, from, to, _)) => println(s"${npc.character.name} MOVE ranks ${battle.battlefield.ranks.indexOf(from)} -> ${battle.battlefield.ranks.indexOf(to)}")
    case ActionLog(_, EvadeAction(npc)) => println(s"${npc.character.name} EVADES")
    case ActionLog(_, NoAction(npc)) => println(s"${npc.character.name} NO ACTION")
    case x: RangedHitLog => println(s"${x.attacker.name} HIT ranged ${x.defender.name}: ${x.pain}/${x.wounds}")
    case x: RangedMissLog => println(s"${x.attacker.name} MISS ranged ${x.defender.name}")
    case x: SkirmishHitLog => println(s"${x.attacker.name} HIT melee ${x.defender.name}: ${x.pain}/${x.wounds}")
    case x: SkirmishMissLog => println(s"${x.protagonist.name} DRAW melee with ${x.antagonist.name}")
    case x: MoveLog => println(s"${x.character.name} MOVE from rank ${x.from.index} to rank ${x.to.index}")
    case _ => ()
  }

  def resolveBattle[F[_]]()(implicit F: MonadBattle[F]): F[Unit] = {
    F.untilM_ {
      resolveRound *>
      incrementRound
    }(isBattleFinished)
  }

  def isBattleFinished[F[_]](implicit F: MonadBattle[F]): F[Boolean] = {
    for {
      battle <- F.ask
      state <- F.get
    } yield (
      battle.protagonists.forall(npc => Rules.isIncapacitated(npc, state.npcStates(npc))) ||
      battle.antagonists.forall(npc => Rules.isIncapacitated(npc, state.npcStates(npc))) ||
      state.round >= 10
    )
  }

  def incrementRound[F[_]](implicit F: MonadBattle[F]): F[Unit] = {
    F.modify(s => s.copy(round = s.round + 1))
  }

  def resolveRound[F[_]](implicit F: MonadBattle[F]): F[Unit] = {
    for {
      actions <- decideActions
      phases = actionsToPhases(actions)
      _ <- resolvePhases(phases)
      _ <- resolveEndOfRound
    } yield ()
  }

  def decideActions[F[_]](implicit F: MonadBattle[F]): F[List[Action]] = {
    for {
      battle <- F.ask
      protagonistActions <- battle.protagonists.traverse(decideAction(_, isProtagonist=true))
      antagonistActions <- battle.antagonists.traverse(decideAction(_, isProtagonist=false))
    } yield protagonistActions ++ antagonistActions
  }

  def decideAction[F[_]](npc: Npc, isProtagonist: Boolean)(implicit F: MonadBattle[F]): F[Action] = {
    for {
      battle <- F.ask
      battleState <- F.get
      round = battleState.round
      npcState = battleState.npcStates(npc)
      action =
        if (Rules.isIncapacitated(npc, npcState)) NoAction(npc)
        else npc.control.decideAction(npc, battle, battleState, isProtagonist)
      _ <- F.tell(List(ActionLog(round, action)))
      newState = npcState.copy(action = action.some)
      _ <- F.set(battleState.copy(npcStates = battleState.npcStates.updated(npc, newState)))
    } yield action
  }

  def actionsToPhases(actions: List[Action]): BattlePhases = {
    actions.foldLeft(BattlePhases()) { (phases, action) =>
      action match {
        case SkirmishAction(protagonist, antagonist) => phases.copy(
          skirmishes = mergeSkirmish(phases.skirmishes, (protagonist, antagonist))
        )
        case a: RangedAction => phases.copy(ranged = phases.ranged :+ a)
        case a: CastAction => phases.copy(casts = phases.casts :+ a)
        case a: MoveAction => phases.copy(moves = mergeMove(phases.moves, a))
        case a: EvadeAction => phases
        case a: NoAction => phases
      }
    }
  }

  def resolvePhases[F[_]](phases: BattlePhases)(implicit F: MonadBattle[F]): F[Unit] = {
    for {
      rr <- presolvePhases[RangedAction, F](phases.ranged, presolveRanged(_))
      _  <- applyPhaseResolution(rr)
      sr <- presolvePhases[Skirmish, F](phases.skirmishes, presolveSkirmish(_))
      _  <- applyPhaseResolution(sr)
      cr <- presolvePhases[CastAction, F](phases.casts, presolveCast(_))
      _  <- applyPhaseResolution(cr)
      _  <- resolveMoves(phases.moves)
    } yield ()
  }

  def presolvePhases[A, F[_]](
    actionables: List[A],
    presolver: A => F[PhaseResolution],
  )(
    implicit
    F: MonadBattle[F],
  ): F[PhaseResolution] = {
    actionables.traverse(presolver).map(_.foldLeft(PhaseResolution())(mergePhaseResolution))
  }

  def cancelIfIncapacitated[F[_]](
    npc: Npc,
    actionable: => F[PhaseResolution],
  )(
    implicit
    F: MonadBattle[F],
  ): F[PhaseResolution] = {
    for {
      npcStates <- F.get.map(_.npcStates)
      noActionable = PhaseResolution().pure[F]
      result <-
        if (Rules.isIncapacitated(npc, npcStates(npc))) noActionable
        else actionable
    } yield result
  }

  def presolveRanged[F[_]](ranged: RangedAction)(implicit F: MonadBattle[F]): F[PhaseResolution] = {
    cancelIfIncapacitated(ranged.source, for {
      round <- F.get.map(_.round)
      npcStates <- F.get.map(_.npcStates)
      sourceState = npcStates(ranged.source)
      targetState = npcStates(ranged.target)
      targetDelta <- Rules.resolveRanged[F](round, ranged, sourceState, targetState)
    } yield PhaseResolution(Map(ranged.target -> targetDelta)))
  }

  def presolveSkirmish[F[_]](skirmish: Skirmish)(implicit F: MonadBattle[F]): F[PhaseResolution] = {
    F.get >>= { battleState =>
      val round = battleState.round
      val npcStates = battleState.npcStates
      val activePs = skirmish.protagonists.filter(n => !Rules.isIncapacitated(n, npcStates(n)))
      val activeAs = skirmish.antagonists.filter(n => !Rules.isIncapacitated(n, npcStates(n)))

      if (activePs.isEmpty || activeAs.isEmpty) PhaseResolution().pure[F]
      else Rules.resolveSkirmish[F](round, Skirmish(activePs, activeAs), npcStates)
    }
  }

  def presolveCast[F[_]](cast: CastAction)(implicit F: MonadBattle[F]): F[PhaseResolution] = {
    cancelIfIncapacitated(cast.source, ???)
  }

  def resolveMoves[F[_]](moves: List[MoveAction])(implicit F: MonadBattle[F]): F[Unit] = {
    moves.traverse_(resolveMove(_))
  }

  def resolveMove[F[_]](move: MoveAction)(implicit F: MonadBattle[F]): F[Unit] = {
    for {
      battle <- F.ask
      state <- F.get
      isProtagonist = battle.protagonists.contains(move.npc)
      frontRank = Rules.getFrontRank(battle, state, isProtagonist)
      isBeyondFront = Rules.isBeyondFront(move.to, frontRank, battle, isProtagonist)
      _ <- if (isBeyondFront) ().pure[F] else {
        F.set(state.copy(positions = state.positions.updated(move.npc, move.to))) *>
        F.tell(List(MoveLog(state.round, move.npc.character, move.from, move.to)))
      }
    } yield ()
  }

  def resolveEndOfRound[F[_]]()(implicit F: MonadBattle[F]): F[Unit] = {
    resolveBleeding[F]: F[Unit]
  }

  def resolveBleeding[F[_]]()(implicit F: MonadBattle[F]): F[Unit] = {
    for {
      battleState <- F.get
      newStates = battleState.npcStates.view.mapValues {
        case s if Rules.isBleeding(s) => applyBleeding(s)
        case s => s
      }
      _ <- F.set(battleState.copy(npcStates = newStates.toMap))
    } yield ()
  }

  def mergeSkirmish(skirmishes: List[Skirmish], skirmish: (Npc, Npc)): List[Skirmish] = {
    val protagonist = skirmish._1
    val antagonist = skirmish._2
    val mergeToExisting =
      skirmishes
        .find(_.protagonists.contains(protagonist))
        .map(s => skirmishes.updated(skirmishes.indexOf(s), s.copy(antagonists = s.antagonists + antagonist))) <+>
      skirmishes
        .find(_.antagonists.contains(antagonist))
        .map(s => skirmishes.updated(skirmishes.indexOf(s), s.copy(protagonists = s.protagonists + protagonist)))

    mergeToExisting.getOrElse(skirmishes :+ Skirmish(Set(protagonist), Set(antagonist)))
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
    NpcState(a.fatigue + b.fatigue, a.wounds ++ b.wounds)
  }

  def applyPhaseResolution[F[_]](pr: PhaseResolution)(implicit F: MonadBattle[F]): F[Unit] = {
    for {
      state <- F.get
      merged = mergePhaseResolution(PhaseResolution(state.npcStates), pr).deltas
      _ <- F.set(state.copy(npcStates = merged))
    } yield ()
  }

  def applyBleeding(state: NpcState): NpcState = {
    state.copy(fatigue = state.fatigue + 1)
  }
}
