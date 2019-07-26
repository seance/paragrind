package paragrind

import paragrind.domain._
import paragrind.context._
import paragrind.control._
import paragrind.data._

object Main extends App {
  import Armory.meleeWeapons._
  import Armory.throwingWeapons._
  import Armory.marksmanWeapons._
  import Armory.shields._
  import Armory.armors._
  import NpcControls._

  implicit def anyToOption[A](x: A): Option[A] = Some(x)

  val battlefield = Battlefields.ranks(count=4)

  val warrior = Character("Warrior", Stats(14, 8, 5), Skills(melee=9), Gear(broadsword, shield=kite, armor=hardenedLeather))
  val paladin = Character("Paladin", Stats(14, 8, 6), Skills(melee=7), Gear(greatAxe, shield=heater, armor=hardenedLeather))
  val rogue = Character("Rogue", Stats(11, 6, 7), Skills(throwing=8, evasion=6), Gear(broadsword, hatchet, shield=heater, armor=hardenedLeather))
  val goblin = Character("Goblin", Stats(), Skills(marksman=4, evasion=4), Gear(dagger, weaponMarksman=shortbow, armor=hardenedLeather))
  val hobgoblin = Character("Hobgoblin", Stats(), Skills(melee=5), Gear(broadsword, armor=hardenedLeather))
  val orc = Character("Orc", Stats(), Skills(melee=2), Gear(greatAxe, armor=hardenedLeather))
  val titan = Character("Titan", Stats(15, 10, 10), Skills(melee=10, evasion=8), Gear(heavyMaul, armor=plateMail))

  val battle = Battle(
    battlefield,
    List(
      Npc(warrior, skirmisher),
      Npc(paladin, skirmisher),
      Npc(rogue, thrower),
    ),
    List(
      //Npc(goblin, marksman),
      //Npc(orc, skirmisher),
      //Npc(hobgoblin, skirmisher),
      Npc(titan, skirmisher),
    )
  )

  val result = Simulation.simulate(battle, count=10)

  println(Transcriber.transcribeSimulationResult(result))
}
