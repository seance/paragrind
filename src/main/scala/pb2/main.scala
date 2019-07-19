package pb2

import pb2.domain._
import pb2.context._
import pb2.control._
import pb2.data._

object Main extends App {
  import Armory.meleeWeapons._
  import Armory.throwingWeapons._
  import Armory.marksmanWeapons._
  import Armory.shields._
  import Armory.armors._
  import NpcControls._

  val field = Battlefields.ranks(count=4)
  val dfak = Character("Dfak", Stats(14, 8, 5), Skills(melee=9), Gear(broadsword, shield=Some(kite), armor=Some(hardenedLeather)))
  val dimathor = Character("Dimathor", Stats(11, 6, 7), Skills(throwing=8, evasion=6), Gear(broadsword, Some(hatchet), shield=Some(heater), armor=Some(hardenedLeather)))
  val goblin = Character("Goblin", Stats(), Skills(marksman=4), Gear(dagger, weaponMarksman=Some(shortbow), armor=Some(hardenedLeather)))
  val hobgob = Character("Hobgoblin", Stats(), Skills(melee=5), Gear(broadsword, armor=Some(hardenedLeather)))
  val orc = Character("Orc", Stats(), Skills(melee=2), Gear(greataxe, armor=Some(hardenedLeather)))

  val battle = Battle(
    field,
    List(
      Npc(dfak, skirmisher),
      Npc(dimathor, thrower),
    ),
    List(
      Npc(goblin, marksman),
      Npc(hobgob, skirmisher),
      Npc(orc, skirmisher),
    )
  )

  Simulation.runBattles(battle, count=1)
}
