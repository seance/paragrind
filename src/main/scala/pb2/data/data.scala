package pb2.data

import pb2.domain._
import pb2.control._

object Battlefields {
  def ranks(count: Int): Battlefield = Battlefield(
    (0 until count).map(Rank).toList
  )
}

object Armory {
  val meleeWeapons = new {
    val dagger = Weapon("Dagger", 4)
    val broadsword = Weapon("Broadsword", 7)
    val greatAxe = Weapon("Great axe", 10)
    val heavyMaul = Weapon("Heavy maul", 12)
  }
  val throwingWeapons = new {
    val hatchet = Weapon("Hatchet", 7)
    val javelin = Weapon("Javelin", 9)
  }
  val marksmanWeapons = new {
    val shortbow = Weapon("Shortbow", 7)
    val longbow = Weapon("Longbow", 10)
  }
  val shields = new {
    val buckler = Shield("Buckler", 1)
    val heater = Shield("Heater", 2)
    val kite = Shield("Kite", 3)
  }
  val armors = new {
    val hardenedLeather = Armor("Hardened leather", 2)
    val chainMail = Armor("Chain mail", 5)
    val plateMail = Armor("Plate mail", 9)
  }
}

object NpcControls {
  val skirmisher = new SkirmisherControl()
  val marksman = new MarksmanControl()
  val thrower = new ThrowerControl()
  val caster = new CasterControl()
}
