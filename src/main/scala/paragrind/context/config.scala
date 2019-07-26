package paragrind.context

import paragrind.domain._
import paragrind.control._

case class Battle(
  battlefield: Battlefield,
  protagonists: List[Npc],
  antagonists: List[Npc],
)
