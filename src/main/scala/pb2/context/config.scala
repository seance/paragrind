package pb2.context

import pb2.domain._
import pb2.control._

case class Battle(
  battlefield: Battlefield,
  protagonists: List[Npc],
  antagonists: List[Npc],
)
