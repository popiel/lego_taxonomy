package com.wolfskeep.rebrickable

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object RebrickableDataActor {
  sealed trait Command
  case class SetColors(colors: List[Color]) extends Command
  case class SetParts(parts: List[Part]) extends Command
  case class SetElements(elements: List[Element]) extends Command
  case class SetSets(sets: List[RebrickableSet]) extends Command
  case class SetInventories(inventories: List[Inventory]) extends Command
  case class SetInventoryParts(inventoryParts: List[InventoryPart]) extends Command
  case class GetData(replyTo: ActorRef[Data]) extends Command

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    val initialData = Data.load()
    context.log.info(
      s"RebrickableData: initialized with {} parts, {} colors, {} elements, {} sets, {} inventories, {} inventoryParts",
      initialData.parts.size,
      initialData.colors.size,
      initialData.elements.size,
      initialData.sets.size,
      initialData.inventories.size,
      initialData.inventoryParts.size
    )
    idle(initialData)
  }

  private def idle(data: Data): Behavior[Command] = Behaviors.receive { (context, message) =>
    message match {
      case SetColors(colors) =>
        context.log.info("RebrickableData: updated {} colors", colors.size)
        idle(data.copy(colors = colors))
      case SetParts(parts) =>
        context.log.info("RebrickableData: updated {} parts", parts.size)
        idle(data.copy(parts = parts))
      case SetElements(elements) =>
        context.log.info("RebrickableData: updated {} elements", elements.size)
        idle(data.copy(elements = elements))
      case SetSets(sets) =>
        context.log.info("RebrickableData: updated {} sets", sets.size)
        idle(data.copy(sets = sets))
      case SetInventories(inventories) =>
        context.log.info("RebrickableData: updated {} inventories", inventories.size)
        idle(data.copy(inventories = inventories))
      case SetInventoryParts(inventoryParts) =>
        context.log.info("RebrickableData: updated {} inventoryParts", inventoryParts.size)
        idle(data.copy(inventoryParts = inventoryParts))
      case GetData(replyTo) =>
        replyTo ! data
        Behaviors.same
    }
  }
}
