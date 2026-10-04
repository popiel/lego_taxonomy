package com.wolfskeep

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import com.wolfskeep.rebrickable.{Data, RebrickableHolder}

object TestStubs {

  def stubRebrickable(data: Data): Behavior[RebrickableHolder.Command] =
    Behaviors.receiveMessage[RebrickableHolder.Command] {
      case RebrickableHolder.GetData(replyTo) =>
        replyTo ! data
        Behaviors.same
      case _ =>
        Behaviors.same
    }

  def fakeProcessor(legoPartFor: ColoredPart => Option[LegoPart]): Behavior[PartsProcessor.Command] =
    Behaviors.receiveMessage[PartsProcessor.Command] {
      case PartsProcessor.ProcessParts(coloredParts, replyTo) =>
        replyTo ! PartsProcessor.ProcessedParts(coloredParts.map(cp => MatchedPart(cp, legoPartFor(cp))))
        Behaviors.same
    }

  def fakeImageResolver(
    ldrawAnswer: ImageResolver.LdrawImageResponse,
    bricksetAnswer: ImageResolver.BricksetImageResponse
  ): Behavior[ImageResolver.Command] =
    Behaviors.receiveMessage[ImageResolver.Command] {
      case ImageResolver.GetLdrawImage(_, _, replyTo) =>
        replyTo ! ldrawAnswer
        Behaviors.same
      case ImageResolver.GetBricksetImageUrl(_, _, replyTo) =>
        replyTo ! bricksetAnswer
        Behaviors.same
    }
}
