package controllers

import akka.actor.ActorRef
import base.{AbstractFSMListener, FSMEvent}
import fsm.{Door, DoorStateChangeEvent}

class DoorController {

  val doorListener = new AbstractFSMListener[Door] {
    override def actionPerformed(fSMEvent: FSMEvent[Door]) = {
        //code here
      fSMEvent match {
        case DoorStateChangeEvent(_, firstState, secondState) =>
          println("State change from " + firstState + " to " + secondState)
      }
    }
  }

  val doorActor: ActorRef = Door.start(1000, this.doorListener)

  Door.pull(doorActor)

}

