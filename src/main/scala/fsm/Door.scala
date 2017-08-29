package fsm

import akka.actor.{Actor, ActorRef, ActorSystem, FSM, Props}
import base.{AbstractFSMListener, FSMEvent}

import scala.concurrent.duration.FiniteDuration

sealed trait DoorState

case object Opened extends DoorState

case object Closed extends DoorState

case class Pull()

case class Hold(duration: FiniteDuration)

sealed trait MyFSM

object Door {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val system = ActorSystem("door-system")

  def start(duration: FiniteDuration, abstractFSMListener: AbstractFSMListener[Door]): ActorRef =
    system.actorOf(Props(new Door(duration, abstractFSMListener)))

  def start(duration: Long, abstractFSMListener: AbstractFSMListener[Door]): ActorRef =
    system.actorOf(Props(new Door(duration.milliseconds, abstractFSMListener)))

  def stop(ref: ActorRef): Unit = system.stop(ref)

  def shutdown(): Unit = system.terminate().map(
    _ => println("Door-System is terminated!")
  )

  def pull(ref: ActorRef): Unit = {
    ref ! Pull()
  }

  def hold(ref: ActorRef, duration: FiniteDuration): Unit = {
    ref ! Hold(duration)
  }

  def hold(ref: ActorRef, duration: Long) = {
    ref ! Hold(duration.milliseconds)
  }
}

case class DoorStateChangeEvent(source: Door, state1: DoorState, state2: DoorState) extends FSMEvent[Door](source)

class Door(duration: FiniteDuration, abstractFSMListener: AbstractFSMListener[Door]) extends Actor with FSM[DoorState, Int] with MyFSM {
  startWith(Closed, 0)

  when(Closed) {
    case Event(Pull(), counter) =>
      goto(Opened) using (counter + 1) forMax duration
    case Event(Hold(_), _) =>
      log.info("You should not hold a closed door...")
      stay
  }

  when(Opened) {
    case Event(Hold(_duration), _) => stay forMax _duration
    case Event(StateTimeout, _) => goto(Closed)
  }

  onTransition {
    case Closed -> Opened =>
      log.info("The door is opened!")
      abstractFSMListener.actionPerformed(DoorStateChangeEvent(this, Closed, Opened))
    case Opened -> Closed =>
      log.info("The door is closed!")
      abstractFSMListener.actionPerformed(DoorStateChangeEvent(this, Opened, Closed))

  }
}
