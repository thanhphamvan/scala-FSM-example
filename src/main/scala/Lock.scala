import akka.actor.{Actor, ActorRef, ActorSystem, FSM, Props}

sealed trait LockState

case object Locked extends LockState

case object Opened extends LockState

case class Button(digit: Int)

object Lock {

  import scala.concurrent.ExecutionContext.Implicits.global

  import scala.concurrent.duration._

  val TIMEOUT: FiniteDuration = 5.seconds

  val system = ActorSystem("Lock-system")

  def start(code: Seq[Int]) =
    system.actorOf(Props(new Lock(code)))

  def stop(ref: ActorRef): Unit = {
    system.stop(ref)
  }

  def shutdown() = {
    system.terminate().map(
      _ => println("I dunno but maybe the ActorSystem has been terminated!")
    )
  }

  def button(ref : ActorRef, digit : Int) {
    ref ! Button(digit)
  }
}

class Lock(code: Seq[Int]) extends Actor with FSM[LockState, Seq[Int]] {

  import Lock._
  import FSM._

  startWith(Locked, Nil)
  log.info("Code: " + code)

  when(Locked) {
    case Event(Button(digit), soFar) =>
      val _soFar = soFar :+ digit
      log.info("So far: " + _soFar)

      if (_soFar == code) {
        goto(Opened) using Nil forMax TIMEOUT
      } else {
        if (_soFar.length < code.length) {
          stay using _soFar forMax TIMEOUT
        } else {
          log.info("Wrong code: " + _soFar)
          stay using Nil
        }
      }

    case Event(StateTimeout, _) =>
      log.info("Reset")
      stay using Nil
  }

  when(Opened) {
    case Event(_, _) => goto(Locked) using Nil
  }

  onTransition {
    case Locked -> Opened =>
      log.info("The lock is opened!")
      //println("State changed! The lock is opened!")
    case Opened -> Locked =>
      log.info("The lock is locked!")
      //println("State changed! The lock is locked!")
  }

  override def preStart() = {
    log.info("This lock has been started " + self)
  }

  override def postStop() = {
    log.info("This lock has been stopped" + self)
  }

  initialize
}