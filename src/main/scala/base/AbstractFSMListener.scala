package base

import fsm.MyFSM

abstract class AbstractFSMListener[T <: MyFSM] {
  def actionPerformed(fSMEvent: FSMEvent[T])
}
