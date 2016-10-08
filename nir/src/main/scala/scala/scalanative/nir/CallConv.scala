package scala.scalanative.nir

/**
  * Created by kamil on 08.10.16.
  */
sealed abstract class CallConv

object CallConv {
  final case object ScalaCC extends CallConv
  final case object CCC extends CallConv
}
