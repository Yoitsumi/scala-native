package scala.scalanative.compiler.codegen

import scala.scalanative.nir.Op.Call
import scala.scalanative.nir._
import scala.scalanative.util.Show.{Repeat => r, Sequence => s}
import scala.scalanative.util.{Show, sh}

/**
 * Created by Kamil Tomala on 25.06.2016.
 */
class Amd64ABILowering(codegen: GenTextualLLVM) {

  import codegen.{ showLocal, showVal, showType, showGlobal }

  val fresh = new Fresh("gen.abi")

  def coerceArguments(args: Seq[Type]): Seq[Type] = args flatMap coerceArgument

  def coerceArgument(arg: Type): Seq[Type] = arg match {
    case struct @ Type.Struct(name, tys) =>
      def size(t: Type): Int = t match {
        case Type.Bool => 1
        case Type.I(w) => w / 8
        case Type.F(w) => w / 8
        case Type.Ptr  => 8
        case Type.Array(e, n) => size(e) * n
        case Type.Struct(_, e) => e.map(size).sum
      }
      size(struct) match {
        case s if s <= 8 => // One parameter
          Seq(Type.I(s * 8))
        case s if s <= 16 => // Two parameters
          Seq(Type.I64, Type.I((s - 8) * 8))
        case _ => // Pointer
          Seq(Type.Ptr)
      }
    // TODO: Once we get vector types their lowering should be handled here too
    case x => Seq(x)
  }


//  def coerceCall(op: Op.Call): (Seq[Inst], Op.Call) = {
//    val Op.Call(Type.Function(_, retty), ptr, args) = op
//    // A tuple for each original arg, containing seq of args that should be used to call
//    // (in most cases just the original arg) and a seq of instructions that should be
//    // inserted before the call, to compute this value (in most cases empty)
//    val argChanges: Seq[(Seq[Val], Seq[Inst])] =
//      for (arg <- args) yield {
//        val coerced = coerceArgument(arg.ty)
//        if(coerced.size == 1 && coerced.head == arg.ty) {
//          (Seq(arg), Seq.empty) // No coercing required
//        } else {
//          val newArgs = coerced.map(t => Val.Local(fresh(), t))
//          val coercety = Type.Struct(Global.None, coerced)
//          val bitcastres = Val.Local(fresh(), coercety)
//          val pointer = Val.Local(fresh(), Type.Ptr)
//          val alloc = Inst()
//          val bitcast = Inst(bitcastres.name, Op.Conv(Conv.Bitcast, coercety, arg))
//          val extracts = for((res, i) <- newArgs.zipWithIndex) yield
//            Inst(res.name, Op.Extract(bitcastres, Seq(i)))
//          (newArgs, bitcast +: extracts)
//        }
//      }
//    val coercedArgs = argChanges.flatMap(_._1)
//    ( argChanges.flatMap(_._2) // All instructions to add before the call
//    , Op.Call(Type.Function(coercedArgs.map(_.ty), retty), ptr, coercedArgs)) // The call instruction with modified args
//  }

  def coerceCall(inst: Inst): Seq[Show.Result] = {

    def isVoid(ty: Type): Boolean =
      ty == Type.Void || ty == Type.Unit || ty == Type.Nothing

    val Op.Call(Type.Function(_, retty), ptr, args) = inst.op
    val argChanges: Seq[(Seq[Val], Seq[Show.Result])] =
      for (arg <- args) yield {
        val coerced = coerceArgument(arg.ty)
        if(coerced.size == 1 && coerced.head == arg.ty) {
          // No changes required
          (Seq(arg), Seq.empty)
        } else if(coerced.size == 1 && coerced.head == Type.Ptr) {
          // Struct too big, passed by pointer
          val alloc = fresh() // arg.ty*
          val bitcasted = fresh() // i8*
          (Seq(Val.Local(bitcasted, Type.Ptr)), Seq(
            sh"%$alloc = alloca ${arg.ty}",
            sh"store $arg, ${arg.ty}* %$alloc",
            sh"%$bitcasted = bitcast ${arg.ty}* %$alloc to i8*"
          ))
        } else {
          val newArgs = coerced.map(t => Val.Local(fresh(), t))
          val coercety = Type.Struct(Global.None, coerced)
          val allocatedStruct = fresh() // coercety*
          val bitcastedAlloc  = fresh() // arg.ty*

          val preparationInsts = Seq(
            sh"%$allocatedStruct = alloca $coercety",
            sh"%$bitcastedAlloc = bitcast $coercety* %$allocatedStruct to ${arg.ty}*",
            sh"store $arg, ${arg.ty}* %$bitcastedAlloc"
          )
          val parameterInsertions = newArgs.zipWithIndex.flatMap {
            case (Val.Local(n, t), i) =>
              val ptr = fresh()
              Seq(
                sh"%$ptr = getelementptr $coercety, $coercety* %$allocatedStruct, i32 0, i32 $i",
                sh"%$n = load $t, $t* %$ptr"
              )
          }
          (newArgs, preparationInsts ++ parameterInsertions)
        }
      }
    val newArgs = argChanges.flatMap(_._1)
    val funcType = Type.Function(newArgs.map(_.ty), retty)

    val bind    = if (isVoid(inst.op.resty)) s() else sh"%${inst.name} = "
    val callInstructions = ptr match {
      case Val.Global(pointee, _) =>
        Seq(sh"${bind}call $funcType @$pointee(${r(newArgs, sep = ", ")})")

      case _ =>
        val pointee = fresh()

        Seq(
          sh"%$pointee = bitcast $ptr to $funcType*",
          sh"${bind}call $funcType %$pointee(${r(newArgs, sep = ", ")})"
        )

    }
    argChanges.flatMap(_._2) ++ callInstructions
  }

}
