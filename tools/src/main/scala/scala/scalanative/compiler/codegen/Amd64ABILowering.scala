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

  // Don't use this name for anything else, than the parameter used for returning large structs.
  // The code using it has to access it from two different places (start of the block and the ret instruction),
  // so it can't use a fresh() identifier
  val ReturnPtrParam = Local("gen.abi.ret", 0)

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
        case 0 => Seq() // Empty struct, ignore
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

  // Return types seem to follow the same coertion scheme as arguments
  // If this function returns a pointer, the actual return type should be void, and the returned struct should be
  // written into space allocated by the caller
  def coerceReturnType(ret: Type): Type = coerceArgument(ret) match {
    case Seq(t) => t
    case s => Type.Struct(Global.None, s)
  }

  def  coerceFunctionType(t: Type): Type = {
    val Type.Function(argtys, retty) = t
    val ret = coerceReturnType(retty)
    val args = coerceArguments(argtys)
    if(ret != retty && ret == Type.Ptr)
      Type.Function(Type.Ptr +: args, Type.Void)
    else
      Type.Function(args, ret)
  }

  def coerceReturn(ret: Cf.Ret): Seq[Show.Result] = {
    val retval = ret.value
    coerceReturnType(retval.ty) match {
      case Type.Ptr if retval.ty != Type.Ptr =>
        val bitcasted = fresh()
        Seq(
          sh"%$bitcasted = bitcast i8* %$ReturnPtrParam to ${retval.ty}*",
          sh"store $retval, ${retval.ty}* %$bitcasted",
          sh"ret void"
        )
      case _ =>
        Seq(sh"ret $retval")
    }
  }

  def coerceCall(inst: Inst): Seq[Show.Result] = {

    def isVoid(ty: Type): Boolean =
      ty == Type.Void || ty == Type.Unit || ty == Type.Nothing

    val Op.Call(funty @ Type.Function(_, retty), ptr, args) = inst.op
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

    val coercedRetType = coerceReturnType(retty)
    val (retn: Option[Local], preRetInsts: Seq[Show.Result], postRetInsts: Seq[Show.Result], additionalArgs: Seq[Val]) =
      if(coercedRetType == retty) {
        (Some(inst.name), Seq(), Seq(), Seq())
      } else if(coercedRetType == Type.Ptr) {
        val alloc = fresh()
        val bitcasted = fresh()
        val pre = Seq(
          sh"%$alloc = alloca $retty",
          sh"%$bitcasted = bitcast $retty* %$alloc to i8*"
        )
        val post = sh"%${inst.name} = load $retty, $retty* %$alloc"
        (None, pre, Seq(post), Seq(Val.Local(bitcasted, Type.Ptr)))
      } else {
        val alloc, bitcasted, ret = fresh()
        val post = Seq(
          sh"%$alloc = alloca $coercedRetType",
          sh"store $coercedRetType %$ret, $coercedRetType* %$alloc",
          sh"%$bitcasted = bitcast $coercedRetType* %$alloc to $retty*",
          sh"%${inst.name} = load $retty, $retty* %$bitcasted"
        )
        (Some(ret), Seq(), post, Seq())
      }



    val newArgs = additionalArgs ++ argChanges.flatMap(_._1)
//    val funcType = Type.Function(newArgs.map(_.ty), coercedRetType)
    val funcType = coerceFunctionType(funty)


    val bind    = if (isVoid(retty) || retn.isEmpty) s() else sh"%${retn.get} = "
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
    argChanges.flatMap(_._2) ++ preRetInsts ++ callInstructions ++ postRetInsts
  }

  def coerceCallee(args: Seq[Val.Local], retty: Type): (Seq[Val], Seq[Show.Result]) = {
    val argChanges: Seq[(Seq[Val.Local], Seq[Show.Result])] =
      for(arg <- args) yield {
        val coercedtys = coerceArgument(arg.ty)
        if(coercedtys.size == 1 && coercedtys.head == arg.ty) {
          (Seq(arg), Seq())
        } else if(coercedtys.size == 1 && coercedtys.head == Type.Ptr) {
          val bitcasted, param = fresh()
          val insts = Seq(
            sh"%$bitcasted = bitcast i8* %$param to ${arg.ty}*",
            sh"%${arg.name} = load ${arg.ty}, ${arg.ty}* %$bitcasted"
          )

          (Seq(Val.Local(param, Type.Ptr)), insts)
        } else {
          val newArgs = coercedtys.map(t => Val.Local(fresh(), t))
          val structty = Type.Struct(Global.None, coercedtys)
          val allocated, bitcasted = fresh()
          val preInsts = Seq(
            sh"%$allocated = alloca $structty",
            sh"%$bitcasted = bitcast $structty* %$allocated to ${arg.ty}*"
          )
          val memberInsts = newArgs.zipWithIndex.flatMap {
            case (v @ Val.Local(_, ty), i) =>
              val elemptr = fresh()
              Seq(
                sh"%$elemptr = getelementptr $structty, $structty* %$allocated, i32 0, i32 $i",
                sh"store $v, $ty* %$elemptr"
              )
          }
          val postInsts = Seq(
            sh"%${arg.name} = load ${arg.ty}, ${arg.ty}* %$bitcasted"
          )

          (newArgs, preInsts ++ memberInsts ++ postInsts)
        }
      }
    val coercedRetType = coerceReturnType(retty)
    val additionalArgs =
      if(coercedRetType == Type.Ptr && retty != Type.Ptr)
        Seq(Val.Local(ReturnPtrParam, Type.Ptr))
      else Seq()
    (additionalArgs ++ argChanges.flatMap(_._1), argChanges.flatMap(_._2))
  }

}
