package scala.scalanative
package compiler
package pass

import scala.scalanative.nir._

/**
 * Created by Kamil on 09.07.2016.
 */
class Amd64AbiLowering(implicit fresh: Fresh, top: analysis.ClassHierarchy.Top) extends Pass {

  sealed trait CoercionResult
  final case class Unchanged(ty: Type)  extends CoercionResult
  final case class Decompose(ty: Type*) extends CoercionResult
  final case class ByPointer(ty: Type)  extends CoercionResult

  def coerceArguments(args: Seq[Arg]): Seq[Arg] =
    args map (a => coerceArgument(a.ty)) flatMap {
      case Unchanged(t) => Seq(Arg(t))
      case d: Decompose => d.ty map (Arg(_))
      case ByPointer(t) => Seq(Arg(Type.Ptr, ArgAttrs(byval = Some(t))))
    }

  def size(t: Type): Int = t match {
    case Type.Bool         => 1
    case Type.I(w)         => w / 8
    case Type.F(w)         => w / 8
    case Type.Ptr          => 8
    case Type.Array(e, n)  => size(e) * n
    case Type.Struct(_, e) => e.map(size).sum
  }

  def coerceArgument(arg: Type): CoercionResult = arg match {
    case struct @ Type.Struct(name, tys) =>
      size(struct) match {
        case s if s <= 8 => // One parameter
          Decompose(Type.I(s * 8))
        case s if s <= 16 => // Two parameters
          Decompose(Type.I64, Type.I((s - 8) * 8))
        case _ => // Pointer
          ByPointer(struct)
      }
    // TODO: Once we get vector types their lowering should be handled here too
    case x => Unchanged(x)
  }

  def coerceReturnType(ret: Type): CoercionResult = coerceArgument(ret)

  def coerceFunctionType(t: Type.Function): Type = {
    val ret  = coerceReturnType(t.ret)
    val args = coerceArguments(t.args)
    ret match {
      case ByPointer(ty)  => Type.Function(Arg(Type.Ptr, ArgAttrs(sret = Some(ty))) +: args, Type.Void)
      case Unchanged(ty) => Type.Function(args, ty)
      case d: Decompose  => Type.Function(args, Type.Struct(Global.None, d.ty))
    }
  }

  private var smallParamAlloc: Val.Local         = _
  private var returnPointerParam: Val.Local      = _
  private var returnCoercionType: CoercionResult = _
  private val SmallParamStructType =
    Type.Struct(Global.None, Seq(Type.I64, Type.I64))

  private var bigParamAllocs: Seq[Val.Local] = _
  private var bigParamAllocType: Type        = _

  private var bigReturnAlloc: Val.Local   = _
  private var bigReturnType: Option[Type] = _

  override def preDefn: OnDefn = {
    case defn @ Defn
          .Define(attrs, name, Type.Function(argtys, retty), blocks)
      if attrs.isExtern =>
      val smallParamAllocNeeded = blocks.exists(_.insts.exists {
        case Inst(_, Op.Call(ty: Type.Function, _, _)) =>
          ty.args.exists(_.ty.isInstanceOf[Type.Struct])
        case _ => false
      })
      smallParamAlloc =
        if(smallParamAllocNeeded) Val.Local(fresh(), Type.Ptr)
        else null
      returnCoercionType = coerceReturnType(retty)
      returnPointerParam = returnCoercionType match {
        case ByPointer(_) =>
          Val.Local(fresh(), Type.Ptr)
        case _ => null
      }
      val allocs = for {
        block                                     <- blocks
        Inst(_, Op.Call(ty: Type.Function, _, _)) <- block.insts
      } yield
        for {
          arg <- ty.args
          coerced = coerceArgument(arg.ty)
          if coerced.isInstanceOf[ByPointer]
        } yield arg.ty
      val maxArgs    = (0 +: allocs.map(_.size)).max
      val maxArgSize = (0 +: allocs.flatten.map(size)).max
      bigParamAllocType = Type.Array(Type.I8, maxArgSize)
      bigParamAllocs = Seq.fill(maxArgs)(Val.Local(fresh(), Type.Ptr))

      val rets = for {
        block                                           <- blocks
        Inst(_, Op.Call(Type.Function(_, retty), _, _)) <- block.insts
        coerced = coerceReturnType(retty)
        if coerced.isInstanceOf[ByPointer]
      } yield retty
      val maxRetSize = (0 +: rets.map(size)).max
      bigReturnType =
        if (maxRetSize > 0) Some(Type.Array(Type.I8, maxRetSize))
        else None: Option[Type]
      bigReturnAlloc = Val.Local(fresh(), Type.Ptr)

      Seq(defn)
  }

  override def postDefn: OnDefn = {
    case Defn.Declare(attrs, name, ty: Type.Function)
      if attrs.isExtern =>
      Seq(Defn.Declare(attrs, name, coerceFunctionType(ty)))

    case Defn
.Define(attrs,
                      name,
                      ty: Type.Function,
                      entryBlock +: otherBlocks) =>
      val newEntryBlock = entryBlock match {
        case Block(blockName, params, insts, cf) =>
          val allocSmallParam =
            Option(smallParamAlloc).map(a => Inst(a.name, Op.Stackalloc(SmallParamStructType)))

          val allocBigRet =
            bigReturnType.map(t => Inst(bigReturnAlloc.name, Op.Stackalloc(t)))

          val allocBigParams = bigParamAllocs.map {
            case Val.Local(n, _) => Inst(n, Op.Stackalloc(bigParamAllocType))
          }

          val argCoercions: Seq[(Seq[Val.Local], Seq[Inst])] =
            for (param <- params)
              yield
                coerceArgument(param.valty) match {
                  case Unchanged(_) =>
                    (Seq(param), Seq())
                  case ByPointer(_) =>
                    val ptr = Val.Local(fresh(), Type.Ptr)
                    (Seq(ptr),
                     Seq(
                         Inst(param.name, Op.Load(param.ty, ptr))
                     ))
                  case Decompose(tys @ _ *) =>
                    val newArgs          = tys.map(ty => Val.Local(fresh(), ty))
                    val coerceStructType = Type.Struct(Global.None, tys)
                    val storeInsts = newArgs.zipWithIndex.flatMap {
                      case (v, i) =>
                        val ptr = Val.Local(fresh(), Type.Ptr)
                        Seq(
                            Inst(ptr.name,
                                 Op.Elem(coerceStructType,
                                         smallParamAlloc,
                                         Seq(Val.I32(0), Val.I32(i)))),
                            Inst(Op.Store(v.ty, ptr, v))
                        )
                    }
                    val loadInst =
                      Inst(param.name, Op.Load(param.ty, smallParamAlloc))
                    (newArgs, storeInsts :+ loadInst)
                }

          val additionalParams: Seq[Val.Local] = returnCoercionType match {
            case ByPointer(_) =>
              Seq(returnPointerParam)
            case _ => Seq()
          }

          Block(
              blockName,
              additionalParams ++ argCoercions.flatMap(_._1),
              allocSmallParam.toSeq ++ allocBigRet ++ allocBigParams ++ argCoercions
                .flatMap(_._2) ++ insts,
              cf)
      }
      val defn = Defn.Define(attrs,
                             name,
                             coerceFunctionType(ty),
                             newEntryBlock +: otherBlocks)
      Seq(defn)
  }

  override def postInst: OnInst = {
    case Inst(res, Op.Call(functy: Type.Function, ptr, args))
      if top.methods =>
      var bigParamIndex = 0
      val argCoertions: Seq[(Seq[Val], Seq[Inst])] = for (arg <- args)
        yield
          coerceArgument(arg.ty) match {
            case Unchanged(_) =>
              (Seq(arg), Seq())
            case ByPointer(_) =>
              val alloc = bigParamAllocs(bigParamIndex)
              bigParamIndex += 1
              (Seq(alloc),
               Seq(
                   Inst(Op.Store(arg.ty, alloc, arg))
               ))
            case Decompose(coerced @ _ *) =>
              val newArgs       = coerced map (t => Val.Local(fresh(), t))
              val coerceType    = Type.Struct(Global.None, coerced)
              val coercedStruct = Val.Local(fresh(), coerceType)

              val prepareInst = Inst(Op.Store(arg.ty, smallParamAlloc, arg))
              val parameterInsertions = newArgs.zipWithIndex.flatMap {
                case (v, i) =>
                  val ptr = Val.Local(fresh(), Type.Ptr)
                  Seq(
                      Inst(ptr.name,
                           Op.Elem(coerceType,
                                   smallParamAlloc,
                                   Seq(Val.I32(0), Val.I32(i)))),
                      Inst(v.name, Op.Load(v.ty, ptr))
                  )
              }
              (newArgs, prepareInst +: parameterInsertions)
          }

      val coercedRetType = coerceReturnType(functy.ret)
      val retCoercions: (Option[Local], Seq[Inst], Seq[Inst], Seq[Val]) =
        coercedRetType match {
          case Unchanged(Type.Void | Type.Unit | Type.Nothing) =>
            (None, Seq(), Seq(), Seq())
          case Unchanged(_) =>
            (Some(res), Seq(), Seq(), Seq())
          case ByPointer(_) =>
            val post = Inst(res, Op.Load(functy.ret, bigReturnAlloc))
            (None, Seq(), Seq(post), Seq(bigReturnAlloc))
          case Decompose(tys @ _ *) =>
            val struct = Type.Struct(Global.None, tys)
            val ret    = Val.Local(fresh(), struct)
            val post = Seq(
                Inst(Op.Store(struct, smallParamAlloc, ret)),
                Inst(res, Op.Load(functy.ret, smallParamAlloc))
            )
            (Some(ret.name), Seq(), post, Seq())
        }
      val (retn: Option[Local],
           preRetInsts: Seq[Inst],
           postRetInsts: Seq[Inst],
           additionalArgs: Seq[Val]) = retCoercions

      val call = Inst(retn getOrElse fresh(),
                      Op.Call(coerceFunctionType(functy),
                              ptr,
                              additionalArgs ++ argCoertions.flatMap(_._1)))
      argCoertions.flatMap(_._2) ++ preRetInsts ++ Seq(call) ++ postRetInsts
  }

  override def postBlock: OnBlock = {
    case block @ Block(name, params, insts, Cf.Ret(v)) =>
      returnCoercionType match {
        case ByPointer(_) =>
          val storeInst = Inst(Op.Store(v.ty, returnPointerParam, v))
          Seq(Block(name, params, insts :+ storeInst, Cf.Ret(Val.None)))
        case Decompose(tys @ _ *) =>
          val struct = Type.Struct(Global.None, tys)
          val ret    = Val.Local(fresh(), struct)
          val retInsts = Seq(
              Inst(Op.Store(v.ty, smallParamAlloc, v)),
              Inst(ret.name, Op.Load(struct, smallParamAlloc))
          )
          Seq(Block(name, params, insts ++ retInsts, Cf.Ret(ret)))
        case _ => Seq(block)
      }
  }
}

object Amd64AbiLowering extends PassCompanion {

  override def apply(ctx: Ctx): Pass = new Amd64AbiLowering()(ctx.fresh, ctx.top)

}
