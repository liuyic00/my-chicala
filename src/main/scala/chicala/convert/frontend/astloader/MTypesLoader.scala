package chicala.convert.frontend

import scala.tools.nsc.Global
import scala.languageFeature.experimental.macros

trait MTypesLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object CDirectionLoader {
    def apply(tr: Tree): Option[CDirection] = passThrough(tr)._1 match {
      /* Select(Select(Ident(chisel3), chisel3.Input), TermName("apply")) */
      case Select(Select(Ident(TermName(chisel3)), tpe), TermName("apply")) =>
        tpe match {
          case TermName("Input")   => Some(Input)
          case TermName("Output")  => Some(Output)
          case TermName("Flipped") => Some(Flipped)
          case _                   => None
        }
      case _ => None
    }
  }

  trait MTypeLoaderLib {}

  object SignalTypeLoader extends LoadedLoader[SignalType] {
    def getWidth(cInfo: CircuitInfo, args: List[Tree]): CSize = {
      args match {
        case Select(Apply(Select(cp, TermName("fromIntToWidth")), List(w)), TermName("W")) ::
            Nil if isChisel3Package(cp) => {
          STermLoader(cInfo, w) match {
            case Right(Loaded(ww)) => KnownSize(ww)
            case _ =>
              errorTree(args.head, "SignalTypeLoader.getWidth")
              UnknownSize
          }
        }
        case _ =>
          unprocessedTree(args.headOption.getOrElse(EmptyTree), "SignalTypeLoader.getWidth")
          UnknownSize
      }
    }

    private def getVecArgs(cInfo: CircuitInfo, args: List[Tree]): (CSize, SignalType) = {
      if (args.length == 2) {
        val Right(Loaded(ww))        = STermLoader(cInfo, args.head)
        val size                     = KnownSize(ww)
        val Right(Loaded(cDataType)) = SignalTypeLoader(cInfo, args.tail.head)
        (size, cDataType)
      } else {
        reporter.error(args.head.pos, "Unknow arg of Vec")
        (UnknownSize, SignalType.empty)
      }
    }

    def fromType(tpe: Type): Option[SignalType] = {
      tpe.typeConstructor.toString() match {
        case "chisel3.UInt" => Some(UInt.empty)
        case "chisel3.SInt" => Some(SInt.empty)
        case "chisel3.Bool" => Some(Bool.empty)
        case "chisel3.Data" => Some(UInt.empty)
        case "chisel3.Vec"  => Some(Vec.empty(fromType(tpe.typeArgs.head).get))
        case _: String =>
          if (tpe.toString().endsWith(".type")) fromType(tpe.erasure)
          else None
      }
    }

    def fromTpt(tree: Tree): Option[SignalType] = {
      val tpe            = autoTypeErasure(tree)
      val someSignalType = fromType(tpe)
      if (someSignalType.isEmpty)
        errorTree(tree, s"unknow data type `${tpe}` in SignalTypeLoader.fromTpt")
      someSignalType
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[SignalType]] = {
      val tree = passThrough(tr)._1
      tree match {
        case Apply(fun, args) =>
          val someDirection = CDirectionLoader(fun)
          someDirection match {
            /* Apply(<Input(_)>, List(<UInt(width.W)>)) */
            case Some(direction) =>
              SignalTypeLoader(cInfo, args.head).map(_.mapValue(_.updatedDriction(direction)))

            case None =>
              val f = passThrough(fun)._1
              f match {
                /* Apply(<UInt(_)>, List(<width.W>)) */
                case Select(Select(cp, name), TermName("apply")) if isChisel3Package(cp) =>
                  name match {
                    case TermName("UInt") => Right(Loaded(UInt.empty.updatedWidth(getWidth(cInfo, args))))
                    case TermName("SInt") => Right(Loaded(SInt.empty.updatedWidth(getWidth(cInfo, args))))
                    case TermName("Bool") => Right(Loaded(Bool.empty))
                    case TermName("Vec") =>
                      val (size, sigType) = getVecArgs(cInfo, args)
                      Right(Loaded(Vec(size, Node, sigType)))
                    case _ =>
                      unprocessedTree(f, "SignalTypeLoader #1")
                      Left(Failed)
                  }
                /* Apply(<new SomeBundle(_)>, List(<args>)) */
                case Select(New(tpt), termNames.CONSTRUCTOR) =>
                  val bundleFullName = tpt.tpe.toString()
                  val eitherBundleDef = cInfo.readerInfo.bundleDefs
                    .get(bundleFullName)
                    .toRight(DependentClassNotDef)
                  eitherBundleDef.flatMap { bundleDef =>
                    MTermLoader
                      .loadTerms(cInfo, args)
                      .map(_.mapValue(bundleDef.applyArgs(_).bundle))
                  }
                case _ =>
                  unprocessedTree(f, "SignalTypeLoader #2")
                  Left(Failed)
              }
          }

        case Block(stats, _) =>
          BundleDefLoader(cInfo, stats.head, "").map(_.mapValue(_.bundle))
        case _ =>
          errorTree(tree, "SignalTypeLoader #3")
          Left(Failed)
      }
    }
  }

  object STypeLoader extends MTypeLoaderLib {

    private val wrappedTypes = List(
      "scala.collection.immutable.Range",
      "scala.collection.immutable.Range.Exclusive",
      "scala.collection.WithFilter[Any,[_]Any]",
      "scala.collection.ArrayOps[",
      "ArrowAssoc[",
      "Nothing"
    )
    private def isSeq(tpe: Type): Boolean = {
      val typeStr = tpe.toString()
      List(
        "IndexedSeq",
        "Seq"
      ).exists(typeStr.startsWith(_))
    }
    private def isArray(tpe: Type): Boolean = {
      val typeStr = tpe.toString()
      List(
        "Array"
      ).exists(typeStr.startsWith(_))
    }

    def fromTpt(tr: Tree): Option[SType] = {
      val tpe = autoTypeErasure(tr)
      if (isScala2TupleType(TypeTree(tpe))) {
        Some(StTuple(tr.tpe.typeArgs.map(x => MTypeLoader.fromTpt(TypeTree(x)).get)))
      } else if ("""(.*): .*""".r.matches(tpe.toString())) {
        Some(StFunc)
      } else if (tr.toString() == "Any") {
        Some(StAny)
      } else if (isSeq(tpe)) {
        val tparam = tpe.typeArgs match {
          case head :: next => MTypeLoader.fromTpt(TypeTree(head)).get
          case Nil          => StAny
        }
        Some(StSeq(tparam))
      } else if (isArray(tpe)) {
        val tparam = tpe.typeArgs match {
          case head :: next => MTypeLoader.fromTpt(TypeTree(head)).get
          case Nil          => StAny
        }
        Some(StArray(tparam))
      } else if (wrappedTypes.exists(tpe.toString().startsWith(_))) {
        Some(StWrapped(tpe.toString()))
      } else {
        tpe.erasure.toString() match {
          case "Int"                     => Some(StInt)
          case "String"                  => Some(StString)
          case "scala.math.BigInt"       => Some(StBigInt)
          case "Boolean"                 => Some(StBoolean)
          case "scala.runtime.BoxedUnit" => Some(StUnit)
          case s =>
            assertWarning(true, tr.pos, s"This type `${s}` need check")
            Some(StWrapped(s))
        }
      }
    }
  }

  object MTypeLoader {
    def fromTpt(tr: Tree): Option[MType] = {
      if (isChiselSignalType(tr)) SignalTypeLoader.fromTpt(tr)
      else STypeLoader.fromTpt(tr)
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, LRSuccess[MType]] = {
      if (isChiselSignalType(tr))
        SignalTypeLoader(cInfo, tr)
      else
        STypeLoader.fromTpt(tr).map(ModifiedAndLoaded(cInfo, _)).toRight(Failed)
    }
  }
}
