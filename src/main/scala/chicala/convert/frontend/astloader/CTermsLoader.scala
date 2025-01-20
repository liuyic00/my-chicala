package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object ConnectLoader extends Loader[Connect] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[Connect] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, TermName("$colon$eq")), args) if isChiselSignalType(qualifier) =>
          assert(args.length == 1, "should have one right expr")

          MTermLoader
            .loadTerms(cInfo, List(qualifier, args.head))
            .flatMap {
              case Loaded(newCInfo, left :: right :: Nil) =>
                Right(Loaded(newCInfo, Connect(left, right)))
              case _ => loadMutilpleMatchError(qualifier)
            }
        case _ => Right(NotThis)
      }
    }
  }
  object CApplyLoader extends Loader[CApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[CApply] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselSignalType(qualifier) =>
          val opName = name.toString()
          COpLoader(opName) match {
            case Some(op) =>
              val tpe = MTypeLoader.fromTpt(tpt).get match {
                case StSeq(tparam: SignalType) => Vec(InferredSize, Node, tparam.setInferredWidth)
                case x: SignalType             => x.setInferredWidth
                case x =>
                  errorTree(tpt, s"Not a processable MType `${x}`")
                  SignalType.empty
              }
              MTermLoader
                .loadTerms(cInfo, qualifier :: args)
                .map { case Loaded(newCInfo, operands) => Loaded(newCInfo, CApply(op, tpe, operands)) }
            case None =>
              unprocessedTree(tr, s"CApplyLoader `${opName}`")
              Left(Failed)
          }
        case a @ Apply(fun, args) => {
          val (f, _) = passThrough(fun)
          val fName  = f.toString()
          COpLoader(fName) match {
            case Some(op) =>
              val tpe = SignalTypeLoader.fromTpt(tpt).get.setInferredWidth
              MTermLoader.loadTerms(cInfo, args).map { case Loaded(newCInfo, operands) =>
                Loaded(newCInfo, CApply(op, tpe, operands))
              }
            case None => Right(NotThis)
          }
        }
      }
    }
  }
  object WhenLoader extends Loader[When] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[When] = {
      def pushBackElseWhen(when: When, elseWhen: When): When = when match {
        case When(cond, whenp, otherp, true) =>
          When(
            cond,
            whenp,
            pushBackElseWhen(otherp.asInstanceOf[When], elseWhen),
            true
          )
        case When(cond, whenp, otherp, false) =>
          When(cond, whenp, elseWhen, true)
      }

      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Apply(cwa, condArgs), args) if isChisel3WhenApply(cwa) => {
          loadMutilple(cInfo)(
            MTermLoader(_, condArgs.head),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded(newCInfo, (cond: MTerm) :: whenp :: Nil) =>
              Right(Loaded(newCInfo, When(cond, whenp, EmptyMTerm)))
            case _ => loadMutilpleMatchError(condArgs.head)
          }
        }
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          loadMutilple(cInfo)(
            WhenLoader(_, qualifier),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded(newCInfo, (when: When) :: otherp :: Nil) =>
              Right(Loaded(newCInfo, When(when.cond, when.whenp, otherp)))
            case _ => loadMutilpleMatchError(qualifier)
          }
        }
        case Apply(Apply(Select(qualifier, TermName("elsewhen")), condArgs), args) => {
          loadMutilple(cInfo)(
            WhenLoader(_, qualifier),
            MTermLoader(_, condArgs.head),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded(newCInfo, (when: When) :: (elseCond: MTerm) :: elseThen :: Nil) =>
              Right(Loaded(newCInfo, pushBackElseWhen(when, When(elseCond, elseThen, EmptyMTerm))))
            case _ => loadMutilpleMatchError(qualifier)
          }
        }
        case _ => Right(NotThis)
      }
    }
  }

  object SwitchLoader extends Loader[Switch] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[Switch] = {
      val (tree, tpt) = passThrough(tr)
      if (!isChisel3UtilSwitchContextType(tpt)) return Right(NotThis)

      tree match {
        case Apply(Apply(Select(qualifier, TermName("is")), vArgs), bodyArgs) =>
          loadMutilple(cInfo)(
            SwitchLoader(_, qualifier),
            MTermLoader(_, vArgs.head),
            MTermLoader(_, bodyArgs.head)
          ).flatMap {
            case Loaded(newCInfo, (switch: Switch) :: v :: branchp :: Nil) =>
              Right(Loaded(newCInfo, switch.appended(v, branchp)))
            case _ => loadMutilpleMatchError(qualifier)
          }
        case Apply(Select(New(t), termNames.CONSTRUCTOR), args) if isChisel3UtilSwitchContextType(t) =>
          loadMutilple(cInfo)(
            MTermLoader(_, args.head)
          ).flatMap {
            case Loaded(newCInfo, cond :: Nil) =>
              Right(Loaded(newCInfo, Switch(cond, List.empty)))
            case _ => loadMutilpleMatchError(args.head)
          }
        case _ =>
          errorTree(tr, "Unknow structure in SwitchLoader")
          Left(Failed)
      }

    }
  }

  object AssertLoader extends Loader[Assert] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[Assert] = {
      val (tree, _) = passThrough(tr)
      if (isReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            MTermLoader(cInfo, args.head).map { case Loaded(newCInfo, ast) =>
              Loaded(newCInfo, Assert(ast))
            }
          case _ => Right(NotThis)
        }
      } else Right(NotThis)
    }
  }

  object LitLoader extends Loader[Lit] {
    private def nameToSomeLitGen(name: Name): (STerm, CSize) => Option[Lit] = {
      name.toString() match {
        case "U" => (litExp, width) => Some(Lit(litExp, UInt(width, Node, Undirect)))
        case "S" => (litExp, width) => Some(Lit(litExp, SInt(width, Node, Undirect)))
        case "B" => (litExp, width) => Some(Lit(litExp, Bool(Node, Undirect)))
        case _   => (litExp, width) => None
      }
    }
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[Lit] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier: Apply, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          STermLoader(cInfo, qualifier.args.head).flatMap { case Loaded(newCInfo, litExp) =>
            val width = SignalTypeLoader.getWidth(newCInfo, args) match {
              case k: KnownSize => k
              case _            => InferredSize
            }
            nameToSomeLitGen(name)(litExp, width) match {
              case Some(lit) => Right(Loaded(newCInfo, lit))
              case None =>
                errorTree(tree, "Unknow name in CExp")
                Left(Failed)
            }
          }
        }
        case Select(qualifier: Apply, name) if isChiselLiteralType(qualifier) => {
          // someInt.U without width
          STermLoader(cInfo, qualifier.args.head).flatMap { case Loaded(newCInfo, litExp) =>
            nameToSomeLitGen(name)(litExp, InferredSize) match {
              case Some(lit) => Right(Loaded(newCInfo, lit))
              case None =>
                errorTree(tree, "Unknow name in CExp")
                Left(Failed)
            }
          }
        }
        case _ => Right(NotThis)
      }
    }
  }
}
