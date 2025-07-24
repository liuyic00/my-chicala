package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object ConnectLoader extends LoadedLoader[Connect] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[Connect]] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, TermName("$colon$eq")), args) if isChiselSignalType(qualifier) =>
          assert(args.length == 1, "should have one right expr")

          MTermLoader
            .loadTerms(cInfo, List(qualifier, args.head))
            .flatMap {
              case Loaded(left :: right :: Nil) =>
                Right(Loaded(Connect(left, right)))
              case _ => loadMutilpleMatchError(qualifier)
            }
        case _ => Left(NotThis)
      }
    }
  }
  object CApplyLoader extends LoadedLoader[CApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[CApply]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) => {
          val (f, _) = passThrough(fun)
          f match {
            case Select(qualifier, name) if isChiselSignalType(qualifier) => {
              val opName = name.toString()
              COpLoader(opName) match {
                case Some(op) =>
                  val someOperands = op match {
                    case AsTypeOf =>
                      // TODO: remove or refactor SignalTypeLoader here, load a signalType generator directly
                      (for {
                        signal <- MTermLoader.must(cInfo, qualifier).map(_.value)
                        typegen <-
                          firstMatchIn(
                            cInfo,
                            args.head,
                            List(
                              SignalTypeLoader(_: CircuitInfo, _: Tree)
                                .map(_.mapValue(signalType => GenCType(signalType))),
                              MTermLoader(_, _)
                            )
                          ).map(_.value)
                      } yield {
                        Loaded(List(signal, typegen))
                      }).left
                        .flatMap {
                          case NotThis =>
                            unprocessedTree(tr, "[@CApplyLoader_1]")
                            Left(Failed)
                          case x: LRError => Left(x)
                        }
                    case _ =>
                      MTermLoader
                        .loadTerms(cInfo, qualifier :: args)
                  }
                  someOperands.map(_.mapValue(operands => {
                    CApply(op, operands)
                  }))
                case None =>
                  unprocessedTree(tr, s"CApplyLoader `${opName}`")
                  Left(Failed)
              }
            }
            case _ => {
              val fName = f.toString()
              COpLoader(fName) match {
                case Some(op) =>
                  MTermLoader.loadTerms(cInfo, args).map { case Loaded(operands) =>
                    Loaded(CApply(op, operands))
                  }
                case None => Left(NotThis)
              }
            }

          }

        }
        case _ => Left(NotThis)
      }
    }
  }
  object WhenLoader extends LoadedLoader[When] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[When]] = {
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
            MTermLoader.must(_, condArgs.head),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded((cond: MTerm) :: whenp :: Nil) =>
              Right(Loaded(When(cond, whenp, EmptyMTerm)))
            case _ => loadMutilpleMatchError(condArgs.head)
          }
        }
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          loadMutilple(cInfo)(
            WhenLoader.must(_, qualifier),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded((when: When) :: otherp :: Nil) =>
              if (when.hasElseWhen) {
                assertError(
                  when.otherp.isInstanceOf[When],
                  qualifier.pos,
                  "When with hasElseWhen should have otherp as When, but got " + otherp.getClass
                )
                val elseWhen = when.otherp.asInstanceOf[When]
                Right(Loaded(When(when.cond, when.whenp, When(elseWhen.cond, elseWhen.whenp, otherp), true)))
              } else {
                assertError(when.otherp == EmptyMTerm, qualifier.pos, "When should not have otherp here")
                Right(Loaded(When(when.cond, when.whenp, otherp)))
              }
            case _ => loadMutilpleMatchError(qualifier)
          }
        }
        case Apply(Apply(Select(qualifier, TermName("elsewhen")), condArgs), args) => {
          loadMutilple(cInfo)(
            WhenLoader.must(_, qualifier),
            MTermLoader.must(_, condArgs.head),
            StatementReader(_, args.head)
          ).flatMap {
            case Loaded((when: When) :: (elseCond: MTerm) :: elseThen :: Nil) =>
              Right(Loaded(pushBackElseWhen(when, When(elseCond, elseThen, EmptyMTerm))))
            case _ => loadMutilpleMatchError(qualifier)
          }
        }
        case _ => Left(NotThis)
      }
    }
  }

  object SwitchLoader extends LoadedLoader[Switch] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[Switch]] = {
      val (tree, tpt) = passThrough(tr)
      if (!isChisel3UtilSwitchContextType(tpt)) return Left(NotThis)

      tree match {
        case Apply(Apply(Select(qualifier, TermName("is")), vArgs), bodyArgs) =>
          loadMutilple(cInfo)(
            SwitchLoader.must(_, qualifier),
            MTermLoader.must(_, vArgs.head),
            MTermLoader.must(_, bodyArgs.head)
          ).flatMap {
            case Loaded((switch: Switch) :: v :: branchp :: Nil) =>
              Right(Loaded(switch.appended(v, branchp)))
            case _ => loadMutilpleMatchError(qualifier)
          }
        case Apply(Select(New(t), termNames.CONSTRUCTOR), args) if isChisel3UtilSwitchContextType(t) =>
          loadMutilple(cInfo)(
            MTermLoader.must(_, args.head)
          ).flatMap {
            case Loaded(cond :: Nil) =>
              Right(Loaded(Switch(cond, List.empty)))
            case _ => loadMutilpleMatchError(args.head)
          }
        case _ =>
          errorTree(tr, "Unknow structure in SwitchLoader")
          Left(Failed)
      }

    }
  }

  object AssertLoader extends LoadedLoader[Assert] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[Assert]] = {
      val (tree, _) = passThrough(tr)
      if (isReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            MTermLoader.must(cInfo, args.head).map { case Loaded(ast) =>
              Loaded(Assert(ast))
            }
          case _ => Left(NotThis)
        }
      } else Left(NotThis)
    }
  }

  object LitLoader extends LoadedLoader[Lit] {
    private def nameToSomeLitGen(name: Name): (STerm, CSize) => Option[Lit] = {
      name.toString() match {
        case "U" => (litExp, width) => Some(Lit(litExp, UInt(width, Node, Undirect)))
        case "S" => (litExp, width) => Some(Lit(litExp, SInt(width, Node, Undirect)))
        case "B" => (litExp, width) => Some(Lit(litExp, Bool(Node, Undirect)))
        case _   => (litExp, width) => None
      }
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[Lit]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier: Apply, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          STermLoader.must(cInfo, qualifier.args.head).flatMap { case Loaded(litExp) =>
            val width = SignalTypeLoader.getWidth(cInfo, args) match {
              case k: KnownSize => k
              case _            => InferredSize
            }
            nameToSomeLitGen(name)(litExp, width) match {
              case Some(lit) => Right(Loaded(lit))
              case None =>
                errorTree(tree, "Unknow name in CExp")
                Left(Failed)
            }
          }
        }
        case Select(qualifier: Apply, name) if isChiselLiteralType(qualifier) => {
          // someInt.U without width
          STermLoader.must(cInfo, qualifier.args.head).flatMap { case Loaded(litExp) =>
            nameToSomeLitGen(name)(litExp, InferredSize) match {
              case Some(lit) => Right(Loaded(lit))
              case None =>
                errorTree(tree, "Unknow name in CExp")
                Left(Failed)
            }
          }
        }
        case _ => Left(NotThis)
      }
    }
  }

  object GenCTypeLoader extends LoadedLoader[GenCType] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[GenCType]] = {
      SignalTypeLoader(cInfo, tr).map(_.mapValue(GenCType(_)))
    }
  }
}
