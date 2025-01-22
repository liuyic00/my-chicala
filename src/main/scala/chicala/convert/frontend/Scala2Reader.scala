package chicala.convert.frontend

import chicala.util._
import chicala.ast.ChicalaAst

trait Scala2Reader
    extends ChicalaAst
    // Loader
    with CClassDefsLoader
    with MStatementsLoader
    with MTermsLoader
    with CTermsLoader
    with STermsLoader
    with MTypesLoader
    with COpsLoader
    // TreeReader
    with ApplysReader
    with AssignsReader
    with BlocksReader
    with DefDefsReader
    with FunctionsReader
    with IdentsReader
    with IfsReader
    with LiteralsReader
    with MatchsReader
    with SelectsReader
    with StatementsReader
    with ValDefsReader
    // util
    with ReaderInfos
    with CircuitInfos
    with ChiselAstCheck
    with Printer {

  sealed trait LRError
  sealed trait LRExit              extends LRError
  sealed trait LRSkip              extends LRError
  case object Failed               extends LRSkip
  case object DependentClassNotDef extends LRExit

  sealed trait LRResult[+A]
  sealed trait NotThis extends LRResult[Nothing]
  case object NotThis  extends NotThis
  sealed trait LRSuccess[+A] extends LRResult[A] {
    def mapValue[B](f: A => B): LRSuccess[B]
  }
  sealed trait LRModified[+A] extends LRSuccess[A] {
    def cInfo: CircuitInfo
  }
  sealed trait LRLoaded[+A] extends LRSuccess[A] {
    def value: A
  }

  case class Modified(cInfo: CircuitInfo) extends LRModified[Nothing] {
    def mapValue[B](f: Nothing => B): Modified = this
  }
  case class Loaded[+A](value: A) extends LRLoaded[A] {
    def mapValue[B](f: A => B): Loaded[B] = Loaded(f(value))
  }
  case class ModifiedAndLoaded[+A](cInfo: CircuitInfo, value: A) extends LRLoaded[A] with LRModified[A] {
    def mapValue[B](f: A => B): ModifiedAndLoaded[B] = ModifiedAndLoaded(cInfo, f(value))
  }

  type LREither[+A]  = Either[LRError, LRResult[A]]
  type LREitherT[+T] = Either[LRError, T]

  object LRSuccess {
    def getFirst[T](gens: List[() => LREither[T]]): LREither[T] = {
      gens match {
        case Nil => Right(NotThis)
        case head :: tail =>
          head() match {
            case Right(NotThis) => getFirst(tail)
            case x              => x
          }
      }
    }
  }
}
