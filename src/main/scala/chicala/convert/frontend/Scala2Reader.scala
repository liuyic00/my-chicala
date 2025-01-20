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
  case object NotThis extends LRResult[Nothing]
  sealed trait LRSuccess[+A] extends LRResult[A] {
    def cInfo: CircuitInfo

    def map[B](f: A => B): LRSuccess[B]
    def flatMap[B](f: A => LRSuccess[B]): LRSuccess[B]
    def mapCInfo(f: CircuitInfo => CircuitInfo): LRSuccess[A]
  }
  case class Changed[+A](cInfo: CircuitInfo) extends LRSuccess[A] {
    def map[B](f: A => B): Changed[B]                       = Changed(cInfo)
    def flatMap[B](f: A => LRSuccess[B]): Changed[B]        = Changed(cInfo)
    def mapCInfo(f: CircuitInfo => CircuitInfo): Changed[A] = Changed(f(cInfo))
  }
  case class Loaded[+A](cInfo: CircuitInfo, value: A) extends LRSuccess[A] {
    def map[B](f: A => B): Loaded[B]                       = Loaded(cInfo, f(value))
    def flatMap[B](f: A => LRSuccess[B]): Loaded[B]        = f(value).asInstanceOf[Loaded[B]]
    def mapCInfo(f: CircuitInfo => CircuitInfo): Loaded[A] = Loaded(f(cInfo), value)
  }

  type LREither[+A]        = Either[LRError, LRResult[A]]
  type LREitherSuccess[+A] = Either[LRError, LRSuccess[A]]
  type LREitherLoaded[+A]  = Either[LRError, Loaded[A]]

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
