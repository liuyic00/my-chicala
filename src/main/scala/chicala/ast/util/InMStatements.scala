package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait InMStatements extends Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  class InMStatementHasTermName(mStatement: MStatement, tn: TermName) extends Transformer {
    var has = false
    override def transformTermName(termName: TermName): TermName = {
      if (tn.equals(termName)) {
        has = true
      }
      termName
    }
    override def transform(mStatement: MStatement): MStatement = {
      if (has) mStatement
      else super.transform(mStatement)
    }
    transform(mStatement)
  }
  object InMStatementHasTermName {
    def apply(mStatement: MStatement, termName: TermName): Boolean = {
      new InMStatementHasTermName(mStatement, termName).has
    }
  }
}
