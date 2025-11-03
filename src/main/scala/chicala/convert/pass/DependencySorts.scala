package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ast.util.Transformers
import chicala.ChicalaConfig

trait DependencySorts extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object DependencySort extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => dependencySort(m)
        case x            => x
      }
    }

    def getDependencyGraph(body: List[MStatement], isModuleTop: Boolean = false)(implicit
        moduleName: String
    ): DirectedGraph = {
      import scala.collection.mutable

      val vertexs = mutable.Set.empty[Id]
      val edges   = mutable.Set.empty[DirectedEdge]

      def addEdges(newEdges: Iterable[DirectedEdge], debugStatement: MStatement = EmptyMTerm): Unit = {
        {
          val selfLoop = newEdges.filter(x => x.from == x.to).toList.sorted
          assertError(
            selfLoop.isEmpty,
            NoPosition,
            s"Self loop is not allowed: ${selfLoop.mkString(", ")}\nimport by ${debugStatement}"
          )
        }
        edges ++= newEdges
      }

      def mergedMapSet[T1, T2](a: Map[T1, Set[T2]], b: Map[T1, Set[T2]]): Map[T1, Set[T2]] = {
        b.foldLeft(a) { case (m, (key, set)) =>
          m.updated(key, m.getOrElse(key, Set.empty) ++ set)
        }
      }

      def getVertexAndLastConnectDependcy(
          id: Id,
          statement: MStatement,
          last: Map[String, Set[Id]],
          isModuleTop: Boolean
      ): Map[String, Set[Id]] = {

        def mergedTwoBranchLast(
            lastMapOne: Map[String, Set[Id]],
            lastMapTwo: Map[String, Set[Id]]
        ): Map[String, Set[Id]] = {
          mergedMapSet(lastMapOne, lastMapTwo)
        }
        def updatedLast(
            last: Map[String, Set[Id]],
            id: Id,
            signals: Set[String]
        ): Map[String, Set[Id]] = {
          val lastIds = signals.map(last.getOrElse(_, List())).flatten
          addEdges(lastIds.map(x => DirectedEdge(id, x)))

          signals.foldLeft(last)(_.updated(_, Set(id))) // overwrite `signals` last connection id
        }

        statement match {
          // No update last:
          case _: Assert | _: CApply | _: STuple | _: SAssign =>
            vertexs += id
            last
          // Update last:
          case _: Connect | _: SubModuleRun | _: SApply =>
            vertexs += id
            updatedLast(last, id, statement.relatedIdents.fully)
          case _: MDef =>
            vertexs += id
            updatedLast(
              last,
              id,
              if (isModuleTop)
                statement.relatedIdents.fully
                  .map(moduleName + ".this." + _)
              else
                statement.relatedIdents.fully
            )

          // Breakable blocks:
          // unbreak block
          case _: SIf | _: SBlock | _: When | _: Switch if ChicalaConfig.unbreakBlocks =>
            vertexs += id
            updatedLast(last, id, statement.relatedIdents.fully ++ statement.relatedIdents.partially)
          // break block
          case w: When =>
            val whenLast  = getVertexAndLastConnectDependcy(id :+ 1, w.whenp, last, false)
            val otherLast = getVertexAndLastConnectDependcy(id :+ 2, w.otherp, last, false)
            mergedTwoBranchLast(whenLast, otherLast)
          case switch: Switch =>
            id.asPrefixZipWith(
              switch.branchs
                .map(_._2)
            ).map({ case (subId, branchp) => getVertexAndLastConnectDependcy(subId, branchp, last, false) })
              .foldLeft(last)(mergedTwoBranchLast(_, _))
          case sIf: SIf =>
            val thenLast = getVertexAndLastConnectDependcy(id :+ 1, sIf.thenp, last, false)
            val elseLast = getVertexAndLastConnectDependcy(id :+ 2, sIf.elsep, last, false)
            mergedTwoBranchLast(thenLast, elseLast)
          case sBlock: SBlock =>
            getVertexAndLastConnectDependcyFromList(id, sBlock.body, last, false)

          // Other statements:
          case EmptyMTerm =>
            last
          case _ =>
            reporter.error(
              NoPosition,
              s"Not processed ${moduleName} statement in " +
                "ToplogicalSort.getDependencyGraph.getVertexAndLastConnectDependcy:\n" +
                s"  ${statement.toString()}"
            )
            last
        }
      }
      def getVertexAndLastConnectDependcyFromList(
          idPrefix: Id,
          statements: List[MStatement],
          lastConnect: Map[String, Set[Id]],
          isModuleTop: Boolean
      ): Map[String, Set[Id]] = {
        idPrefix
          .asPrefixZipWith(statements)
          .foldLeft(lastConnect)({ case (last, (id, statement)) =>
            getVertexAndLastConnectDependcy(id, statement, last, isModuleTop)
          })
      }

      def getConnectDependcy(
          id: Id,
          statement: MStatement,
          lastConnect: Map[String, Set[Id]],
          dependency: Set[String]
      ): Unit = {
        statement match {
          // Connect:
          case c: Connect =>
            c.relatedIdents.fully.foreach(left =>
              if (lastConnect(left).contains(id)) { // only valid connection
                addEdges(
                  (dependency ++ c.relatedIdents.dependency)
                    .map(lastConnect.getOrElse(_, Set.empty))
                    .flatten
                    .map(x => DirectedEdge(id, x)),
                  c
                )
              }
            )
          // Breakable blocks:
          // unbreak block
          case _: When | _: SIf | _: SBlock | _: Switch if ChicalaConfig.unbreakBlocks =>
            addEdges(
              (dependency ++ statement.relatedIdents.dependency)
                .map(lastConnect.getOrElse(_, Set.empty))
                .flatten
                .map(x => DirectedEdge(id, x)),
              statement
            )
          // break block
          case w: When =>
            val newDependency = dependency ++ w.cond.relatedIdents.dependency
            getConnectDependcy(id :+ 1, w.whenp, lastConnect, newDependency)
            getConnectDependcy(id :+ 2, w.otherp, lastConnect, newDependency)
          case sIf: SIf =>
            getConnectDependcy(id :+ 1, sIf.thenp, lastConnect, dependency)
            getConnectDependcy(id :+ 2, sIf.elsep, lastConnect, dependency)
          case switch: Switch =>
            /** dependency with `switch.cond`. `v` for each `branch` should be
              * `Lit` that has no dependency
              */
            val newDependency = dependency ++ switch.cond.relatedIdents.dependency
            id.asPrefixZipWith(
              switch.branchs
            ).foreach { case (subId, (v, branchp)) =>
              getConnectDependcy(subId, branchp, lastConnect, newDependency)
            }
          case sBlock: SBlock =>
            getConnectDependcyFromList(id, sBlock.body, lastConnect, dependency)

          // Other statements:
          case EmptyMTerm =>
          case s @ SApply(_, List(f: SFunction), _) => {
            val dependencys = dependency ++ s.relatedIdents.dependency
            val fullys      = s.relatedIdents.fully
            val intersect   = dependencys.intersect(fullys)
            if (!ChicalaConfig.disableNeedCheckWarn) {
              // add NEEDCHECK comment in pass AfterSort
              assertWarning(
                intersect.isEmpty,
                NoPosition,
                s"NEED CHECK: function has self dependency inside, Chicala cannot solve automatically:\n" +
                  s"  dependency from context: ${dependency}\n" +
                  s"  direct dependency: ${s.relatedIdents.dependency}\n" +
                  s"  fully connected: ${fullys}"
              )
            }
            addEdges(
              dependencys
                .map(lastConnect.getOrElse(_, Set.empty))
                .flatten
                .withFilter(x => x != id)
                .map(x => DirectedEdge(id, x)),
              s
            )
          }
          case s =>
            addEdges(
              (dependency ++ s.relatedIdents.dependency)
                .map(lastConnect.getOrElse(_, Set.empty))
                .flatten
                .map(x => DirectedEdge(id, x)),
              s
            )
        }
      }
      def getConnectDependcyFromList(
          idPrefix: Id,
          statements: List[MStatement],
          lastConnect: Map[String, Set[Id]],
          dependency: Set[String]
      ): Unit = {
        idPrefix
          .asPrefixZipWith(statements)
          .foreach { case (id, statement) =>
            getConnectDependcy(id, statement, lastConnect, dependency)
          }
      }

      case class Previous(updated: Map[String, Set[Id]], used: Map[String, Set[Id]])
      object Previous { def empty = Previous(Map.empty, Map.empty) }
      def mergedTwoBranchPrevious(previous1: Previous, previous2: Previous): Previous = {
        val updated = mergedMapSet(previous1.updated, previous2.updated)
        val used    = mergedMapSet(previous1.used, previous2.used)
        Previous(updated, used)
      }
      def getScalaValDependcy(
          id: Id,
          statement: MStatement,
          previous: Previous,
          used: Set[String]
      ): Previous = {
        def addDependcy(relatedIdents: RelatedIdents) = {
          addEdges(
            (
              // write after read
              relatedIdents.updated
                .map(previous.used.getOrElse(_, Set.empty)) ++
                // write after write
                relatedIdents.updated
                  .map(previous.updated.getOrElse(_, Set.empty)) ++
                // read after write
                relatedIdents.usedAll
                  .map(previous.updated.getOrElse(_, Set.empty))
            ).flatten
              .map(x => DirectedEdge(id, x))
          )
        }
        def updatePrevious(relatedIdents: RelatedIdents): Previous = {
          Previous(
            relatedIdents.updated
              .foldLeft(previous.updated)((p, n) => p.updated(n, Set(id))),
            relatedIdents.usedAll
              .foldLeft(previous.used)((p, n) => p.updated(n, p.getOrElse(n, Set.empty) ++ Set(id)))
          )
        }

        statement match {
          // Breakable blocks:
          // unbreak block
          case _: When | _: SIf | _: SBlock | _: Switch if ChicalaConfig.unbreakBlocks =>
            val relatedIdents = statement.relatedIdents ++ RelatedIdents.used(used)
            addDependcy(relatedIdents)
            updatePrevious(relatedIdents)
          // break block
          case sIf: SIf =>
            val ud           = used ++ sIf.cond.relatedIdents.usedAll
            val thenPrevious = getScalaValDependcy(id :+ 1, sIf.thenp, previous, ud)
            val elsePrevious = getScalaValDependcy(id :+ 2, sIf.elsep, previous, ud)
            mergedTwoBranchPrevious(thenPrevious, elsePrevious)
          case sBlock: SBlock =>
            getScalaValDependcyFromList(id, sBlock.body, previous, used)
          case when: When =>
            val ud           = used ++ when.cond.relatedIdents.usedAll
            val whenPrevious = getScalaValDependcy(id :+ 1, when.whenp, previous, ud)
            val elsePrevious = getScalaValDependcy(id :+ 2, when.otherp, whenPrevious, ud)
            elsePrevious

          // Other statements:
          case EmptyMTerm =>
            previous
          case x =>
            val relatedIdents = x.relatedIdents ++ RelatedIdents.used(used)
            addDependcy(relatedIdents)
            updatePrevious(relatedIdents)
        }
      }
      def getScalaValDependcyFromList(
          idPrefix: Id,
          statements: List[MStatement],
          previous: Previous,
          used: Set[String]
      ): Previous = {
        idPrefix
          .asPrefixZipWith(statements)
          .foldLeft(previous)({ case (pre, (id, statement)) =>
            getScalaValDependcy(id, statement, pre, used)
          })
      }

      val lastConnect = getVertexAndLastConnectDependcyFromList(Id.empty, body, Map.empty, isModuleTop)
      getConnectDependcyFromList(Id.empty, body, lastConnect, Set.empty)
      getScalaValDependcyFromList(Id.empty, body, Previous.empty, Set.empty)

      DirectedGraph(vertexs.toSet, edges.toSet)
    }

    def reorder(body: List[MStatement], topologicalOrder: List[Id]) = {

      def mergeId(idList: List[Id]): List[(Int, List[Id])] = {
        // merge adjacent IDs have same first-level index
        // return list of merged IDs, List((top, List(rest, ...)), ...)
        idList
          .foldLeft(List.empty[(Int, List[Id])]) { case (ls, id) =>
            ls match {
              case Nil => List((id.top, List(id.rest)))
              case (index, restList) :: next =>
                if (index == id.top)
                  (index, id.rest :: restList) :: next // reversed append 1
                else
                  (id.top, List(id.rest)) :: ls // reversed append 2
            }
          }
          .map(x => (x._1, x._2.reverse)) // reverse 1
          .reverse                        // reverse 2
      }
      def splitParts(
          mergeIds: List[(Int, List[Id])],
          maxPartIndex: Int
      ): List[List[(Int, List[Id])]] = {
        var parts     = List.empty[List[(Int, List[Id])]]
        var lastIndex = maxPartIndex
        mergeIds.foreach { x =>
          if (lastIndex >= x._1) parts = List(x) :: parts // reversed append #1
          else parts = (x :: parts.head) :: parts.tail    // reversed append #2
          lastIndex = x._1
        }
        parts.map(_.reverse).reverse // reverse #1 #2
      }

      def doReorderList(bodyList: List[MStatement], idList: List[Id]): List[MStatement] = {
        val body = bodyList.toArray
        mergeId(idList).map { case (index, restList) =>
          doReorder(body(index - 1), restList)
        }.flatten
      }
      def doReorder(mStatement: MStatement, restList: List[Id]): List[MStatement] = {
        restList match {
          case Nil | List(Id(Nil)) => List(mStatement)
          case _ =>
            mStatement match {
              case _: When | _: SIf | _: Switch | _: SBlock if ChicalaConfig.unbreakBlocks =>
                reporter.error(
                  NoPosition,
                  s"Should not break blocks, in ToplogicalSort.doReorder: ${mStatement}\n${restList}"
                )
                List()
              case w: When =>
                val merged = mergeId(restList)
                val parts  = splitParts(merged, 2)
                parts.map(_.toMap).map { mergedOne =>
                  val whenp =
                    if (mergedOne.contains(1)) doReorder(w.whenp, mergedOne(1)).head
                    else EmptyMTerm
                  val otherp =
                    if (mergedOne.contains(2)) doReorder(w.otherp, mergedOne(2)).head
                    else EmptyMTerm
                  val hasElseWhen =
                    if (w.hasElseWhen && otherp.nonEmpty) true
                    else false
                  When(w.cond, whenp, otherp, hasElseWhen)
                }
              case sIf: SIf =>
                val merged = mergeId(restList)
                val parts  = splitParts(merged, 2)

                parts.map(_.toMap).map { mergedOne =>
                  // FIXME: use MTerm, use SBlock wrap SDef
                  val thenp =
                    if (mergedOne.contains(1)) doReorder(sIf.thenp, mergedOne(1)).head
                    else EmptyMTerm
                  val elsep =
                    if (mergedOne.contains(2)) doReorder(sIf.elsep, mergedOne(2)).head
                    else EmptyMTerm
                  SIf(sIf.cond, thenp, elsep, sIf.tpe)
                }
              case switch: Switch =>
                val merged     = mergeId(restList)
                val parts      = splitParts(merged, switch.branchs.size)
                val branchsArr = switch.branchs.toIndexedSeq
                parts.map { ls =>
                  val branchs = ls.map { case (index, rest) =>
                    val body = doReorder(branchsArr(index - 1)._2, rest)
                    assert(body.size == 1, "should have only one statement") // FIXME
                    (branchsArr(index - 1)._1, body.head)
                  }
                  Switch(switch.cond, branchs)
                }

              case sBlock: SBlock =>
                doReorderList(sBlock.body, restList) match {
                  case Nil         => List(EmptyMTerm)
                  case head :: Nil => List(head)
                  case ls          => List(SBlock(ls, sBlock.tpe))
                }
              case s =>
                reporter.error(
                  NoPosition,
                  s"Not processed in ToplogicalSort.doReorder: ${s}\n${restList}"
                )
                List()
            }
        }
      }

      doReorderList(body, topologicalOrder)
    }

    class ReorderSubFieldTransFormer(implicit moduleName: String) extends Transformer {
      def reorderTopSBlockOrOther(bodyp: MStatement): MStatement = {
        bodyp match {
          case SBlock(body, tpe) => SBlock(reorderTopList(body, false), tpe)
          case x                 => transform(x)
        }
      }
      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case s @ SDefDef(_, _, _, defp) =>
          s.copy(defp = reorderTopSBlockOrOther(defp))
        case f @ SFunction(_, funcp) =>
          f.copy(funcp = reorderTopSBlockOrOther(funcp).asInstanceOf[MTerm])
        case i @ SIf(cond, thenp, elsep, tpe) if ChicalaConfig.unbreakBlocks =>
          SIf(cond, reorderTopSBlockOrOther(thenp), reorderTopSBlockOrOther(elsep), tpe)
        case b @ SBlock(body, tpe) if ChicalaConfig.unbreakBlocks =>
          SBlock(reorderTopList(body, false), tpe)
        case w @ When(cond, whenp, otherp, hasElseWhen) if ChicalaConfig.unbreakBlocks =>
          When(cond, reorderTopSBlockOrOther(whenp), reorderTopSBlockOrOther(otherp), hasElseWhen)
        case s @ Switch(cond, branchs) if ChicalaConfig.unbreakBlocks =>
          val newBranchs = branchs.map { case (v, body) =>
            (v, reorderTopSBlockOrOther(body))
          }
          Switch(cond, newBranchs)

        case x => super.transform(x)
      }
    }

    def reorderTopList(body: List[MStatement], isModuleTop: Boolean)(implicit
        moduleName: String
    ): List[MStatement] = {
      val reorderSubField       = new ReorderSubFieldTransFormer
      val newBody               = body.map(reorderSubField(_))
      val dependencyGraph       = getDependencyGraph(newBody, isModuleTop)
      val rightTopologicalOrder = dependencyGraph.toplogicalSortWithCycle()
      if (!ChicalaConfig.disableNeedCheckWarn) {
        assertWarning(
          rightTopologicalOrder.isRight,
          NoPosition,
          s"""NEED CHECK: topological sort has cycle, maybe coused by Vec
           |  topological order: ${rightTopologicalOrder.merge.map(_.toPointString)}""".stripMargin
        )
      }

      rightTopologicalOrder match {
        case Right(order) => reorder(newBody, order)
        case Left(order) =>
          Comment("chicala[NEEDCHECK]: topological sort has cycle, maybe coused by Vec") :: reorder(newBody, order)
      }
    }

    def dependencySort(moduleDef: ModuleDef): ModuleDef = {
      moduleDef.copy(body = reorderTopList(moduleDef.body, true)(moduleDef.name.toString()))
    }
  }
}

case class Id(val seq: List[Int]) extends Ordered[Id] {
  def compare(that: Id): Int = compareSeq(this.seq, that.seq)

  private def compareSeq(seqA: List[Int], seqB: List[Int]): Int = {
    (seqA, seqB) match {
      case (Nil, Nil) => 0
      case (_, Nil)   => 1
      case (Nil, _)   => -1
      case (_, _) =>
        if (seqA.head == seqB.head) compareSeq(seqA.tail, seqB.tail)
        else seqA.head compare seqB.head
    }
  }

  override def toString(): String = {
    if (seq.isEmpty) "Id()"
    else s"Id(${seq.map(_.toString()).reduce(_ + ", " + _)})"
  }

  def toPointString: String = {
    if (seq.isEmpty) ""
    else seq.map(_.toString()).reduce(_ + "." + _)
  }
  def toNameString: String = {
    s"p${toPointString.replace(".", "x")}"
  }

  def asPrefixZipWith[T](seq: Seq[T]): Seq[(Id, T)] = {
    seq.zipWithIndex.map({ case (t, i) => (this :+ (i + 1), t) })
  }

  def :+(number: Int): Id = Id(seq :+ number)

  def top: Int = seq.head
  def rest: Id = Id(seq.tail)
}
object Id {
  def empty = Id(List.empty)
}
case class DirectedEdge(val from: Id, val to: Id) extends Ordered[DirectedEdge] {
  def compare(that: DirectedEdge): Int = {
    val fromCompare = from.compare(that.from)
    if (fromCompare == 0) to.compare(that.to)
    else fromCompare
  }
  override def toString(): String = {
    s"(${from.toPointString} -> ${to.toPointString})"
  }
}
case class DirectedGraph(val vertexs: Set[Id], edges: Set[DirectedEdge]) {

  override def toString(): String = {
    val vertexsSetName = if (vertexs.size <= 4) "Set" else "HashSet"
    val vertexsList    = vertexs.toList.sorted.map(_.toString()).reduce(_ + ", " + _)
    val edgesSetName   = if (edges.size <= 4) "Set" else "HashSet"
    val edgesList      = edges.toList.sorted.map(_.toString()).reduce(_ + ", " + _)
    s"DirectedGraph($vertexsSetName($vertexsList),$edgesSetName($edgesList))"
  }

  def toDot: String = {
    val nodes = vertexs.map(x => s"${x.toNameString} [label=\"${x.toPointString}\"]")
    val diedges = edges.map { case DirectedEdge(from, to) =>
      val fromNode = from.toNameString
      val toNode   = to.toNameString
      s"${fromNode}->${toNode}"
    }

    s"""digraph g{
      |${nodes.map(x => "  " + x + "\n").foldLeft("")(_ + _)}
      |${diedges.map(x => "  " + x + "\n").foldLeft("")(_ + _)}
      |}""".stripMargin
  }

  /** Edge from `from` to `to`, `to` need to be sorted before `from`
    */
  def toplogicalSort(layer: Boolean = false): (List[Id], List[Id]) = {
    import scala.collection.mutable

    val incoming        = mutable.Map.from(vertexs.map(_ -> mutable.Set.empty[Id]))
    val dependencyCount = mutable.Map.from(vertexs.map(_ -> 0))
    edges.foreach { case DirectedEdge(from, to) =>
      incoming(to) += from
      dependencyCount(from) += 1
    }

    val queue   = mutable.PriorityQueue.from(vertexs.filter(dependencyCount(_) == 0)).reverse
    var revList = List.empty[Id] // store reversed toplogical order
    while (queue.nonEmpty) {
      val thisLayer = if (layer) queue.dequeueAll else List(queue.dequeue())
      thisLayer.foreach { v =>
        revList = v :: revList
        incoming(v).foreach { u =>
          dependencyCount(u) -= 1
          if (dependencyCount(u) == 0)
            queue += u
        }
      }
    }

    (revList.reverse, (vertexs -- revList.toSet).toList.sorted)
  }

  /** Edge from `from` to `to`, `to` need to be sorted before `from`. If there
    * is a cycle, place the vertex with the minimal id at the end, then sort the
    * rest.
    *
    * @return
    *   Right: no cycle, Left: with cycle
    */
  def toplogicalSortWithCycle(): Either[List[Id], List[Id]] = {
    import scala.collection.mutable

    val incoming        = mutable.Map.from(vertexs.map(_ -> mutable.Set.empty[Id]))
    val dependencyCount = mutable.Map.from(vertexs.map(_ -> 0))
    edges.foreach { case DirectedEdge(from, to) =>
      incoming(to) += from
      dependencyCount(from) += 1
    }
    val rest = mutable.TreeSet.from(vertexs)

    var revList  = List.empty[Id] // store reversed toplogical order
    val queue    = mutable.PriorityQueue.from(vertexs.filter(dependencyCount(_) == 0)).reverse
    var hasCycle = false
    while (queue.nonEmpty || rest.nonEmpty) {
      val v =
        if (queue.nonEmpty) queue.dequeue()
        else {
          hasCycle = true
          rest.head
        }
      revList = v :: revList
      rest.remove(v)

      incoming(v).foreach { u =>
        dependencyCount(u) -= 1
        if (dependencyCount(u) == 0 && rest.contains(u))
          queue += u
      }
    }

    if (hasCycle) Left(revList.reverse)
    else Right(revList.reverse)
  }
}
object DirectedGraph {
  def empty = DirectedGraph(Set.empty, Set.empty)
}
