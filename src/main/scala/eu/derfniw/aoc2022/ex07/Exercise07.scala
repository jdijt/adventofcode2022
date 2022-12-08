package eu.derfniw.aoc2022.ex07

import scala.io.Source

enum FSTreeNode:
  case Dir(name: String, children: Seq[FSTreeNode], totalSize: Int)
  case File(name: String, fileSize: Int)

  def size = this match
    case Dir(_, _, totalSize) => totalSize
    case File(_, fileSize)    => fileSize

object FSTreeNode:
  def fromCommandList(lines: Seq[String]): FSTreeNode =
    def helper(commands: Seq[String]): (FSTreeNode, Seq[String]) = commands match
      case s"$$ cd $currentDir" +: "$ ls" +: remainder =>
        val lsOutput = remainder.takeWhile(in => !in.startsWith("$"))
        val filesInDir = lsOutput.filterNot(_.startsWith("dir")).map { case s"$size $filename" =>
          File(filename, size.toInt)
        }
        val (childDirectories, remainingCommands) = lsOutput
          .filter(_.startsWith("dir"))
          .foldLeft((Seq[FSTreeNode](), remainder.drop(lsOutput.length))) {
            case ((currentChilds, cmds), s"dir $name") =>
              val (newChild, remCmds) = helper(cmds)
              (currentChilds :+ newChild, remCmds)
          }
        val allChildren = filesInDir ++ childDirectories
        (
          Dir(currentDir, allChildren, allChildren.map(_.size).sum),
          remainingCommands.dropWhile(_ == "$ cd ..")
        )

    helper(lines)._1
  end fromCommandList
end FSTreeNode

def run01(input: Source): Int =
  import FSTreeNode.*
  val fileTree = FSTreeNode.fromCommandList(input.getLines().toSeq)

  def dirsSmallerThan100000(node: FSTreeNode): Seq[Int] = node match
    case File(_, _) => Seq()
    case Dir(_, children, size) =>
      if size <= 100000 then children.flatMap(dirsSmallerThan100000) :+ size
      else children.flatMap(dirsSmallerThan100000)

  dirsSmallerThan100000(fileTree).sum
end run01

def run02(input: Source): Int =
  import FSTreeNode.*
  val fileTree       = FSTreeNode.fromCommandList(input.getLines().toSeq)
  val availableSpace = 70000000 - fileTree.size
  val spaceToBeFreed = 30000000 - availableSpace

  def findDirClosestToNeededSpace(node: FSTreeNode, currentCandidate: Int): Int = node match
    case File(_, _) => currentCandidate
    case Dir(_, children, size) =>
      val newCandidate =
        if size > spaceToBeFreed && size < currentCandidate then size else currentCandidate
      children.map(c => findDirClosestToNeededSpace(c, newCandidate)).min

  findDirClosestToNeededSpace(fileTree, fileTree.size)
end run02

@main
def exercise07() =
  def source = Source.fromResource("input_07")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
