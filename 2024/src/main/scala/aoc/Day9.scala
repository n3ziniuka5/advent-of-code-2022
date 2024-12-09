package aoc

import aoc.Common.timed

object Day9:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 9)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val blocks = lines.head.zipWithIndex.filter(_._2 % 2 == 0).unzip._1.map(_.toString.toInt).zipWithIndex.toVector
        val freeSpaces = lines.head.zipWithIndex.filter(_._2 % 2 == 1).unzip._1.map(_.toString.toInt).toList

        def process(
            blocks: Vector[(Int, Int)],
            freeSpaces: List[Int],
            processFreeSpace: Boolean,
            index: Int,
            result: Long
        ): Long =
            if processFreeSpace then
                blocks.lastOption match
                    case Some((blockSize, blockId)) =>
                        freeSpaces.headOption match
                            case None => process(blocks, freeSpaces, false, index, result)
                            case Some(freeSize) =>
                                if freeSize == 0 then process(blocks, freeSpaces.tail, false, index, result)
                                else if freeSize < blockSize then
                                    /* println(
                                      s"Free space $freeSize is less than block size $blockSize for block $blockId"
                                    ) */
                                    val lastBlockNewSize = blockSize - freeSize
                                    val newFreeSpaces    = 0 +: freeSpaces.tail
                                    val newBlocks =
                                        (freeSize, blockId) +: blocks.init :+ (lastBlockNewSize, blockId)

                                    process(newBlocks, newFreeSpaces, false, index, result)
                                else if freeSize >= blockSize then // TODO maybe wrong
                                    /* println(
                                      s"Free space $freeSize is greater than block size $blockSize for block $blockId"
                                    ) */
                                    val newFreeSpaceSize = freeSize - blockSize
                                    val newFreeSpaces    = newFreeSpaceSize +: freeSpaces.tail
                                    val newBlocks        = (blockSize, blockId) +: blocks.init
                                    process(newBlocks, newFreeSpaces, false, index, result)
                                else
                                    val newFreeSpaces = freeSpaces.tail
                                    val newBlocks     = (blockSize, blockId) +: blocks.init
                                    process(newBlocks, newFreeSpaces, false, index, result)

                    case None => process(blocks, freeSpaces, false, index, result)
            else
                blocks.headOption match
                    case Some((size, blockId)) =>
                        // println(s"Processing block $blockId with size $size at index $index")
                        val sum = (index until index + size).map(i => i.toLong * blockId).sum
                        // println(s"adding $sum to result $result")
                        process(blocks.tail, freeSpaces, true, index + size, result + sum)
                    case None => result

        println(blocks)
        println(freeSpaces)
        println(lines.head.size)

        process(blocks, freeSpaces, false, 0, 0L)

    sealed trait Block
    case class File(size: Int, id: Int) extends Block
    case class FreeSpace(size: Int)     extends Block

    def part2(lines: List[String]): Long =
        // val blocks = lines.head.zipWithIndex.filter(_._2 % 2 == 0).unzip._1.map(_.toString.toInt).zipWithIndex.toVector
        // val freeSpaces = lines.head.zipWithIndex.filter(_._2 % 2 == 1).unzip._1.map(_.toString.toInt).toList

        def initialBlocks(
            remaining: List[Char],
            result: Vector[Block],
            blockIndex: Int,
            isBlock: Boolean
        ): Vector[Block] =
            remaining match
                case head :: tail =>
                    val size = head.toString.toInt
                    if isBlock then initialBlocks(tail, result :+ File(size, blockIndex), blockIndex + 1, false)
                    else if size > 0 then initialBlocks(tail, result :+ FreeSpace(size), blockIndex, true)
                    else initialBlocks(tail, result, blockIndex, true)
                case Nil => result

        val blocks = initialBlocks(lines.head.toList, Vector.empty, 0, true)

        def arrangeBlocks(blocks: Vector[Block], moveBlockIndex: Int): Vector[Block] =
            if moveBlockIndex < 0 then blocks
            else
                blocks(moveBlockIndex) match
                    case File(size, id) =>
                        // println(s"Rearranging block $id with size $size at index $movePageIndex")
                        val maybeSuitableSpace = (0 until moveBlockIndex).find: i =>
                            blocks(i) match
                                case FreeSpace(freeSpaceSize) => freeSpaceSize >= size
                                case _                        => false
                        maybeSuitableSpace match
                            case Some(suitableSpaceIndex) =>
                                val freeSpace   = blocks(suitableSpaceIndex).asInstanceOf[FreeSpace]
                                val removedPage = blocks.patch(moveBlockIndex, List(FreeSpace(size)), 1)

                                if freeSpace.size > size then
                                    val newBlocks = removedPage.patch(
                                      suitableSpaceIndex,
                                      List(File(size, id), FreeSpace(freeSpace.size - size)),
                                      1
                                    )
                                    arrangeBlocks(newBlocks, moveBlockIndex)
                                else
                                    val newBlocks = removedPage.patch(
                                      suitableSpaceIndex,
                                      List(File(size, id)),
                                      1
                                    )
                                    arrangeBlocks(newBlocks, moveBlockIndex - 1)

                            case None => arrangeBlocks(blocks, moveBlockIndex - 1)
                    case FreeSpace(size) =>
                        arrangeBlocks(blocks, moveBlockIndex - 1)

        println(blocks)
        // println("after arranging")
        val rearanged = arrangeBlocks(blocks, blocks.size - 1)
        // println(rearanged)

        def checksum(blocks: Vector[Block], index: Int, sum: Long): Long =
            blocks.headOption match
                case Some(block) =>
                    block match
                        case File(size, id) =>
                            checksum(
                              blocks.tail,
                              index + size,
                              sum + (index until index + size).map(i => i.toLong * id).sum
                            )
                        case FreeSpace(size) => checksum(blocks.tail, index + size, sum)
                case None => sum

        checksum(rearanged, 0, 0L)
