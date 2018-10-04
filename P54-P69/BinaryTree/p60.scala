package binarytree

object HeightBalanceTree {
    private val minHbalnodesStream = Stream.from(1).map(minHbalNodes)
    def minHbalNodes(height: Int): Int = {
        if(height == 1 || height == 0)
            height
        else
            minHbalNodes(height - 1) + minHbalNodes(height - 2) + 1
    }


    def maxHbalNodes(height: Int): Int = 1 << (height - 1) + 1

    def minHbalheight(nodes: Int): Int =
        if(nodes == 0)
            0
        else
            1 + minHbalheight(nodes / 2)

    def maxHbalHeight(nodes: Int): Int =
        minHbalnodesStream.indexWhere(_ > nodes)

    def main(args: Array[String]): Unit = {
        Console println minHbalNodes(3)
        Console println minHbalNodes(1)
        Console println minHbalNodes(2)
        Console println minHbalNodes(4)

        Console println minHbalheight(6)

        Console println maxHbalNodes(3)

        Console println maxHbalHeight(1)
        Console println maxHbalHeight(2)
        Console println maxHbalHeight(3)
        Console println maxHbalHeight(12)
    }
}
