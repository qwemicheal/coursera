package streams

import common._

/**
  * This component implements the solver for the Bloxorz game
  */
trait Solver extends GameDef {

  /**
    * Returns `true` if the block `b` is at the final position
    */
  def done(b: Block): Boolean = {
    b.b1 match {
      case this.goal=>{goal match {
        case b.b2 => true
        case _ => false
      }}
      case _ => false
    }


  }

  /**
    * This function takes two arguments: the current block `b` and
    * a list of moves `history` that was required to reach the
    * position of `b`.
    *
    * The `head` element of the `history` list is the latest move
    * that was executed, i.e. the last move that was performed for
    * the block to end up at position `b`.
    *
    * The function returns a stream of pairs: the first element of
    * the each pair is a neighboring block, and the second element
    * is the augmented history of moves required to reach this block.
    *
    * It should only return valid neighbors, i.e. block positions
    * that are inside the terrain.
    */
  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    //println("origin is "+b)
    val up=new Block(b.b1,b.b2).up
    //println("up is "+up)
    val down=new Block(b.b1,b.b2).down
    //println("down is "+down)
    val left=new Block(b.b1,b.b2).left
    //println("left is "+left)
    val right=new Block(b.b1,b.b2).right
    //println("right is "+right)
    Stream((up,Up::history),(down,Down::history),(left,Left::history),(right,Right::history)).filter(i=>i._1.isLegal)
  }

  /**
    * This function returns the list of neighbors without the block
    * positions that have already been explored. We will use it to
    * make sure that we don't explore circular paths.
    */
  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filter(i=>(!explored.contains(i._1)))
  }

  /**
    * The function `from` returns the stream of all possible paths
    * that can be followed, starting at the `head` of the `initial`
    * stream.
    *
    * The blocks in the stream `initial` are sorted by ascending path
    * length: the block positions with the shortest paths (length of
    * move list) are at the head of the stream.
    *
    * The parameter `explored` is a set of block positions that have
    * been visited before, on the path to any of the blocks in the
    * stream `initial`. When search reaches a block that has already
    * been explored before, that position should not be included a
    * second time to avoid cycles.
    *
    * The resulting stream should be sorted by ascending path length,
    * i.e. the block positions that can be reached with the fewest
    * amount of moves should appear first in the stream.
    *
    * Note: the solution should not look at or compare the lengths
    * of different paths - the implementation should naturally
    * construct the correctly sorted stream.
    */
  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    //println("initial   " + initial)
    if (initial.isEmpty) {
      //println("empty")
      Stream()
    }
    else
    {
      var next_explor=explored
      if (!next_explor.contains(initial.head._1))
        next_explor+=initial.head._1
      initial.flatMap(i => {
        if (done(i._1))
          Stream(i)
        //println("received expl is " + explored)
        else {
        val next: Stream[(Block, List[Move])] = newNeighborsOnly(neighborsWithHistory(i._1, i._2), next_explor)
        if (next.isEmpty)
          Stream(i)
        else from(next, next_explor)
      }}).toStream
    }
  }

  /**
    * The stream of all paths that begin at the starting block.
    */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
    //print(from(Stream((startBlock,List())),Set()))
    val b=startBlock
    from(Stream((b,List())),Set())
  }

  /**
    * Returns a stream of all possible pairs of the goal block along
    * with the history how it was reached.
    */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    //print(" re is "+pathsFromStart)
    pathsFromStart.filter(i=>{

      done(i._1)
    })
  }

  /**
    * The (or one of the) shortest sequence(s) of moves to reach the
    * goal. If the goal cannot be reached, the empty list is returned.
    *
    * Note: the `head` element of the returned list should represent
    * the first move that the player should perform from the starting
    * position.
    */
  lazy val solution: List[Move] = {
    val an=pathsToGoal
    if (an.isEmpty) List()
    else an.sortBy(i=>i._2.length).head._2.reverse

  }
}
