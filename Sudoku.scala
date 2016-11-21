import hw.sudoku._

object Solution extends SudokuLike {
	
	type T = Board

	val emptyBoard = new Board(0.to(80).toList.map(n=>(n/9, n%9) -> 1.to(9).toList).toMap)

	def parse(str: String): Board = {
		(recParse(str.toList, 0, emptyBoard)) // begin with empty board
	}

	def recParse(chars: List[Char], index: Int, board: Board): Board = chars match {
		case Nil => board
		case h :: t => {
			if(h == '.') recParse(t, index+1, board) // go to next character
			else recParse(t, index+1, board.place((index)/9, (index)%9, h.asDigit)) // place digit
		}
	}

	// produces the coordinates of all cells in the same row as r, same column as c, and same block as (r,c)
	def peers(row: Int, col: Int): List[(Int, Int)] = {
		val startBoxRow = (row/3)*3
		val startBoxCol = (col/3)*3
		val sameBox = startBoxRow.to(startBoxRow+2).toList.map {
			x => startBoxCol.to(startBoxCol+2).toList.map {
				y => (x,y)
			}
		}.flatten.filter(z => (z._1, z._2)!=(row,col))
		val sameRow = 0.to(8).toList.map{ c => (row, c)}.filter(x => x._2!=col)
		val sameCol = 0.to(8).toList.map{ r => (r, col)}.filter(x => x._1!=row)
		(sameRow ++ sameCol ++ sameBox).toSet.toList
		}	
}

	class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board] {
		def availableValuesAt(row: Int, col: Int): List[Int] = {
			available.getOrElse((row, col), List())
		}

		def valueAt(row: Int, col: Int): Option[Int] = {
			val avail = availableValuesAt(row, col)
			if(avail.size == 1 && avail.size!=0) Some(avail(0))
			else None
		}
		
		// returns true when there is one available value at every coordinate 
		def isSolved(): Boolean = {
			available.forall(x => x._2.size==1)
		}

		// returns false if theres exists a coordinate with no available values
		def isUnsolvable(): Boolean = {
			available.exists(x => x._2.size==0)
		}

		def place(row: Int, col: Int, value: Int): Board = {
			require(availableValuesAt(row, col).contains(value))
			val peerList = Solution.peers(row, col)
			val newBoard = new Board(available ++ placeHelp(peerList, value, this) + ((row, col) -> List(value)))
			newBoard.recUpdate
		}

		// filter out the value being placed from the list of avaiable values for all peers
		def placeHelp(list: List[(Int, Int)], value: Int, board: Board): Map[(Int, Int), List[Int]] = {
			list match {
				case Nil => board.available
				case head :: tail => {
					val values = board.availableValuesAt(head._1, head._2)
					val newValues = values.filter(x => x!=value)
					val newBoard = board.available + ((head._1, head._2) -> newValues)
					val newB = new Board(newBoard)
					placeHelp(tail, value, newB)
				}
			}
		}

		/* After placing a new value, we must check if the available values of any peers have been reduced down 
		   to one value. If this is the case, we must then "place" that value at the corresponding coordinate */
		def update(list: List[((Int, Int), List[Int])], board: Board): Map[(Int, Int), List[Int]] = {
			list match {
				case Nil => board.available
				case h :: t => {
					if(h._2.size == 1) {
						val newBoard = new Board(placeHelp(Solution.peers(h._1._1, h._1._2), h._2(0), board))
						update(t, newBoard)
					}
					else (update(t, board))
				}
			}
		}

		// recurively call update on the board until there are no more peers that need to be placed
		def recUpdate(): Board = {
			val newBoard = new Board(update(this.available.toList, this))
			if(newBoard.available.equals(this.available)) this
			else newBoard.recUpdate
		}

		// totalAvail counts the number of coordinates that do not have a value yet 
		def totalAvail(): Int = {
			available.foldLeft(0)((sum, x) => {
				if(x._2.size > 1) (sum + 1)
				else sum + 0 
			}
		)}

		/* returns the list of all boards that have exactly one additional value placed on the board. 
		   boards with fewer available occur earlier in the list */
		def nextStates(): List[Board] = {
			if(isUnsolvable) List()
			else {
				val nextBoards = available.flatMap{
					case(k,v) if(v.size>1) => v.map(x => place(k._1, k._2, x))
					case _ => Nil
				}.toList
				val nextBoardsSorted = nextBoards.sortBy(x => x.totalAvail)
				nextBoardsSorted
			}
		}

		// iterate through list of nextStates until we come across a board that is solved 
		def recSolve(list: List[Board]): Option[Board] = list match {
			case h :: t => {
				if(h.isSolved) Some(h)
				else {
					recSolve(t ++ h.nextStates)
				}
			}
			case Nil => None
		}
	
		def solve(): Option[Board] = {
			recSolve(List(this))
		}
}