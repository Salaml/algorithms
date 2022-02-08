abstract sealed class BST {
  def find(x: Int): Boolean
  def insert(x: Int): BST
  def remove(x: Int): BST
  def min: Int
}

case object Empty extends BST {
  override def find(x: Int): Boolean = false
  override def insert(x: Int): BST = Node(x, Empty, Empty)
  override def remove(x: Int): BST = throw new IllegalArgumentException
  override def min: Int = throw new IllegalArgumentException
}

case class Node(v: Int, l: BST, r: BST) extends BST {
  override def find(x: Int): Boolean = {
    if (x == v) true
    else if (x < v) l.find(x)
    else r.find(x)
  }

  override def insert(x: Int): BST = {
    if (x <= v)
      Node(v, l.insert(x), r)
    else
      Node(v, l, r.insert(x))
  }

  override def remove(x: Int): BST = {
    if (x < v)
      Node(v, l.remove(x), r)
    else if (x > v)
      Node(v, l, r.remove(x))
    else { // x == v
      this match {
        case Node(_, left, Empty) => left
        case Node(_, Empty, right) => right
        case Node(_, left, right) => {
          // replace node with smallest value of right subtree
          val min_right = right.min
          Node(min_right, left, right.remove(min_right))
        }
      }
    }
  }

  override def min: Int = {
    this match {
      // smallest value is always in left subtree
      // or in the node itself if not existent
      case Node(value, Empty, _) => value
      case Node(_, left, _) => left.min
    }
  }
}

val t0 = Empty.insert(4).insert(3).insert(5)
val t1 = t0.insert(7).insert(1).insert(4)
t1.find(1)
t1.min

val t2 = t1.remove(1)
t2.find(1)
t2.min

val t3 = t2.remove(5).insert(5)
t3.find(5)

val t4 = t3.remove(3)
t4.find(3)
t4.min
t4.remove(4)
