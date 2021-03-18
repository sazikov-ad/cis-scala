package homeworks.homework2

object task4 extends App {

  sealed trait BinaryTree[+A] {
    // Реализуйте методы insert и contains для бинарного дерева со значениями произвольного типа,
    // используя тайпкласс scala.Ordering.
    // Сигнатуры методов insert/contains или классов можно (и нужно) модифицировать
    // для работы с неявными параметрами
    def insert[B >: A](newValue: B)(implicit ordering: Ordering[B]): BinaryTree[B] = this match {
      case t@Branch(value, _, _) if ordering.equiv(value, newValue) => t
      case Branch(value, left, right) if ordering.lt(newValue, value) => Branch(value, left.insert(newValue), right)
      case Branch(value, left, right) if ordering.gt(newValue, value) => Branch(value, left, right.insert(newValue))
      case Leaf => Branch(newValue, Leaf, Leaf)
    }

    def contains[B >: A](newValue: B)(implicit ordering: Ordering[B]): Boolean = this match {
      case Branch(value, _, _) if ordering.equiv(value, newValue) => true
      case Branch(value, left, _) if ordering.lt(newValue, value) => left.contains(newValue)
      case Branch(value, _, right) if ordering.gt(newValue, value) => right.contains(newValue)
      case _ => false
    }

    def dump(depth: Int = 0): Unit =
      this match {
        case Branch(value, left, right) =>
          right.dump(depth + 1)
          println(" " * depth + value)
          left.dump(depth + 1)
        case _ =>
      }
  }

  final case class Branch[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  case object Leaf extends BinaryTree[Nothing]

  Leaf
    .insert("DEF")
    .insert("ABC")
    .insert("IJK")
    .insert("FGH")
    .insert("LMN")
    .dump()

  //   LMN
  //  IJK
  //   FGH
  // DEF
  //  ABC
}
