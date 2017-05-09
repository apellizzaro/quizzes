
import scala.collection._
import org.json4s.{DefaultFormats, _}
import org.json4s.jackson.JsonMethods
import org.json4s.jackson.Serialization.{write, writePretty}


//these are the model for http://interview.euclidanalytics.com/
case class RouterInnerInfo (ss:Int,ct:Int,s3:Int,ot:Int, si:String, s2:Int, ap:Int,sm:String,sl:Int, sh:Int)
case class InfoRouter (pf:Int, sn:String, vs:Int,sq:Int,ht:List[RouterInnerInfo])

object Json {
  implicit val formats = (DefaultFormats + NoTypeHints) ++ org.json4s.ext.JodaTimeSerializers.all
  def parse[A: Manifest](input: String): A = JsonMethods.parse(input).extract[A](implicitly, implicitly[Manifest[A]])
}

//processing for http://interview.euclidanalytics.com/
class RouterInfoProcessor {

  //for simplicity we'll start assuming that the sream of data comes in as an array of string, each string is a line
  def readStream(inStream: Array[String]): Map[String, Array[InfoRouter]] = {

    //take only the warn and info
    val infoStream = inStream.filter(s => s.startsWith("INFO:"))
    val warnStream = inStream.filter(s => s.startsWith("WARN:"))

    //parse the Json information
    val listOfInfo = infoStream.map(s => Json.parse[InfoRouter](s))
    val listOfWarn = warnStream.map(s => Json.parse[InfoRouter](s))

    Map("INFO" -> listOfInfo, "WARN" -> listOfWarn)
  }


  def printSolutions (info: Map[String, Array[InfoRouter] ]):Unit = {

    //how many info
    println("Number of Info = " + info.get("INFO").map(_.length).getOrElse(0) )

    //how many Warn
    println("Number of warn = " + info.get("WARN").map(_.length).getOrElse(0) )

    //unique sn values
    val snValues = info.flatMap (kv => kv._2.map(_.sn)).toList.groupBy(s=>s).keys.toList.length
    println ("Unique sn values:" + snValues)
    //uniques si values
    val siValues = info.flatMap(kv=>kv._2.flatMap(ri=>ri.ht.map(_.si))).toList.groupBy(s=>s).keys.toList.length
    println ("Unique si values:" + siValues)

    //max value of ss
    //get all the ss that are less or equal to 0
    val ssValues = info.flatMap(kv=>kv._2.flatMap(ri=>ri.ht.map(_.ss))).toList.filter(s=>s<=0)
    val maxSSValue = ssValues.sortBy(s=>s).head

    println(maxSSValue)
  }
}


//Tree/graph definition... various implementations
sealed trait Tree {
  def left:Tree = NilNode
  def right:Tree = NilNode
  def value:Int = 0
}

case object NilNode extends Tree
case class Node (leftNode:Tree,rightNode:Tree, nodeValue:Int) extends Tree {
  override def left: Tree = leftNode
  override def right:Tree = rightNode

  override def value: Int = nodeValue

  override def toString: String = nodeValue.toString
}

case class  Graph [T] (value: T, children: Seq[Graph[T]])

case class BinaryTreeNode (right:BinaryTreeNode, left: BinaryTreeNode, value: Int)

//This models that 'name' depends on the 'dependencies' list of string.
//problem: given an array of Item, print the name only if all its dependencies have been printed
case class Item (name:String, dependencies: List[String])


//this model the following problem:
//I have a list of string, each string represent an id, name, department of employess
val employee = Array[String]("1,john,engineering",
  "2,pippo,engineering",
  "3,pluto,sales",
  "4,minnie,marketing",
  "5,qui,marketing",
  "6,qui,marketing",
  "7,quo,sales")

//this represent the friendship of the employes: 1 is frined with 2, 2 is friend with 3 etc.
val friendships= Array[String] ("1,2","2,3","1,6","4,7")

case class Emp (id:Int, name:String, dep:String)
case class Rel (from:Int, to:Int)
case class NumOutRel(dep:String, numberOfRel:Int)
//problem: print how many friends an employee has that are not in the employee's department
//Implementation to do!

class InterviewTests {


  //Print the items inorder, respecting the dependencies
  def printInOrder(items: List[Item]): Unit = {

    val mapOfItems = items.map(i => (i.name, i)).toMap

    //returns the list of already printed items
    def printIfDepPrinted(item: Option[Item], alreadyPrinted: List[String]): List[String] = {
      item.map(ii =>
        if (ii.dependencies.forall(s => alreadyPrinted.contains(s))) {
          println(ii.name)
          ii.name :: alreadyPrinted
        }
        else
          printIfDepPrinted(mapOfItems.get(ii.dependencies.filterNot(dp => alreadyPrinted.contains(dp)).head), alreadyPrinted)
      ).getOrElse(alreadyPrinted)
    }

    def printOrderWithAlreadyPrinted(items2: List[Item], alreadyPrinted2: List[String]): Unit = {
      if (items2.nonEmpty) {
        val printed = printIfDepPrinted(Some(items2.head), alreadyPrinted2)
        val remains = items2.filterNot(it => printed.contains(it.name))
        printOrderWithAlreadyPrinted(remains, printed)
      }
    }

    printOrderWithAlreadyPrinted(items, List())

  }

  //given a string, returns all its permutation
  //abc => abc, acb, bac, bca,  cab, cba ....
  def buildPermTree(in: String): List[String] = {
    case class StringTuple(left: String, right: String)

    def buildChildren(strings: StringTuple): List[Graph[StringTuple]] = {
      if (strings.right.isEmpty)
        List()
      else {
        strings.right.map(c => {
          val newTuple = StringTuple(strings.left + c,
            strings.right.filterNot(_ == c))
          Graph[StringTuple](newTuple, buildChildren(newTuple))
        }).toList
      }
    }

    def getLeaves(g: Graph[StringTuple]): List[StringTuple] = {
      if (g.children.isEmpty)
        List(g.value)
      else
        g.children.map(c => getLeaves(c)).fold[List[StringTuple]](List())((a, b) => a ::: b)
    }

    val permTree = Graph(StringTuple("", in), buildChildren(StringTuple("", in)))

    getLeaves(permTree).map(_.left)

  }

  //given a binary tree, fidn the index-th element
  //and applies the function dosomething
  def findKthElem(tree: BinaryTreeNode, index: Int, doSomehting: Int => Unit): Unit = {

    def findK(tree: BinaryTreeNode, currentIndex: Int): Int = {
      if (tree == null)
        currentIndex - 1
      else if (currentIndex > index)
        currentIndex
      else if (currentIndex == index) {
        doSomehting(tree.value)
        index + 1
      }
      else {
        val newInsdex = findK(tree.left, currentIndex + 1)
        findK(tree.right, newInsdex + 1)
      }
    }

    findK(tree, 0)
  }

  //given a generic list, eliminates the adjiacent duplicates
  //(a,b,b,c,b,g,g,g,r,t,s,a,a) => (a,b,c,b,g,r,t,s,a)
  def eliminateDups[T](s: List[T]): List[T] = {
    s match {
      case Nil => Nil
      case h :: Nil => s
      case h :: t if h == t.head => eliminateDups(t)
      case h :: t => h :: eliminateDups(t)
    }
  }

  //traverse a graph breadthfirst
  def breadthFirst[T](g: Graph[T], doSomething: T => Unit): Unit = {

    def bdfs(nodes: List[Graph[T]]): Unit = {
      val current = nodes.head
      current.children.foreach(c => doSomething(c.value))

      bdfs(nodes.tail ++ current.children)
    }

    doSomething(g.value)
    bdfs(List(g))
  }

  //returns true if the parenthesis ( { [ matches correctly
  //and in the correct order
  def checkParenthesis2(inString: String): Boolean = {

    def check2(s: String, stack: List[Char]): Boolean = {
      if (s.isEmpty)
        true
      else if (s.head == '{' || s.head == '(' || s.head == '[')
        check2(s.tail, s.head :: stack)
      else if ((s.head == '}' && stack.head == '{') ||
        (s.head == ')' && stack.head == '(') ||
        (s.head == ']' && stack.head == '['))
        check2(s.tail, stack.tail)
      else if (s.head == '}' || s.head == ')' || s.head == ']') {
        false
      }
      else {
        check2(s.tail, stack)
      }
    }

    check2(inString, List())
  }

  //check if a string is the anagram of the other string
  def isAnagram(s1: String, s2: String): Boolean = {
    if (s1.length != s2.length)
      false
    else {
      val map1 = s1.groupBy(c => c).mapValues(_.length)
      val map2 = s2.groupBy(c => c).mapValues(_.length)

      map1.map(kv => map2.get(kv._1).contains(kv._2)).toList.forall(b => b)
    }
  }


  object Solution {

    def main(args: Array[String]): Unit = {

      val test = "abcd"
      val interview = new InterviewTests

      val newList = interview.eliminateDups(List(1, 2, 3, 3, 4, 5, 5, 5, 5, 6, 7, 8, 8))

      newList.foreach { i =>
        print(i)
        print("-")
      }
      println()


      //                   10
      //              /        \
      //             /          \
      //            /            \
      //           /              \
      //          2                1
      //        /    \           /  \
      //     6         3        4     3
      //    /         /        /      /
      //   9      10         8       7
      //  /      / \
      //14      13  15
      //              \
      //              16

      val myTree = BinaryTreeNode(
        BinaryTreeNode(
          BinaryTreeNode(null,
            BinaryTreeNode(null, null, 7), 3),
          BinaryTreeNode(null,
            BinaryTreeNode(null, null, 8), 4), 1),
        BinaryTreeNode(
          BinaryTreeNode(null,
            BinaryTreeNode(
              BinaryTreeNode(
                BinaryTreeNode(null, null, 16), null, 15),
              BinaryTreeNode(null, null, 13), 10), 3),
          BinaryTreeNode(null,
            BinaryTreeNode(null,
              BinaryTreeNode(null, null, 14), 9), 6), 2), 10
      )

      interview.findKthElem(myTree, 1, x => println(x.toString))
      interview.findKthElem(myTree, 2, x => println(x.toString))
      interview.findKthElem(myTree, 3, x => println(x.toString))
      interview.findKthElem(myTree, 4, x => println(x.toString))
      interview.findKthElem(myTree, 5, x => println(x.toString))
      interview.findKthElem(myTree, 6, x => println(x.toString))
      interview.findKthElem(myTree, 7, x => println(x.toString))

      /* interview.findIthElem(myTree,0,0,x=>println(x.toString))
    interview.findIthElem(myTree,1,0,x=>println(x.toString))
    interview.findIthElem(myTree,2,0,x=>println(x.toString))
    interview.findIthElem(myTree,3,0,x=>println(x.toString))
    interview.findIthElem(myTree,4,0,x=>println(x.toString))
    interview.findIthElem(myTree,5,0,x=>println(x.toString))
    interview.findIthElem(myTree,6,0,x=>println(x.toString))
    interview.findIthElem(myTree,7,0,x=>println(x.toString))
    interview.findIthElem(myTree,8,0,x=>println(x.toString))
    */

      println("print in order")
      val itemstoPrint = List(Item("apple", List("orange", "banana")),
        Item("orange", List("mango")),
        Item("mango", List()),
        Item("banana", List("lemon")),
        Item("lemon", List()))

      interview.printInOrder(itemstoPrint)

      println("BFS:")
      val bfsTree = Graph[Int](8, List(Graph[Int](2,
        List(Graph[Int](4, List()),
          Graph[Int](5, List()))),
        Graph[Int](6, List(
          Graph[Int](7, List()),
          Graph[Int](8, List(Graph[Int](9, List()))
          )))))

      interview.breadthFirst[Int](bfsTree, (x: Int) => println(x.toString))

      println()
      //true
      println(interview.checkParenthesis2("[dddfd{ffdddsfd(adfasd)(asda)asda}adaf]"))

      //false
      println(interview.checkParenthesis2("[dddfd{ffdd}dsfd(adfasd)(asda)asda}adaf]"))


      val treePerm = interview.buildPermTree(test)
      treePerm.foreach(s => print(s + "; "))
      println()
      /*
    val rootBsd = BinaryNode(5,
      BinaryNode(3,
        BinaryNode(1,null,null),BinaryNode(4,null,null)),BinaryNode(8,BinaryNode(6,null,null),BinaryNode(9,null,null)))

    interview.findKth(rootBsd,1)
    interview.findKth(rootBsd,2)
    interview.findKth(rootBsd,4)
    interview.findKth(rootBsd,7)
    interview.findKth(rootBsd,6)*/

    }

  }

}



















































/*  val hashImpl = new hashMap

 hashImpl.set ("foo1", "bar1")

 var value:Option[String] = hashImpl.get("foo1")
 System.out.println("Key foo1: " + value.getOrElse("Not Found!!!"))

 hashImpl.set("foo2", "bar2")
 value = hashImpl.get("foo2")
 System.out.println("Key foo2: " + value.getOrElse("Not Found!!!"))

 hashImpl.set("foo3", "bar3")
 value = hashImpl.get("foo3")
 System.out.println("Key foo3: " + value.getOrElse("Not Found!!!"))

 hashImpl.set("foo1", "bar4")
 value = hashImpl.get("foo1")
 System.out.println("Key foo1: " + value.getOrElse("Not Found!!!"))*/
  /*
    case class Element (key: String, value: String)

    trait IHashMap {
      def set (k: String, v:String)
      def get (k:String) : Option[String]
    }

    case class hashedList (hash:String, var elemts: scala.collection.mutable.MutableList[Element])

    class hashMap extends IHashMap {

      var actualmap : scala.collection.mutable.MutableList[hashedList] = scala.collection.mutable.MutableList()

      def get (k:String): Option [String] = {

         actualmap.find ( e=> e.hash==k.hashCode().toString).flatMap ( l => l.elemts.find ( e=>e.key==k)).map(_.value)
      }

      def set (k:String,v:String) = {

        actualmap.find ( h=> h.hash==k.hashCode().toString) match {
          case Some (hash) =>  {
            hash.elemts.find(e=>e.key==k) match {
              case None => hash.elemts += Element(k,v)
              case Some (el) => hash.elemts = hash.elemts.filter (item => item.key!=k)
                hash.elemts += Element(k,v)
            }
          }
          case None => actualmap += hashedList (k.hashCode.toString,    scala.collection.mutable.MutableList (Element(k,v)))
        }

      }
    }*/



    /*

    val piipo :Map[String,String]= Map()

    val strings = Array ("a\t0","b\t111","c\t20")

    val res = strings.map(_.split('\t'))
    val arrayofTuple = res.map(a=>(a(1),a(0) ))

    val mapEncodings = arrayofTuple.toList.toMap


    val encodedstring: String="201110"

    var codeIndex=0
    var currentCodeIdx:Int=0
    val bld:StringBuilder = new StringBuilder()

    while (codeIndex+currentCodeIdx<=encodedstring.length) {
      val currentCode=encodedstring.substring(codeIndex,codeIndex+currentCodeIdx)
      if (mapEncodings.keys.exists(_==currentCode)) {
        val charCoded = mapEncodings.get(currentCode).map (s=>
          if (s=="[newline]") {
            "\n"
          }
          else {
            s
          }
        )
        bld.append(charCoded.get)
        codeIndex+=currentCodeIdx
        currentCodeIdx=0

      }
      else
        currentCodeIdx+=1
    }

    val finalres=bld.mkString

    println(finalres)

*/

