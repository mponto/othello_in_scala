import scala.actors.Actor
import scala.actors.Actor._

case object Human
case object Machine
case object FieldManager
case object Map

class Human extends Actor {
  def act() {
    loop {
      receive {
	case "next" =>
          println("Your turn.")
          var intarr = readLine.split(",").map((s:String) => {s.toInt})
          var pos = Pair(intarr(0), intarr(1))
          sender ! (Human, pos)
	case "puterr" =>
          println("You can't put there.")
          var intarr = readLine.split(",").map((s:String) => {s.toInt})
          var pos = Pair(intarr(0), intarr(1))
          sender ! (Human, pos)
	case "end" =>
          println("Human died.")
          exit()
      }
    }
  }
}

class Machine extends Actor {
  def act() {
    loop {
      receive {
	case "next" =>
          println("Machine turn.")
          var intarr = readLine.split(",").map((s:String) => {s.toInt})
          var pos = Pair(intarr(0), intarr(1))
          sender ! (Machine, pos)

	case "puterr" =>
          println("You can't put there.")
          var intarr = readLine.split(",").map((s:String) => {s.toInt})
          var pos = Pair(intarr(0), intarr(1))
          sender ! (Machine, pos)

	case "end" =>
          println("Human died.")
          exit()
      }
    }
  }
}

class FieldManager(human:Actor, machine:Actor) extends Actor {
  def act() {
    var field = new Field()
    println("b/w ?")
    var human_turn = readLine
    field.putmap
    if (human_turn == "b") {
      human ! "next"
    } else if(human_turn == "w") {
      machine ! "next"
    } else {
      human ! "end"
      machine ! "end"
      exit()
    }

    loop {
      receive {
        case (Human, p:Pair[Any,Any]) =>
	  println("recieve from human")
          if(!p._1.isInstanceOf[Int] || !p._2.isInstanceOf[Int]) {
            human ! "puterr"
          }
          var place:Pair[Int, Int] = Pair(p._1.asInstanceOf[Int], p._2.asInstanceOf[Int])
          if(field.put(human_turn, place)) {
	    field.putmap
            machine ! "next"
          } else {
            human ! "puterr"
          }
        case (Machine, p:Pair[Any,Any]) =>
	  println("recieve from machine")
          if(!p._1.isInstanceOf[Int] || !p._2.isInstanceOf[Int]) {
            machine ! "puterr"
          }
          var place:Pair[Int, Int] = Pair(p._1.asInstanceOf[Int], p._2.asInstanceOf[Int])
          if (field.put(backside(human_turn), place)) {
	    field.putmap
            human ! "next"
          } else {
            machine ! "puterr"
          }
      }
    }
  }

  def backside(color:String):String = {
    (color match {
      case "b" => "w"
      case "w" => "b"
      case _ => "e"
    })
  }
}

object Othello extends App {
  val human = new Human
  val machine = new Machine
  val field_manager = new FieldManager(human, machine)
  println("Human coming..")
  human.start
  println("Machine coming..")
  machine.start
  println("FieldManager coming..")
  field_manager.start
}

class Map(vals:Array[Array[String]]) {
  var inner_array:Array[Array[String]] = vals
  def get_position(pair:Pair[Int,Int]) = inner_array(pair._1)(pair._2)
  def set_position(value:String, pair:Pair[Int,Int]) = { inner_array(pair._1)(pair._2) = value }
  def print_map = {
    for(i <- inner_array) {
      for (j <- i) {
        if (j == "") print(" ")
        else print(j)
      }
      print("\n")
    }
  }
}

class Field() {

  val hoge = Array(
    Array("*", "1", "2", "3", "4", "5", "6", "7", "8", "*"),
    Array("1", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("2", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("3", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("4", "",  "",  "",  "b", "w", "",  "",  "",  "*"),
    Array("5", "",  "",  "",  "w", "b", "",  "",  "",  "*"),
    Array("6", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("7", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("8", "",  "",  "",  "",  "",  "",  "",  "",  "*"),
    Array("*", "*", "*", "*", "*", "*", "*", "*", "*", "*"))
  var map:Map = new Map(hoge)

  val left:Pair[Int,Int] = Pair(-1,0)
  val upleft:Pair[Int,Int] = Pair(-1,-1)
  val up:Pair[Int,Int] = Pair(0,-1)
  val upright:Pair[Int,Int] = Pair(1,-1)
  val right:Pair[Int,Int] = Pair(1,0)
  val downright:Pair[Int,Int] = Pair(1,1)
  val down:Pair[Int,Int] = Pair(0,1)
  val downleft:Pair[Int,Int] = Pair(-1,1)

  def put(color:String, pos:Pair[Int,Int]):Boolean = {

    if(this.isPuttable(pos)) {
      return this.reverse(color, pos)
    } else {
      return false
    }
  }

  def isPuttable(pos:Pair[Int,Int]):Boolean = this.map.get_position(pos) == ""

  def reverse(color:String, pos:Pair[Int,Int]):Boolean = {
    var post:Pair[Int,Int] = pos
    var reversable:Boolean = false

    def go_base = { post = pos }

    val directions = Array(left,upleft,up,upright,right,downright,down,downleft)

    for(direction <- directions) {
      go_base
      post = go(post, direction)
      if(this.map.get_position(post) == backside(color)) {

        while( this.map.get_position(post) == backside(color) ) {
          post = go(post, direction)
        }

        if(this.map.get_position(post) == color) {
          reversable = true
          go_base
          post = go(post, direction)
          while( this.map.get_position(post) == backside(color) ) {
            this.map.set_position(color, post)
            post = go(post, direction)
          }
        }
      }
    }

    if( reversable ) {
      this.map.set_position(color, pos)
      return true
    } else {
      return false
    }
  }

  def go(pos:Pair[Int,Int], direction:Pair[Int,Int]):Pair[Int,Int] = Pair(pos._1 + direction._1, pos._2 + direction._2)

  def backside(color:String):String = {
    (color match {
      case "b" => "w"
      case "w" => "b"
      case _ => "e"
    })
  }

  def putmap = {
    this.map.print_map
  }
}
