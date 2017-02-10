package weighted {
  import scala.collection._
  import scala.collection.mutable._
  import weighted._
  
  trait Item {
    def Weight() : Float
    def SetWeight(weightIn: Float) : Unit
  }
  trait IndexItem {
    def Item : Item
    def Index() : Int
  }
}