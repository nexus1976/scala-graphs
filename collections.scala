package collections {
  import scala.collection._
  import scala.collection.mutable._
  import weighted._
  
  class VertexPriorityQueue extends ArrayBuffer[Vertex] {
    def Len() : Int = {
      return this.size0
    }
    
    def Insert(e: Vertex) : Unit = {
      this += e
    }
    
    def DelMin() : Vertex = {
      var di : Int = -1
      var e: Vertex = null
      var i: Int = 0
      val len: Int = this.Len()
      
      for(i <- 0 until len) {
        var v = this(i)
        if(e == null || di == -1 || v.Weight() < e.Weight()) {
          di = i
          e = v
        }
      }
      if(di == -1) {
        return null
      }
      return e
    }
    
    def Weight() : Float = {
      var weight: Float = 0.0f
      this.foreach { weight += _.Weight() }
      return weight
    }
  }
  
}