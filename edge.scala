package weighted {
  class Edge(vc: Int, wc: Int, weightc: Float) {
    var v: Int = vc
    var w: Int = wc
    var weight: Float = weightc
    
    def Weight() : Float = {
      return weight  
    }
    
    def SetWeight(weightIn: Float) {
      weight = weightIn
    }
    
    def OtherVertex(vertex: Int) : Int = {
      if(vertex == v) {
        return w
      } else if(vertex == w) {
        return v
      }
      return -1
    }
    
    def From() : Int = {
      return v
    }
    
    def To() : Int = {
      return w
    }
    
    def CompareTo(edge : Edge) : Int = {
      if(weight < edge.weight) {
        return -1
      } else if(weight > edge.weight) {
        return 1
      } else {
        return 0
      }
    }
    
    def Initialized() : Boolean = {
      return v != 0 || w != 0
    }
  }
  
  object EdgeFactory {
    def NewEdge(v: Int, w: Int, weight: Float) : Edge = {
      var e : Edge = new Edge(v, w, weight)
      return e
    }  
  }
}