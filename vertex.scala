package weighted {
  class Vertex(indexc: Int, weightc: Float) {
    var index: Int = indexc
    var weight: Float = weightc
    
    def Weight() : Float = {
      return weight
    }
    
    def Index() : Int = {
      return index
    }
    
    def SetWeight(weightIn: Float) {
      weight = weightIn
    }
  }
  
  object VertexFactory {
    def NewVertex(index: Int, weight: Float) : Vertex = {
      return new Vertex(index, weight)
    }
  }
}