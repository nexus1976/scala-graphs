import scala.collection.mutable.ArrayBuffer

package weighted {
  object WeightedTypes {
    type rowOfEdges = ArrayBuffer[Edge]    
  }
  
  class EdgeWeightedGraph(vc: Int, ec: Int, adjc: ArrayBuffer[WeightedTypes.rowOfEdges]) {
    var v: Int = vc
    var e: Int = ec
    var adj: ArrayBuffer[WeightedTypes.rowOfEdges] = adjc
    
    def Vertices() : Int = {
      return v
    }
    
    def AddUndirectedEdge(edge: Edge) : Unit = {
      var ev: Int = edge.From()
      var ew: Int = edge.OtherVertex(ev)
      
      adj(ev) += edge
      adj(ew) += edge
      e += 1
    }
    
    def AddDirectedEdge(edge: Edge) : Unit = {
      var index: Int = edge.From()
      adj(index) += edge
      e += 1
    }
    
    def AdjacentTo(index: Int) : WeightedTypes.rowOfEdges = {
      return adj(index)
    }
    
    def Edges() : WeightedTypes.rowOfEdges = {
      var b: WeightedTypes.rowOfEdges = new WeightedTypes.rowOfEdges
      var i: Int = 0 
      
      for(i <- 0 until v) {
        for(edge <- adj(i)) {
          if(edge.OtherVertex(i) > i) {
            b += edge
          }
        }
      }
      return b
    }
  }
  
  object EdgeWeightedGraphFactory {
    def NewEdgeWeightedGraph(v: Int) : EdgeWeightedGraph = {
      var newAdj = ArrayBuffer.fill(v)(new ArrayBuffer[Edge]())
      var g: EdgeWeightedGraph = new EdgeWeightedGraph(v, 0, newAdj)
      return g
    }
  }
}