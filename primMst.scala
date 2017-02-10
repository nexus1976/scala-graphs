package minimumSpanningTree {
  import scala.collection._
  import scala.collection.mutable._
  import weighted._
  import collections._
  
  class PrimMst(markedc: ArrayBuffer[Boolean], edgeToc: ArrayBuffer[Edge], distToc: ArrayBuffer[Float], pqc: VertexPriorityQueue) {
    var marked: ArrayBuffer[Boolean] = markedc
    var edgeTo: ArrayBuffer[Edge] = edgeToc
    var distTo: ArrayBuffer[Float] = distToc
    var pq: VertexPriorityQueue = pqc
    
    def visit(g: EdgeWeightedGraph, v: Int) {
      marked(v) = true
      for(edge <- g.AdjacentTo(v)) {
        var w = edge.OtherVertex(v)
        if(marked(w) == false) {
          if(edge.Weight() < distTo(w)) {
            edgeTo(w) = edge
            distTo(w) = edge.Weight()
            if(pq.exists { x => x.Index() == w }) {
              pq(w).SetWeight(distTo(w))
            } else {
              var vertex = VertexFactory.NewVertex(w, distTo(w))
              pq += vertex
            }
          }
        }
      }
    }
    
    def Edges() : ArrayBuffer[Edge] = {
      return edgeTo
    }
    
    def Weight() : Float = {
      var weight: Float = 0.0f
      for(edge <- edgeTo) {
        weight += edge.Weight()
      }      
      return weight
    }
    
    def PrintMinimumSpanningTree() : Unit = {
      println("Prim’s Minimum Spanning Tree:")
      
      for(e <- Edges()) {
        printf(s"\{${e.From()} ${e.To()} ${e.Weight()} \}")
        println()
      }
      
      printf(s"Weight: ${Weight()}")
    }
  }
  
  object PrimMstFactory {
    def NewPrimMst(g: EdgeWeightedGraph) : PrimMst = {
      val verticesCount: Int = g.Vertices()
      var initBooleanArray: ArrayBuffer[Boolean] = ArrayBuffer.fill(verticesCount)(false)
      var initFloatArray: ArrayBuffer[Float] = ArrayBuffer.fill(verticesCount)(Float.MaxValue)
      var initEdgeArray: ArrayBuffer[Edge] = ArrayBuffer.fill(verticesCount)(null)
      var initPQ: VertexPriorityQueue = new VertexPriorityQueue()
      
      var l: PrimMst = new PrimMst(initBooleanArray, initEdgeArray, initFloatArray, initPQ)
      
      l.distTo(0) = 0.0f
      var vertex: Vertex = VertexFactory.NewVertex(0, 0.0f)
      l.pq.insert(0, vertex)
      while(!l.pq.isEmpty) {
        var lowest = l.pq.sortBy { x => x.Weight() }.head
        l.pq.remove(lowest.Index())
        l.visit(g, lowest.Index())  
      }
  
      return l
    }
  }
}