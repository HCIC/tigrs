package tigrs

import collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode

import scala.scalajs.js.Dynamic.global
import scala.annotation.meta.field
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scalacss.Defaults._
import scalacss.ScalaCssReact._
import diode._
import diode.ActionResult.ModelUpdate
import diode.react._
import org.singlespaced.d3js
import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._
import org.singlespaced.d3js.Link
import org.singlespaced.d3js.forceModule.Force

import js.JSConverters._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

object GraphView {

  class D3Vertex(
    val v: Vertex
  ) extends d3js.forceModule.Node

  class D3Edge(
    val e: DiEdge[Vertex],
    @(JSExport @field) var source: D3Vertex,
    @(JSExport @field) var target: D3Vertex
  ) extends d3js.Link[D3Vertex]

  case class Props(
    proxy: ModelProxy[Graph[Vertex, DiEdge]],
    width: Double = 960,
    height: Double = 500.0,
    charge: Double = -0.3
  ) {
    def graph = proxy.value
  }

  case class State(
    force: Force[D3Vertex, D3Edge],
    var vertices: js.Array[D3Vertex],
    var edges: js.Array[D3Edge],
    var domVertices: js.UndefOr[d3js.selection.Update[tigrs.GraphView.D3Vertex]] = js.undefined,
    var domEdges: js.UndefOr[d3js.selection.Update[tigrs.GraphView.D3Edge]] = js.undefined
  )

  def initialState(p: Props): State = {
    import p._
    State(
      force = d3.layout.force[D3Vertex, D3Edge]()
        .charge((d: D3Vertex, _: Double) => d.v.r * d.v.r * charge)
        .linkDistance(100)
        .size((width, height)),
      vertices = js.Array(),
      edges = js.Array()
    )
  }

  val vertexGroupRef = Ref[raw.HTMLCanvasElement]("vertices")
  val edgeGroupRef = Ref[raw.HTMLCanvasElement]("edges")

  class Backend($: BackendScope[Props, State]) {

    def updateData(p: Props, s: State) = Callback {
      import p._
      import s._

      val oldVertices = vertices.map(d => d.v -> d).toMap
      val newVertices = p.graph.nodes.map((v: Graph[Vertex, DiEdge]#NodeT) => v.value).map { v =>
        oldVertices.get(v) match {
          case Some(d3v) => d3v
          case None => new D3Vertex(v)
        }
      }.toJSArray

      val vertexMap = newVertices.map(d => d.v -> d).toMap
      val oldEdges = edges.map(d => (d.e -> d)).toMap
      val newEdges = p.graph.edges.map { e_inner: Graph[Vertex, DiEdge]#EdgeT =>
        val e = e_inner.toOuter
        oldEdges.get(e) match {
          case Some(d3e) =>
            d3e
          case None =>
            new D3Edge(e, vertexMap(e.source), vertexMap(e.target))
        }
      }.toJSArray

      vertices = newVertices
      edges = newEdges
    }

    def updateVisualization(p: Props, s: State) = Callback {
      import p._
      import s._
      lazy val domVerticesSel = d3.select(vertexGroupRef($).get).selectAll("circle")
      lazy val domEdgesSel = d3.select(edgeGroupRef($).get).selectAll("line")

      domVertices = domVertices.getOrElse(domVerticesSel).data(vertices)
      domVertices.get.exit().remove()
      domVertices.get.enter().append("circle")
      domVertices.get
        .attr("cx", (d: D3Vertex) => d.x)
        .attr("cy", (d: D3Vertex) => d.y)
        .attr("r", (d: D3Vertex) => d.v.r)
        .style("fill", "steelblue")
        .on("click", (d: D3Vertex) => p.proxy.dispatch(RemoveVertex(d.v)).runNow())

      domEdges = domEdges.getOrElse(domEdgesSel).data(edges)
      domEdges.get.exit().remove()
      domEdges.get.enter().append("line")
      domEdges.get
        .attr("x1", (d: D3Edge) => d.source.x)
        .attr("y1", (d: D3Edge) => d.source.y)
        .attr("x2", (d: D3Edge) => d.target.x)
        .attr("y2", (d: D3Edge) => d.target.y)
        .style("stroke", "#666")
        .style("stroke-width", 2)

      force.nodes(vertices).links(edges)
      force.start()
    }

    def update(p: Props, s: State): Callback = updateData(p, s) >> updateVisualization(p, s)

    def registerTick(s: State) = Callback {
      import s._

      force.on("tick", (e: Event) => {
        domVertices.get
          .attr("cx", (d: D3Vertex) => d.x)
          .attr("cy", (d: D3Vertex) => d.y)

        domEdges.get
          .attr("x1", (d: D3Edge) => d.source.x)
          .attr("y1", (d: D3Edge) => d.source.y)
          .attr("x2", (d: D3Edge) => d.target.x)
          .attr("y2", (d: D3Edge) => d.target.y)
        ()
      })
    }

    def stopForce(s: State) = Callback {
      s.force.stop()
    }

    def render(p: Props, s: State) = {
      <.div(
        <.button(^.onClick --> p.proxy.dispatch(AddVertex(Vertex(r = scala.util.Random.nextDouble * 20 + 10))), "add vertex"),
        <.button(^.onClick --> {
          val vs = scala.util.Random.shuffle(p.graph.nodes.toSeq).take(2)
          p.proxy.dispatch(AddEdge(DiEdge(vs(0).value, vs(1).value)))
        }, "add edge"),
        <.div(
          ^.width := "1000px",
          ^.height := "1000px",
          ^.position := "relative",
          <.svg.svg(
            ^.width := "1000px",
            ^.height := "1000px",
            ^.position := "absolute",
            ^.ref := "edges"
          ),
          <.svg.svg(
            ^.width := "1000px",
            ^.height := "1000px",
            ^.position := "absolute",
            ^.ref := "vertices"
          )
        )
      )
    }
  }

  private val component = ReactComponentB[Props]("SmartComponent")
    .initialState_P(initialState)
    .renderBackend[Backend]
    .componentDidMount(c => c.backend.update(c.props, c.state) >> c.backend.registerTick(c.state))
    .shouldComponentUpdate(c => { c.$.backend.update(c.currentProps, c.currentState).runNow(); false }) // let d3 update, instead of react
    .componentWillUnmount(c => c.backend.stopForce(c.state))
    .build

  def apply(proxy: ModelProxy[Graph[Vertex, DiEdge]]) = component(Props(proxy))
}
