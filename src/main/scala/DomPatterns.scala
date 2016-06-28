package tigrs

import org.scalajs.dom._

package object domPatterns {

  implicit class NodeCollection(nodeList: NodeList) extends Iterable[Node] {
    override def iterator = new Iterator[Node] {
      var i = 0
      def hasNext = i < nodeList.length
      def next = {
        val node = nodeList.item(i)
        i += 1
        node
      }
    }
  }
  implicit class NodeMapCollection(nodeList: NamedNodeMap) extends Iterable[(String, String)] {
    override def iterator = new Iterator[(String, String)] {
      var i = 0
      def hasNext = i < nodeList.length
      def next = {
        val node = nodeList.item(i)
        i += 1
        (node.name -> node.value)
      }
    }
  }

  object NodeEx {
    def unapply(node: Element): Option[(String, Seq[(String, String)], String, Seq[Node])] = {
      Some((node.nodeName, node.attributes.toSeq, node.textContent, node.childNodes.toSeq))
    }
  }
}
