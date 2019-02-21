import monix.eval.Task
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.HTMLImageElement

package object tanks {

  def appendPar(targetNode: dom.Node, text: String): Task[Unit] = Task {
    val parNode  = document.createElement("p")
    val textNode = document.createTextNode(text)

    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  type Ctx2D = dom.CanvasRenderingContext2D

  def loadImage(ctx: Ctx2D, src: String = "images/general-sprites2.png"): Task[HTMLImageElement] =
    Task.async { cb =>
      val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      image.src = src
      image.onload = { _ =>
        cb.onSuccess(image)
      }
    }
}
