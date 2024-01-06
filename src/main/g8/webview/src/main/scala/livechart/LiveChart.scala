package livechart

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given} 

val model = new Model
import model.*

@main
def LiveChart(): Unit = {

  val allowedIcons = List("🎉", "🚀", "🍉")
  val iconVar = Var(initial = allowedIcons.head)
  
  renderOnDomContentLoaded(
    dom.document.body,
    div(
      width := "100%",
      div(
        display := "flex",
        marginBottom := "2rem",
        justifyContent := "space-around",
        select(
          onChange.mapToValue --> iconVar.writer,
          value <-- iconVar.signal,
          allowedIcons.map(icon => option(value(icon), icon))
        ),
        button(
          tpe := "button", 
          "Click me!",
          onClick --> { event => println("Clicked") }
        ),
        input(
          padding := "10px",
          width := " 25rem",
          placeholder("Search")
        ),
      ),
      renderDataTable(),
    )
  )

  dom.document.addEventListener("keydown", (event: dom.KeyboardEvent) => {
      var selectedCell = dom.document.getElementsByClassName("selected").headOption.map(_.asInstanceOf[HTMLTableCellElement])

      selectedCell match{
        case None => null
        case Some(cell) => {
          if (event.key == "ArrowDown") {
            // Get the current row and cell index
            val rowIndex = cell.parentElement.asInstanceOf[HTMLTableRowElement].rowIndex
            val cellIndex = cell.cellIndex
            // Move to the cell below, max -2 for the footer
            if (rowIndex < dom.document.getElementById("myTable").asInstanceOf[HTMLTableElement].rows.length - 2) {
              cell.classList.remove("selected")
              dom.document.getElementById("myTable")
                .asInstanceOf[HTMLTableElement].rows(rowIndex + 1)
                .asInstanceOf[HTMLTableRowElement].cells(cellIndex).asInstanceOf[HTMLTableCellElement].classList.add("selected")
            }
          }
          if (event.key == "ArrowUp") {
            // Get the current row and cell index
            val rowIndex = cell.parentElement.asInstanceOf[HTMLTableRowElement].rowIndex
            val cellIndex = cell.cellIndex
            // Move to the cell above
            if (rowIndex > 1) {
              cell.classList.remove("selected")
              dom.document.getElementById("myTable")
                .asInstanceOf[HTMLTableElement].rows(rowIndex - 1)
                .asInstanceOf[HTMLTableRowElement].cells(cellIndex).asInstanceOf[HTMLTableCellElement].classList.add("selected")
            }
          }
          if (event.key == "ArrowLeft") {
            // Get the current row and cell index
            val rowIndex = cell.parentElement.asInstanceOf[HTMLTableRowElement].rowIndex
            val cellIndex = cell.cellIndex
            // Move to the cell left
            if (cellIndex >= 1) {
              cell.classList.remove("selected")
              dom.document.getElementById("myTable")
                .asInstanceOf[HTMLTableElement].rows(rowIndex)
                .asInstanceOf[HTMLTableRowElement].cells(cellIndex - 1).asInstanceOf[HTMLTableCellElement].classList.add("selected")
            }
          }
          if (event.key == "ArrowRight") {
            // Get the current row and cell index
            val rowIndex = cell.parentElement.asInstanceOf[HTMLTableRowElement].rowIndex
            val cellIndex = cell.cellIndex
            // Move to the cell left
            if (cellIndex < dom.document.getElementById("myTable").asInstanceOf[HTMLTableElement].rows(rowIndex).asInstanceOf[HTMLTableRowElement].cells.length - 1 ) {
              cell.classList.remove("selected")
              dom.document.getElementById("myTable")
                .asInstanceOf[HTMLTableElement].rows(rowIndex)
                .asInstanceOf[HTMLTableRowElement].cells(cellIndex + 1).asInstanceOf[HTMLTableCellElement].classList.add("selected")
            }
          }
        }
      }
      

      
    })
}
end LiveChart

def renderDataTable(): Element =
    table(
      idAttr := "myTable",
      width := "100%",
      thead(
        border := "1px solid grey",
        tr(th("Label"), th("Price"), th("Count"), th("Full price"), th("Action"))
      ),
      tbody(
        children <-- dataSignal.map(data => data.map { item =>
          renderDataItem(item.id, item)
        }),
      ),
      tfoot(tr(
        td(button("➕")),
        td(),
        td(),
        td(child.text <-- dataSignal.map(data => "%.2f".format(data.map(_.fullPrice).sum))),
        td(),
      )),
    )
end renderDataTable

def renderDataItem(id: DataItemID, item: DataItem): Element =
    def handleCellClick(event: MouseEvent): Unit = {
      dom.document.getElementsByClassName("selected").map(element => element.classList.remove("selected"))
      event.target.asInstanceOf[HTMLTableCellElement].className = "selected"
      println(event.target.asInstanceOf[HTMLTableCellElement].innerText)
    }

    tr(
      td(item.label, onClick --> handleCellClick),
      td(item.price, onClick --> handleCellClick),
      td(item.count, onClick --> handleCellClick),
      td("%.2f".format(item.fullPrice), onClick --> handleCellClick),
      td(button("🗑️"), onClick --> handleCellClick),
    )
end renderDataItem




import scala.util.Random
import org.scalajs.dom.HTMLTableElement
import org.scalajs.dom.HTMLTableRowElement
import org.scalajs.dom.HTMLTableCellElement
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.html

final class DataItemID

case class DataItem(id: DataItemID, label: String, price: Double, count: Int):
  def fullPrice: Double = price * count

object DataItem:
  def apply(): DataItem =
    DataItem(DataItemID(), "?", Random.nextDouble(), Random.nextInt(5) + 1)
end DataItem

type DataList = List[DataItem]

final class Model:
  val dataVar: Var[DataList] = Var(List(DataItem(DataItemID(), "one", 1.0, 1), DataItem(DataItemID(), "two", 3.0, 2)))
  val dataSignal = dataVar.signal

  def addDataItem(item: DataItem): Unit =
    dataVar.update(data => data :+ item)

  def removeDataItem(id: DataItemID): Unit =
    dataVar.update(data => data.filter(_.id != id))
end Model