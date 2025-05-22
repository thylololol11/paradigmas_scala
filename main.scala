import scala.io.StdIn
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import java.io.{File, PrintWriter}

case class Ingrediente(nombre: String, cantidad: Double, unidad: String)
case class Receta(dia: String, receta: String, ingredientes: List[Ingrediente])

object PlanificadorCompras {
  // Datos iniciales
  var menu: List[Receta] = List(
    Receta("lunes", "ensalada", List(
      Ingrediente("lechuga", 1, "pieza"),
      Ingrediente("tomate", 2, "pieza")
    )),
    Receta("martes", "pasta", List(
      Ingrediente("pasta", 200, "gramos"),
      Ingrediente("tomate", 1, "pieza")
    ))
  )

  var inventario: List[Ingrediente] = List(
    Ingrediente("tomate", 1, "pieza"),
    Ingrediente("pasta", 100, "gramos")
  )

  def generarListaCompras(menu: List[Receta], inventario: List[Ingrediente]): List[Ingrediente] = {
    val ingredientesNecesarios = menu.flatMap(_.ingredientes)
    
    val ingredientesAgrupados = ingredientesNecesarios
      .groupBy(i => (i.nombre, i.unidad))
      .map { case ((nombre, unidad), ingredientes) =>
        Ingrediente(nombre, ingredientes.map(_.cantidad).sum, unidad)
      }.toList
    
    ingredientesAgrupados.flatMap { necesario =>
      inventario.find(i => i.nombre == necesario.nombre && i.unidad == necesario.unidad) match {
        case Some(disponible) =>
          val cantidadFaltante = necesario.cantidad - disponible.cantidad
          if (cantidadFaltante > 0) Some(necesario.copy(cantidad = cantidadFaltante))
          else None
        case None => Some(necesario)
      }
    }
  }

  def mostrarMenuJson(menu: List[Receta]): String = {
    val jsonRecetas = menu.map { receta =>
      val ingredientesJson = receta.ingredientes.map { ingrediente =>
        s"""    { "nombre": "${ingrediente.nombre}", "cantidad": ${ingrediente.cantidad}, "unidad": "${ingrediente.unidad}" }"""
      }.mkString(",\n")
      
      s"""  {
  |    "dia": "${receta.dia}",
  |    "receta": "${receta.receta}",
  |    "ingredientes": [
  |$ingredientesJson
  |    ]
  |  }""".stripMargin
    }.mkString(",\n")
    
    s"""[
$jsonRecetas
]"""
  }

  def mostrarInventarioJson(inventario: List[Ingrediente]): String = {
    val items = inventario.map { item =>
      s"""  { "nombre": "${item.nombre}", "cantidad": ${item.cantidad}, "unidad": "${item.unidad}" }"""
    }.mkString(",\n")
    
    s"""[
$items
]"""
  }

  def mostrarListaComprasJson(compras: List[Ingrediente]): String = {
    val items = compras.map { item =>
      s"""  { "nombre": "${item.nombre}", "cantidad": ${item.cantidad}, "unidad": "${item.unidad}" }"""
    }.mkString(",\n")
    
    s"""[
$items
]"""
  }

  def agregarReceta(): Unit = {
    println("\n--- Agregar Nueva Receta ---")
    print("Día de la semana (ej. lunes): ")
    val dia = StdIn.readLine()
    print("Nombre de la receta: ")
    val nombreReceta = StdIn.readLine()
    
    val ingredientes = ListBuffer[Ingrediente]()
    var continuar = true
    
    while (continuar) {
      println("\nAgregar ingrediente:")
      print("Nombre: ")
      val nombre = StdIn.readLine()
      print("Cantidad: ")
      val cantidad = Try(StdIn.readDouble()).getOrElse(0.0)
      print("Unidad (ej. pieza, gramos): ")
      val unidad = StdIn.readLine()
      
      ingredientes += Ingrediente(nombre, cantidad, unidad)
      
      print("¿Agregar otro ingrediente? (s/n): ")
      continuar = StdIn.readLine().toLowerCase == "s"
    }
    
    menu = menu :+ Receta(dia, nombreReceta, ingredientes.toList)
    println("¡Receta agregada exitosamente!")
  }

  def agregarAlInventario(): Unit = {
    println("\n--- Agregar al Inventario ---")
    var continuar = true
    
    while (continuar) {
      println("\nAgregar ingrediente al inventario:")
      print("Nombre: ")
      val nombre = StdIn.readLine()
      print("Cantidad: ")
      val cantidad = Try(StdIn.readDouble()).getOrElse(0.0)
      print("Unidad (ej. pieza, gramos): ")
      val unidad = StdIn.readLine()
      
      inventario.find(_.nombre == nombre) match {
        case Some(existente) =>
          inventario = inventario.map { item =>
            if (item.nombre == nombre && item.unidad == unidad) 
              item.copy(cantidad = item.cantidad + cantidad)
            else item
          }
        case None =>
          inventario = inventario :+ Ingrediente(nombre, cantidad, unidad)
      }
      
      print("¿Agregar otro ingrediente? (s/n): ")
      continuar = StdIn.readLine().toLowerCase == "s"
    }
    
    println("¡Inventario actualizado exitosamente!")
  }

  def menuPrincipal(): Unit = {
    var salir = false
    
    while (!salir) {
      println("\n=== Planificador de Compras Semanales ===")
      println("1. Mostrar menú semanal (JSON)")
      println("2. Mostrar inventario disponible (JSON)")
      println("3. Generar lista de compras (JSON)")
      println("4. Agregar nueva receta")
      println("5. Agregar items al inventario")
      println("6. Salir")
      print("\nSeleccione una opción: ")
      
      Try(StdIn.readInt()) match {
        case Success(1) =>
          println("\nMenú Semanal (JSON):")
          println(mostrarMenuJson(menu))
        
        case Success(2) =>
          println("\nInventario Disponible (JSON):")
          println(mostrarInventarioJson(inventario))
        
        case Success(3) =>
          val listaCompras = generarListaCompras(menu, inventario)
          println("\nLista de Compras (JSON):")
          println(mostrarListaComprasJson(listaCompras))
        
        case Success(4) =>
          agregarReceta()
        
        case Success(5) =>
          agregarAlInventario()
        
        case Success(6) =>
          salir = true
          println("¡Hasta luego!")
        
        case _ =>
          println("Opción no válida. Por favor intente de nuevo.")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    menuPrincipal()
  }
}
