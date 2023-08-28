package introduccion

import javax.naming.OperationNotSupportedException

class EjercicioListas {
  /*
  * Punto 2 repetir lista
  * @param lista Lista a repetir
  * @param n Número de veces a repetir la lista
  * @return Lista repetida n veces
  * @example repetirLista(List(1, 2, 3), 2) = List(List(1,1),List(2,2),List(3,3))
  * @throws IllegalArgumentException si n es negativo
  */
  def repetirListas(lista: List[Int], n: Int): List[List[Int]] = {
    if (n < 0){
      throw new IllegalArgumentException ("n no puede ser menor a 0")
    }
    var listaRepetida : List[List[Int]] = List()
    for(i<-0 until lista.length){
      var tempList : List[Int] = List();
      for(_ <-0 until n){
        tempList = tempList :+ lista(i)
      }
      listaRepetida = listaRepetida :+ tempList
    }
    return listaRepetida
  }
  /*
  * Punto 3: Filtrar listas
  * @param criterioIn Criterio de filtrado que puede ser "mayor", "menor", "mayoroigual", "igual", "diferente" o "menoroigual"
  * @param n Número a comparar
  * @param lista Lista a filtrar
  * @return Lista filtrada de acuerdo al criterio y n
  * @throws IllegalArgumentException si el criterio no es uno de los valores válidos
  */

  def filtrarListas(criterioIn: String, n: Int, lista: List[Int]) : List[Int] = {
    var criterio : String = criterioIn.toLowerCase()
    var listaFiltrada : List[Int] = List()

    val result = criterio match {
      case "mayor" => lista.filter(_ > n)
      case "menor" => lista.filter(_ < n)
      case "mayoroigual" => lista.filter(_ >= n)
      case "igual" => lista.filter(_ == n)
      case "diferente" => lista.filter(_ != n)
      case "menoroigual" => lista.filter(_ <= n)
      case _ => throw new IllegalArgumentException("El criterio no es valido")
    }
    return result
  }
}
