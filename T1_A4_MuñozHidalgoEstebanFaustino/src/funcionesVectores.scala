
object funcionesVectores {

  def Media(vector: Array[Int]): Double = {
    var suma = 0.0
    for (dat <- vector)
      suma += dat.toDouble
    return suma / vector.length
  }

  
  def Moda(vector: Array[Int]): Double = {
    var maxRep = 0
    var moda = 0

    for (dat1 <- vector) {
      var numRep = 0;
      for (dat2 <- vector) {
        if (dat1 == dat2) {
          numRep += 1
        }
        if (numRep > maxRep) {
          moda = dat1;
          maxRep = numRep;
        }
      }
    }
    return moda
  }

  
  def Mediana(vector: Array[Int]): Double = {
    var mediana = 0.0
    scala.util.Sorting.quickSort(vector)
    if (vector.length % 2 == 0) {
      var sumaMedios = vector(vector.length / 2) + vector(vector.length / 2 - 1)
      mediana = sumaMedios / 2
    } else {
      mediana = vector(vector.length / 2)
    }
    return mediana
  }

  
  def Desviacion(vector: Array[Int], media: Double): Unit = {
    var desv = 0.0
    for (dat <- vector) {
      desv = dat.toDouble - media
      println("Para " + dat + " es de: " + desv.abs)
    }

  }

  
  def DesviacionMedia(vector: Array[Int], media: Double): Double = {
    var desvM = 0.0
    var sum = 0.0
    for (dat <- vector) {
      desvM = (dat.toDouble - media)
      sum += desvM
    }
    return sum.abs / vector.length
  }

  
  def Varianza(vector: Array[Int], media: Double): Double = {
    var desvM = 0.0
    var sum = 0.0
    for (dat <- vector) {
      desvM = (dat.toDouble - media)
      sum += Math.pow(desvM, 2)
    }
    return sum.abs / vector.length
  }

  
  def desviacionEstandar(varianza: Double): Double = {
    return Math.sqrt(varianza)
  }

  
  def main(args: Array[String]): Unit = {
    var j = 0
    println("Ingresa el tamaño del vector:")
    j = scala.io.StdIn.readInt()

    var datos: Array[Int] = new Array[Int](j)
    println("El tamaño del vector es: " + datos.length)
    for (i <- 0 until j) {
      println("Ingresa  el valor " + i + ":")
      datos(i) = scala.io.StdIn.readInt()
    }


    val med = Media(datos)
    println("La media es: " + med)
    println("la Moda es: %f".format(Moda(datos)))
    println("La mediana es " + Mediana(datos))
    println("La desviacion respecto a la media es de:")
    Desviacion(datos, med)
    println("La desviacion media es de: " + DesviacionMedia(datos, med))
    val varian = Varianza(datos, med)
    println("La varianza es de: " + varian)
    println("La desviacion estandar es de: " + desviacionEstandar(varian))

  }

}

