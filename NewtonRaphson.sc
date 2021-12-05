
//------** Mecanismos de control de entrada de datos ***-----------

def controlador(num : Int): Double = {
  if(num < 0){
    println("Numero negativo ingresado")
    println("Se tomara el valor absoluto del numero")
    num * -1
  }else{
    num
  }
}

// ----*********** Metodo de Newton-Raphson ********----------

def funcionyDerivada (x : Double , aux : Double): Double = ((x*x)-aux)/(2*x)
  // define f(x) = x^2 - C sobre f'(x) = 2x

def iterac(num : Double , i : Int , aux : Double): Double ={

  val x = num-(funcionyDerivada(num , aux)) // resultado de la formula Xn+1 = Xn- f(Xn) / f'(Xn)

  if(i == 1){
    x
  } else {
    iterac(x,(i-1),aux)
  }
}

def newtonRaph(num : Int ): Double = {
  var x0 = num.toDouble

  if(num == 0){
    return 0
  } else {
    x0 = controlador(num)
  }

  val x1 = x0 / 2       // Hace referencia al x1

  iterac(x1,7, x0) // 8 iteraciones para promedio de 15 decimales exactos
}


// ----------------***********PRUEBAS**********-------------------

newtonRaph(10)
print("-----------------------")
newtonRaph(0)
print("-----------------------")
newtonRaph(100)
print("-----------------------")
newtonRaph(12)
print("-----------------------")
newtonRaph(-50)
print("-----------------------")
newtonRaph(2)
print("-----------------------")
newtonRaph(4)
print("-----------------------")
newtonRaph(-4)

