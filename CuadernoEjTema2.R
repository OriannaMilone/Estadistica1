variaciones <- function(n, m) exp(lgamma(n + 1L) - lgamma(n - m + 1L))

## EJERCICIOS DEL CUADERNILLO 2

#1. La combinaci´on de una caja fuerte consiste en 5 cifras distintas ¿Cu´antas combinaciones tendr´as que
#probar como m´aximo para abrirla? ¿Y si cada cifra s´olo tuviese que ser distinta de la anterior? ¿Y si
#pudieses repetir cualquier cifra?

#R1: Si consideramos que las cifras son todas diferentes, hablaremos de variaciones sin repetición, de 10 posibles elementos. n(0:9) en 5 posiciones
combinacion = sample(c(0:9), 5)
variaciones(10,5)

#R3: Si se pudiese repetir cualquier cifra, tendriamos, variaciones con repeticion
contraseña = c(0,0,0,0,0)
i = 1
while(i<6){
  contraseña[i] = sample(c(0:9), 1)
  i = i + 1
}
contraseña
10^5
#Otra solucion: 
numeros = 0:9
posibles_com = (sample(rep(numeros,5), 5))
posibles_com

#R2: Si solo tuviesen que ser distintas de la anterior
con1 = sample(numeros, 1)
con = setdiff(sample(numeros,1), con1) #Y este proceso se repite hasta que se agoten los espacios donde colocar una cifra
10 * (9^4)


# 2. Dos bolas se sacan al azar de una urna que contiene 6 bolas blancas y 5 negras. ¿Cu´al es la probabilidad
#de que una de las bolas sea blanca y la otra negra?

N = 5000
sims = replicate(N,{
  urna = c(rep("B", 6), rep("N", 5))
  sacado = sample(urna, 2)
  evento = all(table(sacado) == 1) 
  sum(evento)
})
mean(sims)

#3. ¿Cu´antas palabras distintas de 8 letras pueden formarse con a,a,a,b,b,c,c,d?

letras = c(rep("a", 3), rep("b", 2), rep("c", 2), "d")
orden = sample(letras) #Permutaciones con elementos indistinguibles, es decir:

factorial(8)/ (factorial(3) * factorial(2) * factorial(2))


#4. Un comit´e de tama˜no 5 se selecciona al azar entre un grupo de 6 hombres y 9 mujeres. ¿Cu´al es la
#probabilidad de que el comit´e consista en 3 hombres y 2 mujeres?

#Supongamos que los hombres vienen representados por los 1, y las mujeres por los 0

N = 5000
sims = replicate(N, {
  grupo = c(rep(1, 6), rep(0, 9))
  comite = sum(sample(grupo, 5))
  sum((comite == 3))
}
)
mean(sims)


#5. En una clase de estad´ıstica hay 6 alumnos de ADE y 4 inform´aticos. Los alumnos reciben un ranking
#dependiendo de su nota en un examen. Asumiendo que no hay notas repetidas, ¿Cu´antos rankings
#posibles hay? ¿Cu´al es la probabilidad de que los 4 inform´aticos obtengan los 4 primeros puestos del
#ranking?

#a) PRIMERA PREGUNTA NO SE

#b) Prob de que los 4 informáticos obtengan los primeros 4 puestos. 
#Supongamos que los fijamos en ellas, y calculamos de cuántas formas distintas podemos distribuir a los otros 6 alumnos 
#si consideramos que el orden importa, ya que son 6 personas distintas. tendremos permutaciones de 6. Si consideramos que el orden 
#nos importa para las 6 personas, entonces, para los 4 informaticos también, entonces, permutaciones de 4

alumnos = c(paste("I", (1:4)), paste("ADE", (1:6)))
casos_tot = sample(alumnos)
casos_fav = sample(c(paste("ADE", (1:6)))) 


casos_fav = factorial(4) * factorial(6)
casos_tot = factorial(10)
casos_fav/casos_tot


# 6. En una carrera participan 7 atletas, ¿Cu´antas listas de llegada son posibles? Si hay 3 ingleses, 2
#franceses, un portugu´es y un espa˜nol, ¿de cu´antas maneras pueden figurar en la lista de llegada las
#banderas nacionales?


factorial(7) #Si consideramos todas las maneras posibles en que pueden llegar los 7 corredores distintos
factorial(7) / (factorial(3) * factorial(2))


# 7. Se saca una bola al azar de una caja que contiene 6 bolas rojas, 4 blancas y 5 azules.
#Determina la probabilidad de que la bola sea (a) roja, (b) blanca, (c) azul (d) no sea roja (e) sea roja o
#blanca.

#Lo puedo hacer con simulaciones, para cada caso. 
caja = c(rep("R", 6), rep("B", 4), rep("A", 5))
casos_totales = sample(caja, 1) #Permutaciones con elementos indistinguibles
caso_R = "R" #6/15
caso_B = "B" #4/15
caso_A = "A" #5/15

caso_NoR = 1-(6/15)
caso_RUB = (4/15) + (6/15)

# 8. ¿Cu´al es la probabilidad de obtener al menos un 3 en dos lanzamientos de un dado balanceado? (nos
# referimos al n´umero de 3s sacados, no a la suma).

N = 5000
sims = replicate(N, {
  lanz1 = sample(c(1:6), 1)
  lanz2 = sample(c(1:6), 1)
  exito = any((lanz1 == 3) || (lanz2 == 3))
  sum(exito)
})
sum(sims)/N

#No obtener un 3 en 2 tiros
dado = c(1:6)

N = 5000
simulacion = replicate(N, {
  lanzamiento1 = sample(dado, 1)
  lanzamiento2 = sample(dado, 1)
  lanz = c((lanzamiento1 != 3), (lanzamiento2 != 3))
  all(lanz)
})

(1) - (sum(simulacion)/N)

# 9. Un grupo de 5 chicos y 10 chicas quieren sentarse en un banco.
#(a) ¿Cu´al es la probabilidad de que la cuarta persona sea un chico?
#(b) ¿Cu´al es la probabilidad de que la decimo-segunda posici´on est´e ocupada por un chico?
#(c) ¿Cu´al es la probabilidad de que un chico en particular est´e en la tercera posici´on?

grupo = c(paste("M", 1:10), paste("H", 1:5))
grupo

#a)
#Si fijamos a un chico en la cuarta posición, nos queda, evaluar de cuantas maneras se pueden 
#organizar las 10 chicas y los restantes 4 chicos en la primeras 3 y en las ultimas 10 posiciones. 

primeras_posiciones = variaciones(14, 3)
ultimas_posiciones = factorial(11)
caso_fav = primeras_posiciones * 5 * ultimas_posiciones
casos_tot = factorial(15)

caso_fav/casos_tot

#b) Lo mismo que el caso anterior
#c) De ser un chico en concreto no evaluamos entonces la posibilidad de que el chico cambie, que es lo que haciamos 
#con el "*5*"

primeras_posiciones = variaciones(14, 2)
ultimas_posiciones = factorial(12)
caso_fav = primeras_posiciones * ultimas_posiciones
casos_tot = factorial(15)

caso_fav/casos_tot


#10. Una caja A contiene cinco canicas rojas y tres azules, una caja B contiene dos rojas y tres azules. Se
#coje una canica al azar de cada caja:
#(a) Halla la probabilidad p de que ambas sean rojas.
#(b) Halla la probabilidad p de que una sea roja y otra azul.

#Asumiremos que las bolas Rojas son los 1, y los 0 las bolas azules
A = c(rep(1, 5), rep(0, 3))
B = c(rep(1, 2), rep(0, 3))

#a)
N = 5000
sims = replicate(N,{
  casoA = sample(A, 1)
  casoB = sample(B, 1)
  exito = ((sum(casoA, casoB))>1)
  sum(exito)
})
mean(sims)

#b) 
N = 5000
sims = replicate(N,{
  casoA = sample(A, 1)
  casoB = sample(B, 1)
  exito = ((sum(casoA, casoB)) == 1)
  sum(exito)
})
mean(sims)

## Otra solucion (solo me interesa recordar el table)
A = c(rep("R", 5), rep("A", 3))
B = c(rep("R", 2), rep("A", 3))

simulacionB = replicate(N, {
  cajaA = sample(A, 1) 
  cajaB = sample(B, 1)
  all(table(c(cajaA, cajaB)) == 1)
})
probB = sum(simulacionB)/ N

# 11. Un armario tiene 10 pares de zapatos. Si seleccionamos 8 zapatos al azar, Cu´al es la
#probabilidad de (a) no seleccionar un par completo, (b) seleccionar exactamente un par completo

### RESOLVER POR COMBINATORIA QUE NO DA IGUAL
N = 50000
sims = replicate(N,{
  closet = c(rep((0:1), 10))
  escoger = sample(closet, 8)
  exito = ((sum(escoger)) == 4)
})

par_completo = mean(sims)
par_completo
par_incompleto = 1 - (par_completo)
par_incompleto


# 12. En una l´ınea est´an acomodadas cinco canicas rojas, 2 blancas y 3 azules. Si las canicas del
# mismo color son indistinguibles. ¿Cu´antas ordenaciones distintas existen?

#permutaciones con elementos indistinguibles
factorial(10) / (factorial(5)*factorial(2)*factorial(3))

canicas = c(rep("R", 5),rep("B", 2),rep("Az", 3))
sample(canicas)  ##Falta como una mejor explicacion, pero xs 


# 13. ¿Cu´antas “hamburguesas” distintas pueden hacerse con carne, lechuga, tomate, queso y bacon? Enti´endase
#por hamburguesa la combinaci´on de los anteriores ingredientes, habiendo al menos uno de ellos presente.

ingredientes = c("Carne", "Lechuga", "tomate", "Queso", "Bacon")
hamburguesa1 = sample(ingredientes, 1) # 5       Este representa los 5 posibles casos de tener solo 1 ingredientes, si consideramos que hay 5, entonces 5
hamburguesa2 = sample(ingredientes, 2) # combinaciones(5, 2) Ahora, evaluamos de cuantas maneras podemos tomar 2 ingredientes de 5, sin que nos importe el orden
hamburguesa3 = sample(ingredientes, 3) # combinaciones(5, 3) Mismo razonamiento para 3 ing de 5
hamburguesa4 = sample(ingredientes, 4) # combinaciones(5, 4) Mismo razonamiento para 4 ing de 5
hamburguesa5 = sample(ingredientes)    # 1       Este representa el único caso, donde seleccionamos todos los ingredientes

choose(5,2) + choose(5,3) + choose(5,4) + 5 + 1


#14.  ¿De cu´antas maneras posibles se pueden sentar 7 personas alrededor de una mesa redonda
#si (a) se pueden sentar en cualquier lugar, (b) 2 personas se llevan mal y no pueden sentase juntas.

# a) Permutaciones circulares 
invitados = c(1:7) 
jefe_mesa = sample(invitados, 1) #Fijamos a un "jefe en la mesa", y permutamos al resto de personas alrededor. 
invitados_restantes = setdiff(sample(invitados), jefe_mesa) #De cuantas maneras podemos ordenar a 7-1 personas
factorial(7-1)
# b) Para este caso, fijamos a las dos personas siempre con una persona de por medio (a ambos lados), de modo que
#ambos serán como "jefes de mesa", entonces permutaciones circulares de 7-2, sin embargo, también hay que considerar
#que estos jefes de mesa pueden sentarse en el lugar del otro.

factorial(7-2) * 4 #Veo porque sería *2, pero *4 ??? / Los dos extremos?

#15. Una caja contiene 8 bolas rojas, 3 blancas y 9 azules. Si se sacan 3 bolas al azar sin
#reemplazamiento, determina la probabilidad de que (a) las 3 sean rojas, (b) las 3 sean blancas, (c) 2 sean
#rojas y 1 blanca, (d) al menos 1 sea blanca, (e) se saque una de cada color, (f) se saquen en el siguiente
#orden: roja, blanca, azul.

N = 5000
#a)
simsR = replicate(N, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  exito = all((table(bolas) == 3) & (bolas[1] == "R"))
  sum(exito)
})
mean(simsR)
#b)
simsB = replicate(N, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  exito = all((table(bolas) == 3) & (bolas[1] == "B"))
  sum(exito)
})
mean(simsB) 
#c) 
#### REVISAR

simsC = replicate(100, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  
  aja = table(bolas, exclude = c("B", "A"))

  exito = all((any(bolas == "B")) && (table(bolas, exclude = c("B", "A")) == 2))
  exito
})
sum(simsC) / 100

#d)
simsD = replicate(N, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  exito = any(bolas == "B")
  sum(exito)
})
mean(simsD) 
#e)
simsE = replicate(N, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  exito = all((table(bolas) == 1))
  sum(exito)
})
mean(simsE) 
#f) 
simsF = replicate(N, {
  caja = c(rep("R", 8), rep("B", 3), rep("A", 9))
  bolas = sample(caja, 3)
  exito = all((bolas[1] == "R") & (bolas[2] == "B") & (bolas[3] == "A"))
  sum(exito)
})
mean(simsF) 

#18. Se lanzan dos monedas y se tira un dado tantas veces como caras se hayan obtenido. (a) Halla la
#probabilidad de que la suma de las puntuaciones sea 6. (b) Si sabemos que el resultado del juego es que
#los dados han sumado 6, ¿Cu´al es la probabilidad de haber obtenido 0 caras, 1 cara o 2 caras?

#Para facilitar los calculos, vamos a asumir que las Caras son 1, y los 0 Cruz
#a)
N = 50000
sims = replicate(N,{
  monedas = sample(c(1,0), 2, replace = TRUE)
  lanzamiento = sum(monedas)
  if(lanzamiento != 0){
    dado = sample(c(1:6), lanzamiento, replace = TRUE)
  }else{
    dado = 0
  }
  exito = (sum(dado) == 6)
  sum(exito)
})
resultado6 = mean(sims)

#b) 
#b.1) Evaluamos lo siguiente: Se nos esta preguntando por P(H=0 | r=6), tenemos una probabilidad condicionada 
#Queremos hallar la prob de que hayan salido 0 caras, sabiendo que el resultado final, es 6. Por Bayes podemos decir 
# que P(H=0 | r=6) = P(H=0 Y r=6)/ P(r=6). Sabemos cual es la probabilidad de que el resultado final sea 6, sin 
#embargo, para calcular la intersección, descubrimos que el evento de sacar 0 caras y obtener como resultado final 
#6, son eventos que no pueden ocurrir al mismo tiempo. Esto nos lleva a concluir que, la P(H=0 | r=6) = 0.

#b.2) P(H=1 | r=6) = P(H=1 Y r=6)/ P(r=6)
#Calculamos la intersección entre : Ha salido solo una cara y el resultado ha sido 6
N = 50000
sims = replicate(N,{
  monedas = sample(c(1,0),1)
  monedas
  if(monedas != 0){
    dado = sample(c(1:6), 1)
  }else{
    dado = 0
  }
  exito = (sum(dado) == 6)
  sum(exito)
})
interseccion = mean(sims)

#Aplicamos la formula, y dividimos la intersección entre la prob de obtener como resultado 6.
interseccion / resultado6

#b.3) Repetimos el procedimiento anterior, particularizando para caras = 2
# P(H=2 | r=6) = P(H=2 Y r=6)/ P(r=6)

N = 50000
sim = replicate(N,{
  dado = sample(c(1:6), 2, replace = TRUE)
  exito = (sum(dado) == 6)
  sum(exito)
})
b3 = mean(sim)
b3/resultado6

#19) El 52% de los estudiantes de una universidad son mujeres. El 5% de los estudiantes estudian
#inform´atica. El 2% de los estudiantes son mujeres que estudian inform´atica. Si un estudiante es elegido
#al azar, calcula la probabilidad de que
#(a) el estudiante sea mujer, dado que el estudiante hace inform´atica.
#(b) el estudiante hace inform´atica, dado que el estudiante sea mujer.


