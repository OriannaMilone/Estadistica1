
### PREGUNTAS DEL TEST 1
# 1. Tenemos dos urnas: la urna I y la urna II. La urna I contiene tres bolas blancas 
#y cuatro negra39
1-(casos_fav/casos_totales)

no_coincidencias = casos_fav / casos_totales
al_menos_unaC = 1- no_coincidencias


# 3. Se está desarrollando una nueva mascarilla contra el COVID-19. 
#La mascarilla está formada por N capas sucesivas que filtran el virus del COVID-19. 
#Un virus de COVID-19 atraviesa una capa de la mascarilla con probabilidad 0.6. 
#¿Cuántas capas son necesarias para que la probabilidad de que un virus de COVID-19 
#atraviese todas las capas de la mascarilla sea, como máximo, del 0.1%?

#La prob de que atraviese 1 capa es de 0.6
#La prob de que atraviese 2 capas será 0.12
#La prob de que atravierse 3 capas será 0.18

#0.6^ncapas 
0.6^13


P(atravesarNcapas) <= 0.001 

i = 1
capas = c(1:14)

for(i in capas){
	  tmp = 0.6 ^ i
  if(tmp <= 0.001){
	      cap = i
    }
}
cap


# 4. Lanzamos varias veces una moneda trucada (la probabilidad de cara es 0.4) hasta que salen 2 caras. 
#¿Cuál es la probabilidad de que se necesiten exactamente 10 lanzamientos para obtener las dos caras? 
#En caso de que quieras resolver el ejercicio por simulaciones, usa el argumento prob de sample().

N = 50000

simulacion = replicate(N, {
			         n_lanzam = 0 
				   n_caras = 0
				   while(n_caras != 2){
					       lanzam = sample(c(1, 0), 1, prob = c(0.4, 0.6))
				       n_caras = n_caras + lanzam
				           n_lanzam = n_lanzam + 1
				         }
				     exito = (n_lanzam == 10)
				     sum(exito)
})
sum(simulacion) / N

# 5. Se sospecha que cierto sorteo de una prestigiosa competición de fútbol se ha amañado. 
#Se han emparejado 8 equipos de fútbol para las eliminatorias, 
#pero mucha gente cree que el sorteo no ha sido justo porque cada equipo fuerte ha sido emparejado 
#con un equipo débil (sospechosamente, para la UEFA esto es lo más interesante desde un punto de vista económico).
#¿Cuál es la probabilidad de que los 4 mejores equipos queden emparejados con los 4 peores equipos? 
#Ten en cuenta que las eliminatorias son a ida y vuelta, por lo que además de los emparejamientos 
#ha de decidirse qué equipo comienza a jugar como local.

#Mejores son del 1-4, peores del 5-8

N = 50000

simulacion = replicate(N, {
			         equipos = c(1:8)
				   trampa = 0
				   x = c(1, 2, 3, 4)
				     for(e in x){
					         rondaI = sample(equipos, 2)
				       casosI = !(all(rondaI >= 5) | all(rondaI <= 4))
				           equipos = setdiff(sample(equipos), rondaI)
				           trampa = trampa + sum(casosI)
					      
					     }
				     sum(trampa_total = (trampa == 4))
})

(sum(simulacion)/N)
s mientras que la urna II está inicialmente vacía. Extraemos dos bolas al
#azar de la urna I y, sin mirar su color, las ponemos dentro de la urna II. A continuación 
#cogemos al azar una bola de la urna I y otra bola al azar de la urna II. 
#¿Cuál es la probabilidad de que ambas sean blancas?

#Supondremos que las bolas del 1-3 son blancas y las bolas del 4-7 son negras

N = 5000
sims = replicate(N,{
			   urna1 = 1:7
			     urna2 = sample(urna1, 2)
			     urna1 = setdiff(urna1, urna2)
			       
			       s_1 = sample(urna1,1)
			       s_2 = sample(urna2,1)
			         sum(all((s_1 < 4),(s_2 < 4)))
}
)
mean(sims)
#sum(simulacion)/N


# 2.Hay 39 ex-presidentes de EEUU que han fallecido. ¿Cuál es la probabilidad de que al
#menos 2 de ellos hayan muerto el mismo dı́a? (Asume que un año tiene 365 dı́as).

calendario = 365
bajas = sample(calendario, 39)

casos_fav = variaciones(365, 39) #De cuantas maneras distintas puedo tener 39 bajas en un año
casos_totales = 365^
