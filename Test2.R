##PREGUNTAS DEL TEXT2

#1) La instalación de algún paquete de software requiere la descarga de 82 archivos. 
#En promedio, se tarda 15 segundos en descargar un archivo, con una varianza de 16 segundos^2. 
#La distribución del tiempo de descarga es acampanada y simétrica. 
#¿Cuál es la probabilidad de que el software se instale en menos de 20 minutos?

82 archivos 
media/Esperanza= 15seg * archv
varianza= 16seg^2

variableAleatoria = Tmp

P(Tmp < 20)


#2)El número medio de pacientes que llega a las urgencias de cierto hospital es 50 personas. 
#Cada paciente tiene probabilidad 0.25 de requerir hospitalización. 
#Calcula mediante simulaciones cuántas camas debe tener el hospital para que la probabilidad 
#de que algún paciente se quede sin cama sea menor que 0.015.

pacientes = 50
hospitalizados = 0.25
#paciente sin cama
PSC = 0.015

#P(PSC < 0.015)

#3)Cierta computadora se encarga del envío de mensajes militares desde un submarino. 
#Debido a las condiciones extremas en las que opera el submarino, un mensaje no llega a su 
#destino con probabilidad 0.7. Por otra parte, y para asegurarse que el contenido del mensaje es correcto, 
#es necesario recibir 3 veces el mensaje para considerar una transmisión como exitosa. 
#¿Cuál es el número esperado de mensajes fallidos hasta conseguir una transmisión exitosa?

nollega = 0.7
X = mensajesEnviados


#4)Un laboratorio de computación tiene dos impresoras. La impresora I maneja el 40% de todos los trabajos.
#Su tiempo de impresión es exponencial con una media de 2 minutos. La impresora II maneja el 60% restante de 
#los trabajos. Su tiempo de impresión es uniforme entre 0 minutos y 5 minutos. Se imprime un trabajo en menos 
#de 1 minuto. ¿Cuál es la probabilidad de que haya sido impreso por la impresora I?

impresora1 = 0.40
tmpI1 = exp() # en media 2 minutos
impresora2 = 0.60
tmpI2 = 0-5 minutos 
  
#5)Si tomas la píldora roja, el próximo invierno tendrás, de media, 1 resfriado.  
#Si tomas la pastilla azul, el número medio  de resfriados será 4. La elección  de la pastilla es 
#aleatoria y equiprobable. Supongamos que el próximo invierno tienes tres resfriados. 
#¿Cuál es la probabilidad de que hayas tomado la pastilla azul?  
  
roja = media 1 resfriado
azul = media 4

3 resfriados 

P(A|3R) = P(A Y 3R) / P(3R)

#Para facilitar los calculos, vamos a asumir que la píldora roja es 1, y la azul 0. 
N = 50000 
sims = replicate(N,{
  pildora = sample(0:1, 1)
  if(pildora == 1){
    resfriado = sample(0:1,1,  prob = c(0.25,0.75))
  }else{
    resfriado = sample(2:4,1,  prob = c(0.25,0.25,0.50))  
  }
  exito = (resfriado == 3)
  sum(exito)
})

P3R = mean(sims)

N = 50000 
sim = replicate(N,{
  resfriado = sample(2:4,1,  prob = c(0.25,0.25,0.50))  
  resfriado
  exito = (resfriado == 3)
  sum(exito)
})

PAY3R = mean(sim)

PAY3R/P3R



1: 0.5673926
2: 0.7611297
3: 20
4: 0.2037683
5: 7
  
  
  