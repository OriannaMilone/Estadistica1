##PREGUNTAS DEL TEXT2

#1) La instalación de algún paquete de software requiere la descarga de 82 archivos. 
#En promedio, se tarda 15 segundos en descargar un archivo, con una varianza de 16 segundos^2. 
#La distribución del tiempo de descarga es acampanada y simétrica. 
#¿Cuál es la probabilidad de que el software se instale en menos de 20 minutos?

#82 archivos 
#media= 15seg * archv
#varianza = 16seg^2

#Variable aleatoria es tiempo de descarga
pnorm(1200, 1230, 4 * sqrt(82))
# == 0.2037683

#2)El número medio de pacientes que llega a las urgencias de cierto hospital es 50 personas. 
#Cada paciente tiene probabilidad 0.25 de requerir hospitalización. 
#Calcula mediante simulaciones cuántas camas debe tener el hospital para que la probabilidad 
#de que algún paciente se quede sin cama sea menor que 0.015.

# Variable Aleatoria: X = Pacientes sin cama
# la prob de que todos tengan cama, es 0.985 (1-0.015)
# pacientes = 50 como media
# hospitalizados = 0.25 ~ 12-13

N = 50000
pacientes = (rpois(N, 50))
hospitalizados = mean(rbinom(N, pacientes, 0.25))
mean(rbinom(N,13,(0.015)))

# = 19

#3)Cierta computadora se encarga del envío de mensajes militares desde un submarino. 
#Debido a las condiciones extremas en las que opera el submarino, un mensaje no llega a su 
#destino con probabilidad 0.7. Por otra parte, y para asegurarse que el contenido del mensaje es correcto, 
#es necesario recibir 3 veces el mensaje para considerar una transmisión como exitosa. 
#¿Cuál es el número esperado de mensajes fallidos hasta conseguir una transmisión exitosa?

#Distribución binomial negativa 
#r, 3
#p, 0.7

mean(rnbinom(x, size = 3, prob = 1-0.7))

# == 7.0

#4)Un laboratorio de computación tiene dos impresoras. La impresora I maneja el 40% de todos los trabajos.
#Su tiempo de impresión es exponencial con una media de 2 minutos. La impresora II maneja el 60% restante de 
#los trabajos. Su tiempo de impresión es uniforme entre 0 minutos y 5 minutos. Se imprime un trabajo en menos 
#de 1 minuto. ¿Cuál es la probabilidad de que haya sido impreso por la impresora I?

#Variable aleatoria: Tiempo de impresión x
impresora1 = 0.40
tmpI1 = pexp(1, 1/2) # lambda = 1/ media --> Lambda es el arg de la exp

impresora2 = 0.60
tmpI2 = punif(1, 0, 5) 

#P(I1|x<1) = P(x<1|I1) P(I1) / P(x<1|I1) P(I1) + P(x<1|I2) P(I2)

(tmpI1 * 0.4) /((tmpI1 * 0.4) + (tmpI2 *0.6))

# == 0.56

#5)Si tomas la píldora roja, el próximo invierno tendrás, de media, 1 resfriado.  
#Si tomas la pastilla azul, el número medio  de resfriados será 4. La elección  de la pastilla es 
#aleatoria y equiprobable. Supongamos que el próximo invierno tienes tres resfriados. 
#¿Cuál es la probabilidad de que hayas tomado la pastilla azul?    -- 0.7611

# P(A|3R) = P(A Y 3R) / P(A Y 3R) + P(R Y 3R) 
# P(A|3R) = P(0.5 * 3R) / P(0.5 * 3R) + P(0.5 * 3R)

0.5 * dpois(3, 4) /(0.5 * dpois(3, 4) +  0.5 * dpois(3, 1))

# == 0.7611





1: 0.5673926  listo
2: 0.7611297 listo
3: 20 listo
4: 0.2037683 listo
5: 7 listo
  
  
  