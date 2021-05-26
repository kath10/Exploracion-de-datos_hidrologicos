# Datos hidrológicos, ejercicio explorativo



### Importar los datos hidrológicos del archivo FDC 
inp <- read.csv("FDC.csv", na.strings="")
inp <- read.csv("FDC.csv")


###Visualizar datos e información del archivo importado
head(inp)
dim(inp)



inp[!complete.cases(inp),]

# newinp <- na.omit(inp)
  ### Gráfico de líneas del hidrograma de lo ríos de Estrella y Banano
plot(
  inp[,2], 
  type = "l", col="navy blue",
  main = ("Hidrograma de los r?os Estrella y Banano"),
  xlab = ("Tiempo"),
  ylab = ("Nivel del caudal")
)
  lines(inp[,3], col= "gray")
  

summary(inp[,2:3])  
hist(inp[,2])
hist(inp[,3])


### Gr?fico de barras del histograma del r?o Estrella
plot(
  hist(inp[,2]),
  main = ("Histograma de Estrella"),
  height = inp$Estrella,
  names.arg = inp$Fecha,
  xlab = ("Agua por d?a"),
  ylab = ("Nivel del caudal"),
  col = "Brown"
)


### Gr?fico de barras del histograma del r?o Banano
plot(
  hist(inp[,3]),
  main = ("Histograma de Banano"),
  names.arg = inp$Fecha,
  xlab = ("Agua por d?a"),
  ylab = ("Nivel del caudal"),
  col = "pink"
)

## Determinar nombres a la columnas 1,2 y3 con los nombres de Fecha, Estrella y Banano
names(inp) <- c("Fecha", "Estrella", "Banano")

### Gr?fico de puntos del r?o Estrella
plot(
  inp$Estrella,
  main = "Tiempos",
  xlab = "Tiempo",
  ylab = "Nivel de caudal",
  col = "Orange"
)

### Gr?fico de puntos del r?o Banano
plot(
  inp$Banano,
  main = "Tiempos",
  xlab = "Tiempo",
  ylab = "Nivel de caudal",
  col = "violet"
)


### Conversi?n de la columna de Fecha a la clase Date
Tempdate <- strptime(inp[,1], format = "%d/%m/%Y")
### Determinar el c?lculo del caudal de manera anual del r?o Estrella y Banano entre los a?os de 1973 a 1983
MAQ_Estrella <- tapply(inp[,2], format(Tempdate, format = "%Y"), FUN = sum)
MAQ_Banano <- tapply(inp[,3], format(Tempdate, format= "%Y"), FUN = sum)
### Exportar los archivos de MAQ Estrella y MAQ Banano en un mismo archivo para el directorio correspondiente 
write.csv(rbind(MAQ_Estrella,MAQ_Banano), file="MAQ.csv")


### Gráfico de líneas y puntos de los caudales del río Estrella y Banano
plot(MAQ_Banano, 
    ylim=c(100,3000),
    xlab= ("Tiempo"),
    ylab= ("Nivel del caudal"),
    main = ("Comparaci?n de los niveles de los caudales de los r?os en el lapso de los a?os 1973 al 1983"),
    col="dark green"
  )
lines(MAQ_Estrella, col="maroon")

### Determinar el c?lculo del caudal de manera mesual del r?o Estrella y Banano
MMQ_Estrella <- tapply(inp[,2], format(Tempdate, format= "%m"), FUN=sum)
MMQ_Banano <- tapply(inp[,3], format(Tempdate, format= "%m"), FUN=sum)
write.csv(rbind(MMQ_Estrella, MMQ_Banano), file="MMQ.csv")
          

# Anáisis de correlación
corinp <- cor(inp[,2:3], method= "spearman")
corinp

# Gráfico de la relación dada entre los caudales de río Estrella y río Banano
plot(
  inp$Estrella, 
  inp$Banano, 
  xlab = ("Estrella"),
  ylab = ("Banano"),
  main = ("Relación de los caudales de r?o Banano y r?o Estrella"),
)

### Gráficos de correlación de los ríos Estrella y Banano
```{r pressure, echo=FALSE}
inp.lm <- lm(inp[,2]~inp[,3], data=inp)
summary(inp.lm)
plot(inp.lm)
```




