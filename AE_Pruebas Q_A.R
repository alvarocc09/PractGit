
          ## PRUEBAS PARAMETRICAS PARA Q_A ##

        # VARIABLE CANTIDAD DE GRAMOS DE ALCOHOL #



  # Prueba de NORMALIDAD de KOLMOGOROV-SMIRNOV
lillie.test(datos$Q_A) # Sí existe normalidad p=0.507
hist(datos$Q_A,
     main = "Histograma de Gramos de alcohol",
     xlab = "")




  # Prueba de HOMOGENEIDAD de las varianzas
var.test(datos$Q_A ~ datos$Sexo) # No existe diferencia
var.test(datos$Q_A ~ datos$L_resid) # Sí existe diferencia
var.test(datos$Q_A ~ datos$Sit_lab) # No existe diferencia

pv <- c(tf1$p.value, tf2$p.value, tf3$p.value)
st <- c(tf1$statistic, tf2$statistic, tf3$statistic)
n <- c("Sexo","Lugar de residencia","Situación laboral")



  # Prueba T de Student

#Sexo
t.test(datos$Q_A ~ datos$Sexo, var.eq = F)
boxplot(datos$Q_A ~ datos$Sexo,
        main = "Gramos de alcohol a la semana diferenciando por sexo",
        ylab = "Gramos",
        xlab = "")
group_by(datos, Sexo) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

#L_resid
t.test(datos$Q_A ~ datos$L_resid, var.eq = T)
boxplot(datos$Q_A ~ datos$L_resid,
        main = "Gramos de alcohol a la semana por lugar de residencia",
        ylab = "Gramos",
        xlab = "")
group_by(datos, L_resid) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

#Sit_lab
t.test(datos$Q_A ~ datos$Sit_lab, var.eq = F)
boxplot(datos$Q_A ~ datos$Sit_lab,
        main = "Gramos de alcohol a la semana por situcación laboral",
        ylab = "Gramos",
        xlab = "")
group_by(datos, Sit_lab) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

# Graficar T de Student de Q_A y Sexo
ggstatsplot::ggbetweenstats(
  data = datos,
  x = Sexo,
  y = Q_A,
  xlab = "", # etiqueta eje x
  ylab = "Gramos de alcohol",
  type = "p", # eliges entre prueba parametrica (p), no param. (np), robusta (r) o bayesiana (bf)
  effsize.type = "g", # tipo de estimador de efectos: g o d
  conf.level = 0.95,
  plot.type = "box", # puede ser por separado box y violin
  outlier.tagging = TRUE, # Te indica cuales son outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  #outlier.label = IIEF, # La etiqueta que va a tener los outliers
  outlier.label.args = list(color = "red"), # la etiqueta de los outlairs en rojo
  messages = FALSE, # apagar los mensajes, ¿qué mensajes?, ni idea. 
  ggtheme = ggplot2::theme_grey(), # cambiar el fondo de la gráfica todos los temas en https://ggplot2.tidyverse.org/reference/ggtheme.html
  package = "yarrr", # El paquete asociado a la paleta de colores.
  palette = "info2", # Elegir la paletta dentro del paquete 
  title = "Gramos de alcohol semanales diferenciando por sexo",
  caption = ""
)

  


# Prueba ANOVA

#Titulacion
anova_tit <- aov(datos$Q_A ~ datos$Titulacion)
summary(anova_tit)
boxplot(datos$Q_A ~ datos$Titulacion,
        main = "Gramos de alcohol a la semana por titulación",
        ylab = "Gramos",
        xlab = "")
group_by(datos, Titulacion) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anova_tit)
plot(TukeyHSD(anova_tit))

#T_resid
anova_T_resid <- aov(datos$Q_A ~ datos$T_resid)
summary(anova_T_resid)
boxplot(datos$Q_A ~ datos$T_resid,
        main = "Gramos de alcohol a la semana por tipo de residencia",
        ylab = "Gramos",
        xlab = "")
group_by(datos, T_resid) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anova_T_resid)
plot(TukeyHSD(anova_T_resid))

#Salud
anova_Salud <- aov(datos$Q_A ~ datos$Salud)
summary(anova_Salud)
boxplot(datos$Q_A ~ datos$Salud,
        main = "Gramos de alcohol a la semana por salud",
        ylab = "Gramos",
        xlab = "")
group_by(datos, Salud) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

#Info
anova_Info <- aov(datos$Q_A ~ datos$Info)
summary(anova_Info)
boxplot(datos$Q_A ~ datos$Info,
        main = "Gramos de alcohol a la semana por información",
        ylab = "Gramos",
        xlab = "")
group_by(datos, Info) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

#Padres_A
anova_P_A <- aov(datos$Q_A ~ datos$P_A)
summary(anova_P_A)
boxplot(datos$Q_A ~ datos$P_A,
        main = "Gramos de alcohol a la semana por padres bebedores",
        ylab = "Gramos",
        xlab = "")
group_by(datos, P_A) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )

#Padres_T
anova_P_T <- aov(datos$Q_A ~ datos$P_T)
summary(anova_P_T)
boxplot(datos$Q_A ~ datos$P_T,
        main = "Gramos de alcohol a la semana por padres fumadores",
        ylab = "Gramos",
        xlab = "")
group_by(datos, P_T) %>%
  summarise(
    Count = n(),
    Media = mean(Q_A, na.rm = T),
    Desv = sd(Q_A, na.rm = T),
    Mediana = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )





  # Correlacion de Spearman

sapply(datos, function(x) sum(is.na(x)))


# 1) Edad, Cursos, Ps(x6), Edad_A, Punt_A 
datos_corrQ_A <- data.frame(datos$Q_A, datos$Punt_A,
                            datos$Edad, datos$Cursos,
                            datos$Ps, datos$Pl, datos$Pr, datos$Pp, datos$Pd,
                            datos$Pe,
                            datos$Edad_A)
sapply(datos_corrQ_A, function(x) sum(is.na(x)))
datos_corrQ_A <- datos_corrQ_A[!is.na(datos_corrQ_A$datos.Q_A),]
str(datos_corrQ_A)

corr_Q_A <- cor(datos_corrQ_A, method = "spearman")
corr_Q_A

apa.cor.table(datos_corrQ_A, filename = "Correlacion_Q_A_TFG.doc",
              table.number = 1) # OJO, NO ES LA CORRELACION DE SPEARMAN

pairs.panels(datos_corrQ_A, method = "spearman", pch = 20, stars = T,
             main = "Correlacion_Q_A")
chart.Correlation(datos_corrQ_A, method = "spearman")

corrplot(corr_Q_A)
corrplot(corr_Q_A, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black")



# 2) Q_T, Edad_T, Punt_T 
datos_corrQ_A2 <- data.frame(datos$Q_A, datos$Punt_A, datos$Edad_A,
                             datos$Q_T, datos$Edad_T, datos$Punt_T)
datos_corrQ_A2 <- datos_corrQ_A2[!is.na(datos_corrQ_A2$datos.Q_T),]
datos_corrQ_A2 <- datos_corrQ_A2[!is.na(datos_corrQ_A2$datos.Q_A),]
sapply(datos_corrQ_A2, function(x) sum(is.na(x)))
str(datos_corrQ_A2)

corr_Q_A2 <- cor(datos_corrQ_A2, method = "spearman")
corr_Q_A2

apa.cor.table(datos_corrQ_A2, filename = "Correlacion_Q_A2_TFG.doc",
              table.number = 1) # OJO, NO ES LA CORRELACION DE SPEARMAN

pairs.panels(datos_corrQ_A2, method = "spearman", pch = 20, stars = T,
             main = "Correlacion_Q_A")


corrplot(corr_Q_A2)
corrplot(corr_Q_A2, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black")



# 3) Punt_C
datos_corrQ_A3 <- data.frame(datos$Q_A, datos$Punt_A, datos$Edad_A,
                             datos$Punt_C)
datos_corrQ_A3 <- datos_corrQ_A3[!is.na(datos_corrQ_A3$datos.Punt_C),]
datos_corrQ_A3 <- datos_corrQ_A3[!is.na(datos_corrQ_A3$datos.Q_A),]
sapply(datos_corrQ_A3, function(x) sum(is.na(x)))
str(datos_corrQ_A3)

corr_Q_A3 <- cor(datos_corrQ_A3, method = "spearman")
corr_Q_A3

apa.cor.table(datos_corrQ_A3, filename = "Correlacion_Q_A3_TFG.doc",
              table.number = 1) # OJO, NO ES LA CORRELACION DE SPEARMAN

pairs.panels(datos_corrQ_A3, method = "spearman", pch = 20, stars = T,
             main = "Correlacion_Q_A")

table(datos$`A/T_C`)





  # ANOVA para la Q_A respecto la Frecuencia y el Gasto

# F_A
F_A <- factor(datos$F_A, ordered = T)
str(F_A)

anov_F_A <- aov(datos$Q_A ~ F_A)
summary(anov_F_A)
boxplot(datos$Q_A ~ F_A, main = "Cantidad de alcohol semanal por nivel de frecuencia de alcohol",
        xlab = "Frecuencia", ylab = "Gramos")
group_by(datos, F_A) %>% 
  summarise(
    Count = n(),
    Mean = mean(Q_A, na.rm = T),
    Sd = sd(Q_A, na.rm = T),
    Median = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anov_F_A)
plot(TukeyHSD(anov_F_A))


# F_T
F_T <- factor(datos$F_T, ordered = T)
str(F_T)

anov_F_T <- aov(datos$Q_A ~ F_T)
summary(anov_F_T)
boxplot(datos$Q_A ~ F_T, main = "Cantidad de alcohol semanal por nivel de frecuencia de tabaco",
        xlab = "Frecuencia", ylab = "Gramos")
group_by(datos, F_T) %>% 
  summarise(
    Count = n(),
    Mean = mean(Q_A, na.rm = T),
    Sd = sd(Q_A, na.rm = T),
    Median = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )


# F_C
F_C <- factor(datos$F_C, ordered = T)
str(F_C)

anov_F_C <- aov(datos$Q_A ~ F_C)
summary(anov_F_C)
boxplot(datos$Q_A ~ F_C, main = "Cantidad de alcohol semanal por nivel de frecuencia de cachimba",
        xlab = "Frecuencia", ylab = "Gramos")
group_by(datos, F_C) %>% 
  summarise(
    Count = n(),
    Mean = mean(Q_A, na.rm = T),
    Sd = sd(Q_A, na.rm = T),
    Median = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anov_F_C)
plot(TukeyHSD(anov_F_C))


# G_A
G_A <- factor(datos$G_A, ordered = T)
str(G_A)

anov_G_A <- aov(datos$Q_A ~ G_A)
summary(anov_G_A)
boxplot(datos$Q_A ~ G_A, main = "Cantidad de alcohol semanal por nivel de gasto de alcohol",
        xlab = "Nivel de gasto", ylab = "Gramos")
group_by(datos, G_A) %>% 
  summarise(
    Count = n(),
    Mean = mean(Q_A, na.rm = T),
    Sd = sd(Q_A, na.rm = T),
    Median = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anov_G_A)
plot(TukeyHSD(anov_G_A))


# G_T
G_T <- factor(datos$G_T, ordered = T)
str(G_T)

anov_G_T <- aov(datos$Q_A ~ G_T)
summary(anov_G_T)
boxplot(datos$Q_A ~ G_T, main = "Cantidad de alcohol semanal por nivel de gasto de tabaco",
        xlab = "Nivel de gasto", ylab = "Gramos")
group_by(datos, G_T) %>% 
  summarise(
    Count = n(),
    Mean = mean(Q_A, na.rm = T),
    Sd = sd(Q_A, na.rm = T),
    Median = median(Q_A, na.rm = T),
    IQR = IQR(Q_A, na.rm = T)
  )
TukeyHSD(anov_G_T)
plot(TukeyHSD(anov_G_T))