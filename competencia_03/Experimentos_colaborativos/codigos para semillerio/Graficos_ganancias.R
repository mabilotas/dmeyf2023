#cargo librerias

library(tidyverse)
library(ggplot2)

#cargo datasets
resultados_ganancia_baseline<-read_csv("C:/Users/mabil/Documents/DATA_MINING/DMEyF/dmeyf2023/competencia_03/Experimentos_colaborativos/Resultados/BO1_a_resultados_ganancia_baseline.csv")
resultados_ganancia_ensembles<-read_csv("C:/Users/mabil/Documents/DATA_MINING/DMEyF/dmeyf2023/competencia_03/Experimentos_colaborativos/Resultados/BO1_a_resultados_ganancia.csv")
resultados_ganancia_50_ensembles<-read_csv("C:/Users/mabil/Documents/DATA_MINING/DMEyF/dmeyf2023/competencia_03/Experimentos_colaborativos/Resultados/BO1_a_resultados_ganancia_ensembles.csv")

#seteo directorio
setwd("C:/Users/mabil/Documents/DATA_MINING/DMEyF/dmeyf2023/competencia_03/Experimentos_colaborativos/Graficos/")


#------------------------------------------------------------------------------------------
#-------------GANANCIA BASELINE------------------------------------------------------------

resultados_ganancia_baseline$semilla <- as.factor(1:nrow(resultados_ganancia_baseline))

# Scatter plot
scatter_plot <- ggplot(resultados_ganancia_baseline, aes(x = semilla, y = ganancia)) +
  geom_point() +
  labs(x = "Modelos", y = "Ganancia") +
  ggtitle("Ganancias vs Modelos con distinta semilla") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "black"))  # Ajusta el color del texto del eje x

# ggsave("ganancias_modelos_baseline.png", scatter_plot, width = 8, height = 5, units = "in", bg = "white")

# Gráfico de densidad
density_plot <- ggplot(resultados_ganancia_baseline, aes(x = ganancia)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Ganancia", y = "Densidad") +
  ggtitle("Densidad de Ganancias: Baseline") +
  theme_minimal()

# ggsave("distribucion_densidad_baseline.png", density_plot, width = 8, height = 5, units = "in", bg = "white")
# Mostrar los gráficos
print(scatter_plot)
print(density_plot)


#-------------------------------------------------------------------------------------------------
# #---------------------------semillerio GANANCIA VS ENVÍOS---------------------------------------

gg <- ggplot(resultados_ganancia_ensembles, aes(x = envios))

# Agregar las curvas de ganancia para todos los ensembles 
for (i in 2:30) {
  ensemble_col <- paste0("ganancia_", i)
  gg <- gg + geom_line(aes(y = .data[[ensemble_col]]), color = "gray70")
}


# Agregar la curva de ganancia para "ganancia_20" (resaltada en rojo)
gg <- gg + geom_line(aes(y = ganancia_30), color = "red", size = 0.5)

# Agregar la curva de ganancia para "ganancia_2" (resaltada en rojo)
gg <- gg + geom_line(aes(y = ganancia_2), color = "blue", size = 0.5)

# Personalizar etiquetas y temas
gg <- gg +
  labs(x = "Cantidad de Envíos", y = "Ganancia") +
  theme_minimal()

theme_white <- theme_minimal() + theme(panel.background = element_rect(fill = "white"))

# Crear el gráfico con el nuevo tema
gg <- gg + theme_white

# Guardar el gráfico con fondo blanco
ggsave("ganancias_envios.png", gg, width = 8, height = 5, units = "in", bg = "white")


 


#----------------------------------------------------------------------------------------------
#---------GANANCIA BASELINE VS SEMILLERIOS-----------------------------------------------------

# Scatter plot
scatter_plot <- ggplot(resultados_ganancia_50_ensembles, aes(x = ensemble, y = ganancia)) +
  geom_point() +
  labs(x = "Ensemble", y = "Ganancia") +
  ggtitle("Ganancias de los modelos ensembles") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# ggsave("ganancias_modelos_ensemble.png", scatter_plot, width = 8, height = 5, units = "in", bg = "white")

# Gráfico de densidad
density_plot <- ggplot(resultados_ganancia_50_ensembles, aes(x = ganancia)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(x = "Ganancia", y = "Densidad") +
  ggtitle("Densidad de Ganancias: Ensembles") +
  theme_minimal()
# ggsave("distribucion_densidad_ensembles.png", density_plot, width = 8, height = 5, units = "in", bg = "white")

# Mostrar los gráficos
print(scatter_plot)
print(density_plot)



# Scatterplot
scatterplot <- ggplot() +
  geom_point(data = resultados_ganancia_baseline, aes(x = as.numeric(factor(semilla)), y = ganancia, color = "Baseline"), size = 3) +
  geom_point(data = resultados_ganancia_50_ensembles, aes(x = ensemble + length(unique(resultados_ganancia_50_ensembles$ensemble)), y = ganancia, color = "Ensemble"), size = 3) +
  labs(title = "Comparación de Ganancias entre modelos: semillas sueltas & ensembles",
       x = "",  # Oculta las etiquetas del eje x
       y = "Ganancia") +
  theme_minimal() +
  scale_x_continuous(breaks = NULL) +
  scale_color_manual(values = c("Baseline" = "blue", "Ensemble" = "green"))

ggsave("ganancias_modelos_vs.png", scatterplot, width = 8, height = 5, units = "in", bg = "white")
print(scatterplot)
# Gráfico de densidad
density_plot <- ggplot() +
  geom_density(data = resultados_ganancia_baseline, aes(x = ganancia, fill = "Baseline"), alpha = 0.5) +
  geom_density(data = resultados_ganancia_50_ensembles, aes(x = ganancia, fill = "Ensemble"), alpha = 0.5) +
  labs(title = "Distribución de Ganancias",
       x = "Ganancia",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("Baseline" = "blue", "Ensemble" = "green"))

ggsave("distribucion_densidad_vs.png", density_plot, width = 8, height = 5, units = "in", bg = "white")
print(density_plot)