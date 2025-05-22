# ========================
# Introducción a los Tibbles y análisis del conjunto de datos Gapminder
# ========================

# Instalamos y cargamos los paquetes necesarios
install.packages("tibble")
install.packages("dplyr")
install.packages("gapminder")
install.packages("ggplot2")


library(tibble)
library(dplyr)  # Para manipulación de datos con tablas tipo tibble
library(gapminder) #dataset tipo tibble
library(ggplot2)  # Para visualización de datos

# ======================================================

# 🟢 1. Crear un tibble desde cero
mi_tibble <- tibble(
  Nombre = c("Ana", "Carlos", "Beatriz"),
  Edad = c(25, 30, 22),
  Puntaje = c(88.5, 92.3, 79.6)
)

# Mostrar el tibble
print(mi_tibble)

# Acceder a una columna
# Usando el nombre de la columna
mi_tibble$Nombre[1]
promedio <- mean(mi_tibble$Puntaje)
print(promedio)
mean(mi_tibble$Edad)
max(mi_tibble$Edad)

# Comparar con un data.frame clásico
mi_dataframe <- data.frame(
  Nombre = c("Ana", "Carlos", "Beatriz"),
  Edad = c(25, 30, 22),
  Puntaje = c(88.5, 92.3, 79.6)
)

# Mostrar el data.frame
print(mi_dataframe)

# 2. Conversión entre tibble y data.frame

# De tibble a data.frame
df <- as.data.frame(mi_tibble)

# De data.frame a tibble
tb <- as_tibble(mi_dataframe)

# Verificar clases
class(df)  # "data.frame"
class(tb)  # "tbl_df" "tbl" "data.frame"

# ========================
# Gapminder: análisis estadístico y visualización
# ========================

# Ver los primeros registros del conjunto de datos
print(gapminder)

# Ver estructura con glimpse (dplyr)
glimpse(gapminder)

# 3. Estadísticas resumidas por continente (año más reciente: 2007)
resumen <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarise(
    esperanza_vida_promedio = mean(lifeExp),
    pib_percapita_promedio = mean(gdpPercap),
    poblacion_total = sum(pop)
  )

print(resumen)

gapminder_df <- as.data.frame(gapminder)
gapminder_df
rm(gapminder_df)
# ========================
# 4. Visualización con ggplot2
# ========================

# Diagrama de dispersión PIB per cápita vs esperanza de vida (2007)
gapminder %>%
  filter(year == 2007) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +  # Escala logarítmica para PIB per cápita
  theme_minimal() +
  labs(
    title = "Esperanza de vida vs PIB per cápita (2007)",
    x = "PIB per cápita (escala logarítmica)",
    y = "Esperanza de vida"
  )

# ========================
# 5. Esperanza de vida a lo largo del tiempo (países seleccionados)
# ========================

# Selección de países
paises <- c("Colombia", "Brasil", "United States", "China")

gapminder %>%
  filter(country %in% paises) %>%
  ggplot(aes(x = year, y = lifeExp, color = country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Evolución de la esperanza de vida",
    x = "Año",
    y = "Esperanza de vida"
  )


# 6. Guardar el tibble en un archivo CSV
write.csv(gapminder, "gapminder.csv", row.names = FALSE)

# 7. Leer el archivo CSV de nuevo
gapminder_leido <- read.csv("gapminder.csv")

# 8. Verificar el tipo de datos en de gapminder_leido. Convertir a tibble

class(gapminder_leido)
gapminder_leido <- as_tibble(gapminder_leido)
class(gapminder_leido)


#Cuantos continentes considera gapminder  ---> 5 continentes
gapminder %>%
  group_by(continent) %>%
  summarise(numero_cont = n_distinct(continent))

gapminder %>%
  summarise(numero_cont = n_distinct(continent))

#Cuantos países hay en el dataset gapminder  ---> 142 países
gapminder %>%
  summarise(numero_pais = n_distinct(country))


#Cuantos países considera gapminder por continente
gapminder %>%
  group_by(continent) %>%
  summarise(paises = n_distinct(country))