library(tidyverse)
library(forcats)
library(gganimate)
library(gifski)
library(dplyr)
library(ggridges)

df <- read.csv("movies.csv")


# Plan 


# Checklist
problems(df)
sum(is.na(df))
df %>% summarise(
                 distintos=n_distinct(df),
                 total=n(),
                 repetidas=total-distintos
                 )



glimpse(df)
df$name = as.character(df$name)
df$director = as.character(df$director)
df$writer = as.character(df$writer)
df$star = as.character(df$star)

#df con 2 columnas mas: ganancia(ganancia - costo) y rentable(>=2*costo)
dfr <- df %>%
  mutate(ganancia=gross-budget) %>%
  mutate(rentable = (ganancia >= 2*budget))
dfr$rentable <- as.integer(dfr$rentable)
dfr <- dfr[!is.na(dfr$budget),]
dfr <- dfr[!is.na(dfr$gross),]


  

#Dfr solo con peliculas rentables

df_rentables <- dfr %>%
  filter(rentable == TRUE)

# cantidad de Peliculas mas rentables por genero

ggplot(data=df_rentables) +
  geom_bar(aes(x=genre, fill=genre)) +
  labs(title='las peliculas mas rentables', subtitle='osea sume las ganancias de todo el genero y lo reste por los costos')

# boxplot presupuesto medio de cada genero

presupuesto <- dfr %>%
  filter(genre %in% generos_rentables$genre)
  
ggplot(presupuesto,aes(reorder(genre, -budget), budget, fill=genre)) +
  geom_violin() + 
  labs(title='Presupuesto de los 10 generos más rentables',
       x='Género',
       y='Presupuesto')

# generos mas rentables

generos_rentables <- dfr %>%
  group_by(genre) %>%
  summarise(total=n(), rentables=sum(rentable, na.rm=TRUE), prom=round((rentables/total)*100,2)) %>%
  arrange(desc(prom)) %>%
  filter(total>10) %>%
  head(10)

ggplot(generos_rentables) +
  geom_col(aes(y=reorder(genre, prom), prom, fill=genre)) +
  geom_text(aes(y=genre, x=prom, label=paste0(prom,'%')),
            hjust = 1.1,
            color='white') +
  labs(title='Generos más rentables en porcentaje',
       x='Porcentaje',
       y='Generos')

# Density presupuesto medio de cada genero
presupuesto <- dfr %>%
  filter(genre %in% generos_rentables$genre)

ggplot(presupuesto, aes(x=budget, y=reorder(genre, budget), fill=genre)) +
  geom_density_ridges(alpha=0.8) +
  labs(title='Presupuesto de los 10 generos más rentables',
       x='Presupuesto en (U$D)',
       y='Género') +
  theme_minimal() +
  scale_x_continuous(label = function(x) paste0(x/1000000, 'M'),
                     limits = c(0,100000000)) +
  theme(legend.position = 'none')

# Hacer zoom y comparar presupuesto medio de horror y accion

# Hay un outlier!

# Ahora filtrar por las 50 mejores peliculas(por score y 2004), hacer un innerjoin
# y comparar ganancia por genero sin filtro y con filtro



# Ganancia a travez de los años

años <- dfr %>%
  group_by(year) %>%
  summarise(ganancia=mean(ganancia), presupuesto=mean(budget), total=n())



ggplot(años) +
  geom_line(aes(x=year, y=ganancia), color='green2') +
  geom_line(aes(x=year, y=presupuesto), color='#FF0000') +
  geom_point(aes(x=year, y=ganancia), color='green2') +
  geom_point(aes(x=year, y=presupuesto), color='#FF0000') +
  theme_minimal() +
  labs(title='Promedio de ganancias y costos de las peliculas a través de los años',
       x='Años',
       y='Ganancias (en U$D)',
       fill='a',
       color='b') +
  scale_y_continuous(label = function(x) paste0(x/1000000, 'M')) +
  transition_reveal(year)
  
  
a

animate(a, height = 600, width =600)






#                CALCULAR PROMEDIO

# Actores si queres rentabilidad

actor <- dfr %>%
  group_by(star) %>%
  summarise(total=n(), rentable=sum(rentable), prom=round((rentable/total)*100,2), ganancia=sum(gross)/total) %>%
  filter(total > 10) %>%
  arrange(desc(prom)) %>%
  head(10)

ggplot(actor, aes(y=reorder(star, prom), x=prom, fill=star)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(y=star, x=prom, label=paste0(prom,'%')),
            hjust = 1.1,
            color='white') +
  labs(x='porcentaje',
       y='actor',
       title='Actores que mas porcentaje de peliculas rentables realizó') +
  theme(legend.position = 'none')

# Actores ganancias 
actor2 <- dfr %>%
  filter(star %in% actor$star) %>%
  group_by(star) %>%
  summarise(total=n(), rentable=sum(rentable), prom=round((rentable/total)*100,2), ganancia=gross/total)

ggplot(actor2, aes(reorder(star, -ganancia), ganancia, fill=star)) +
  geom_boxplot() +
  geom_jitter()
  


# Actores rentables a traves de los años


# Directores si queres rentabilidad

director <- dfr %>%
  group_by(director) %>%
  summarise(total=n(), rentable=sum(rentable),prom=round((rentable/total)*100,2)) %>%
  filter(total >= 7) %>%
  arrange(desc(prom)) %>%
  head(10)

ggplot(director, aes(y=reorder(director, prom), x=prom, fill=director)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(y=director, x=prom, label=paste0(prom,'%')),
            hjust = 1.1,
            color='white') +
  labs(x='Porcentaje',
       y='Director',
       title='Directores que mas porcentaje de peliculas rentables realizó') +
  theme(legend.position = 'none')

ggplot(director, aes(y=reorder(director, rentable), x=rentable, fill=director)) +
  geom_col() +
  theme_minimal() +
  geom_text(aes(y=director, x=rentable, label=paste0(rentable,'')),
            hjust = 1.5,
            color='white') +
  labs(x='Cantidad de peliculas rentables',
       y='Director',
       title='Directores que más peliculas rentables realizaron') +
  theme(legend.position = 'none')


#


# Escritor si queres rentabilidad 

escritor <- dfr %>%
  group_by(writer) %>%
  summarise(total=n(), rentable=sum(rentable),prom=round((rentable/total)*100,2)) %>%
  filter(total > 10) %>%
  arrange(desc(prom)) %>%
  head(20)
  

# Escritor y director que mas peliculas rentables hicieron

dir_esc <- dfr %>%
  filter(writer == director) %>%
  group_by(writer) %>%
  summarise(total=n(), rentable=sum(rentable),prom=round((rentable/total)*100,2)) %>%
  filter(total > 2) %>%
  arrange(desc(prom)) %>%
  head(10)
  
# Paises que mas peliculas rentables hicieron
  
paises <- dfr %>%
  group_by(country) %>%
  summarise(total=n(), rentable=sum(rentable),prom=round((rentable/total)*100,2)) %>%
  arrange(desc(total)) %>%
  head(100)
  

# Compañia que mas peliculas rentables hizo

compañias <-  df_rentables %>%
  group_by(company) %>%
  summarise(total=n()) %>%
  arrange(desc(total))

# Compañias rentables a travez de los añois




_#------
generos <- dfr %>%
  group_by(genre) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=sum()) %>%
  arrange(desc(rentabilidad)) %>%
  head(10)
cinco_años <- df %>%
  filter(year>2017) %>%
  group_by(genre) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=ganado-costo) %>%
  arrange(desc(rentabilidad)) %>%
  head(10)

compañias <- df %>%
  group_by(company) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=ganado-costo) %>%
  arrange(desc(rentabilidad)) %>%
  head(5)

compañias_5 <- df %>%
  filter(year>2017) %>%
  group_by(company) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=ganado-costo) %>%
  arrange(desc(rentabilidad)) %>%
  head(5)

actor_rentable <- df %>%
  group_by(star) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=ganado-costo) %>%
  arrange(desc(rentabilidad)) %>%
  head(5)

actor_rating <- df %>%
  group_by(star) %>%
  summarise(total=n(), rating=mean(score)) %>%
  filter(total>10) %>%
  arrange(desc(rating)) %>%
  head(5)

directores <- df %>%
  group_by(director) %>%
  summarise(total=n(), costo=sum(budget, na.rm=T), ganado=sum(gross, na.rm=T) , rentabilidad=ganado-costo) %>%
  arrange(desc(rentabilidad)) %>%
  head(5)

directores_rating <- df %>%
  group_by(director) %>%
  summarise(total=n(), rating=mean(score)) %>%
  filter(total>10) %>%
  arrange(desc(rating)) %>%
  head(5)



# filtrado df por los 5 directores con mas rating


df_dir <- df %>%
  filter(director %in% c('Christopher Nolan',
                   'Peter Jackson',
                   'Martin Scorsese',
                   'Steven Spielberg',
                   'Pedro Almodóvar'))

df_dir <- df %>%
  filter(director %in% directores_rating$director)

ggplot(df_dir, aes(x=genre, fill=genre)) + geom_histogram(stat='count') +
  facet_wrap(~director)


accion <- df %>%
  filter(genre == 'Action') %>%
  mutate(ganancia=gross-budget) %>%
  mutate(rentable = (ganancia >= 2*budget))

ggplot(df) + 
  geom_point(aes(x=score, y=budget))


#--



duracion <- df %>%
  filter(gross>714421502) %>%
  group_by(genre) %>%
  summarise(total=n(), prom=mean(runtime))
  



generos_años <- df %>%
  filter(year>2011, year<2020) %>%
  mutate(rentabilidad=gross-budget) %>%
  group_by(genre, year) %>%
  summarise(total=n(), rent=sum(rentabilidad, na.rm=T)) %>%
  filter(!genre %in% c('Horror','Thriller','Sport','Sci-Fi','Mystery','Musical','Fantasy','Family'))

ggplot(generos_años) +
  geom_line(aes(x=year, y=rent, color=genre), size=1) + 
  labs(title='Rentabilidad de cada genero (2012-2020)',
       x='años',
       y='rentabilidad')



generos_rating <- df %>%
  filter(year>2011, year<2020) %>%
  group_by(genre, year) %>%
  summarise(total=n(), prom=mean(score, na.rm=T)) %>%
  filter(!genre %in% c('Horror','Thriller','Sport','Sci-Fi','Mystery','Musical','Fantasy','Family'))

ggplot(generos_rating) +
  geom_line(aes(x=year, y=prom, color=genre), size=1) +
  labs(title='Rating promedio de cada genero en los ultimos 10 años')



ggplot(df, aes(year, budget)) +
  geom_violin(aes(group = cut_width(year, 10)), scale = "width", draw_quantiles = c(0,0.25,0.5,0.75,1)) +
  labs(title='Evolucion del presupuesto de peliculas (1980-2020)',
       y='Presupuesto',
       x='década')






ggplot(data=generos) +
  geom_col(aes(x=reorder(genre, -rentabilidad), y=rentabilidad, fill=genre))
  labs(title='Los 10 generos mas rentables', subtitle='osea sume las ganancias de todo el genero y lo reste por los costos')

ggplot(data=cinco_años) +
  geom_col(aes(x=reorder(genre, -rentabilidad), y=rentabilidad, fill=genre)) +
  labs(title='Los 10 generos mas rentables en los ultimos 5 años', subtitle='osea sume las ganancias de todo el genero y lo reste por los costos')

ggplot(data=compañias) +
  geom_col(aes(x=reorder(company, -rentabilidad), y=rentabilidad, fill=company)) +
  labs(title='Las 5 compañias más rentables en los ultimos 42 años')

ggplot(data=compañias_5) +
  geom_col(aes(x=reorder(company, -rentabilidad), y=rentabilidad, fill=company)) +
  labs(title='Las 5 compañias más rentables en los ultimos 5 años')

ggplot(data=actor_rentable) +
  geom_col(aes(x=reorder(star, -rentabilidad), y=rentabilidad, fill=star)) +
  labs(title='Los actores que peliculas más rentables hicieron',)

ggplot(data=actor_rating) +
  geom_col(aes(x=reorder(star, -rating), y=rating, fill=star)) +
  labs(title='Los actores con mas rating promedio (?')

ggplot(data=directores) +
  geom_col(aes(x=reorder(director, -rentabilidad), y=rentabilidad, fill=director)) +
  labs(title='Los directores que mas rentabilidad generaron')

ggplot(data=directores_rating) +
  geom_col(aes(x=reorder(director, -rating), y=rating, fill=director)) +
  labs(title='Los directores que mas mas rating generaron')

ggplot(data=duracion) +
  geom_violin(aes(x=genre, y=prom, fill=genre)) +
  labs(title='Promedio de duracion por genero', subtitle='Promedio de duracion de las 115 peliculas que mas dinero generaron por genero')


