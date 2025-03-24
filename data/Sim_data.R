library(tidyverse); library(drc); library(writexl)

set.seed(121212)

# 1. Impact de l'ether sur la reproduction des dragons

# Paramètres de la courbe dose-réponse
EC50 <- 12
Slope <- 1.2
Y_max <- 20
Y_min <- 0

# Paramètre de notre jeu de donnée
nb_rep <- 5
Dose_min <- 0.25
Factor_dose <- 10^(1/2)
sigma <- 5 # Variabilité de la production de cocons

Doses <- c(
  0,
  Dose_min,
  Dose_min*Factor_dose^1, 
  Dose_min*Factor_dose^2,
  Dose_min*Factor_dose^3, 
  Dose_min*Factor_dose^4,
  Dose_min*Factor_dose^5,
  Dose_min*Factor_dose^6
  )

nb_doses <- length(Doses)

nb_ind <- nb_doses * nb_rep

f_Hill <- function(Dose){
  return(Y_min+(Y_max-Y_min)/(1+exp(Slope*(log(Dose)-log(EC50)))))
}

df_sim_repro <- expand.grid(
  nb_rep = seq(1, nb_rep, 1),
  Dose = Doses
  ) |> 
  mutate(
    ID = row_number(),
    Oeuf_th = f_Hill(Dose),
    Oeuf_rand = round(rnorm(n(), Oeuf_th, sigma),1),
    Oeuf = case_when(
      Oeuf_rand < 0 ~ 0,
      Oeuf_rand >= 0 ~ Oeuf_rand
      )
    ) |> 
  dplyr::select(ID, Dose, nb_rep, Oeuf)

write_xlsx(df_sim_repro, "data/Data_sim_repro_dragon.xlsx")

# 2. Impact de l'éther sur la croissance des dragons ----

# Paramètres de la courbe dose-réponse
EC50 <- 100
Slope <- 1.8
Y_max <- 30
Y_min <- -5

# Paramètre de notre jeu de donnée
nb_rep <- 5
Dose_min <- 5
Factor_dose <- 10^(1/3)
sigma <- 6 # Variabilité de la croissance

Doses <- c(
  0,
  Dose_min,
  Dose_min*Factor_dose^1, 
  Dose_min*Factor_dose^2,
  Dose_min*Factor_dose^3, 
  Dose_min*Factor_dose^4,
  Dose_min*Factor_dose^5,
  Dose_min*Factor_dose^6
)

nb_doses <- length(Doses)

nb_ind <- nb_doses * nb_rep

f_Hill <- function(Dose){
  return(Y_min+(Y_max-Y_min)/(1+exp(Slope*(log(Dose)-log(EC50)))))
}

df_sim_croiss <- expand.grid(
  nb_rep = seq(1, nb_rep, 1),
  Dose = Doses
) |> 
  mutate(
    ID = row_number(),
    Txcroiss_th = f_Hill(Dose),
    Txcroiss = round(rnorm(n(), Txcroiss_th, sigma),3)
  ) |> 
  dplyr::select(ID, Dose, nb_rep, Txcroiss)

write_xlsx(df_sim_croiss, "data/Data_sim_croiss_dragon.xlsx")




