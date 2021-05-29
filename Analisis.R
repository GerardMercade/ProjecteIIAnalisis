# Escrivim el nom dels arxius que utilitzarem
# 1r dels agents analitzats (verd)
nom_arxiu_1 <- "training1_Oscar.txt"

# 2n dels agents analitzats (blau)
nom_arxiu_2 <- "training1_v2_Oscar.txt"

# Indiquem de quin arxiu volem que ens doni detalls 
# Es mostraran, en concret, els Grafics Reward i Loss


# Nombre dades amb que es farà la mitjana a la comparativa
cada_quants_mitjana <- 15


# A partir d'aquí, en principi, no cal tocar res

# Importa packages
require(stringr)
require(ggplot2)
require(dplyr)

# Importa txt document
data <- read.delim2(nom_arxiu_1, header = FALSE, 
                    sep = "\n", dec = ".")

# Neteja les dades per extreure el total timesteps
raw_timesteps <- data[str_detect(data[,1], "total_timesteps"),]

# Neteja les dades per extreure les recompenses
raw_rewards <- data[str_detect(data[,1], "ep_rew_mean"),]
# Creem el dataframe que emplenarem
plotData <- data.frame(1:length(raw_rewards)*as.numeric(str_extract(
  raw_timesteps, "[[:digit:]]+.+[[:digit:]]"))[1])

# Extreiem els rewards
plotData[,2]<- as.numeric(str_extract(raw_rewards, "[[:digit:]]+.+
                                      [[:digit:]]|-+[[:digit:]]+.+
                                      [[:digit:]]|[[:digit:]]|-+
                                      [[:digit:]]"))

# El mateix pero amb el loss
# El primer valor es NA, perque no te amb que comparar
raw_loss <- data[str_detect(data[,1], " loss "),]

# Igual que abans xtreiem els loss
plotData[,3]<- c(NA,as.numeric(str_extract(raw_loss, "[[:digit:]]+.+
                                           [[:digit:]]|-+
                                           [[:digit:]]+.+
                                           [[:digit:]]|[[:digit:]]|-+
                                           [[:digit:]]")),
                 rep(NA, nrow(plotData)-length(raw_loss)-1))

# Reanomenem les columnes del data frame
names(plotData) <- c("Posicio","Rewards", "Loss")

# Fem la mitjana cada cada_quants_mitjana mostres
# Inicialitzem les variables
n<-0
suma <- 0

# Inicialitzem els comptadors que s'aniran actualitzant
mean <- c()
noms <-c()

# Fem un for per a cada fila de les dades
for (i in 1:nrow(plotData)){
  if (n==cada_quants_mitjana) { # Si toca fer la mitjana
    # Es fa la mitjana, s'afegeix el nom i 
    # es reinicialitza el comptador i el sumatori
    mean <- c(mean,suma/cada_quants_mitjana)
    noms <- c(noms, plotData[i,1])
    n <- 0
    suma <- 0
    
  } else { # Si encara no toca fer la mitjana
    # S'augmenta el comptador en 1 i se suma el nou valor a la suma
    n <- n+1
    suma <- suma + plotData[i,2]
  }
}

# Es crea un data frame amb els valors obtinguts
ambMitjanes1 <- data.frame(mean,noms)


# Fem grafics
# Recompenses
RewardPlot<- ggplot(plotData,aes(x=Posicio, y = Rewards)) + 
  geom_line() + # Marquem el tipus de grafic
  labs(x = "Steps", y = "Reward") # 

# Loss
LossPlot <- ggplot(plotData,aes(x=Posicio, y = Loss)) +
  geom_line() +
  labs(x = "Steps", y = "Loss") 


#Realitzem el mateix procediment pero amb les altres dades

# Importa txt document
data <- read.delim2(nom_arxiu_2, header = FALSE, 
                    sep = "\n", dec = ".")

# Neteja les dades per extreure el total timesteps
raw_timesteps <- data[str_detect(data[,1], "total_timesteps"),]

# Neteja les dades per extreure les recompenses
raw_rewards <- data[str_detect(data[,1], "ep_rew_mean"),]

# Creem el dataframe que emplenarem
plotData <- data.frame(1:length(raw_rewards)*as.numeric(
  str_extract(raw_timesteps, "[[:digit:]]+.+[[:digit:]]"))[1])

# Extreiem els rewards
plotData[,2]<- as.numeric(str_extract(raw_rewards, "[[:digit:]]+.+
                                      [[:digit:]]|-+[[:digit:]]+.+
                                      [[:digit:]]|[[:digit:]]|-+
                                      [[:digit:]]"))

# Reanomenem les columnes del Data Frame
names(plotData) <- c("Posicio","Rewards")

# Fem les mitjes dels rewards, cada 5
# Inicialitzem variables
n<-0
suma <- 0

# Inicialitzem comptadors
mean <- c()
noms <-c()

# Repetim el procediment per a cada fila
for (i in 1:nrow(plotData)){ 
  if (n==cada_quants_mitjana) { # Quan toqui guardar la mitjana
    # Emmagatzemem mean i noms
    mean <- c(mean,suma/cada_quants_mitjana)
    noms <- c(noms, plotData[i,1])
    
    # Resetegem comptadors
    n <- 0
    suma <- 0
  } else { # Si no toca guardar mitjana
    n <- n+1 # Augmentem en 1 el comptador
    suma <- suma + plotData[i,2] # Afegim el valor a la suma.
  }
}

# Creem el dataframe amb aquests valors
ambMitjanes2 <- data.frame(mean,noms)

#Preparem el grafic per a ser mostrat
# Agafem nomes els primers valors
quants_valors_agafem <- min(nrow(ambMitjanes1), nrow(ambMitjanes2))

ambMitjanes1 <-ambMitjanes1[1:quants_valors_agafem,]

names(ambMitjanes1) <- c("Nou", "noms")

# Afegim els valors de la mitjana del codi d'Atenea
ambMitjanes1$Atenea <-ambMitjanes2[1:quants_valors_agafem,1]


# Tornem a invertir els noms per a conservar els colors
names(ambMitjanes1) <- c("Nou", "noms", "Atenea")


# Creem el gràfic comparatiu
ComparativaPlot<- ggplot(ambMitjanes1,aes(x=noms)) + 
  
  # Linia del codi que hem creat nosaltres
  geom_line(aes(y=Nou),color = "darkgreen") +
  
  # Linia del codi d'Atenea
  geom_line(aes(y=Atenea),color="steelblue") +
  
  # Etiquetes del gràfic
  labs(x = "Steps", y = "Reward") 

# Mostrem els grafics
if (TRUE){
  print(RewardPlot)
  print(LossPlot)
  print(ComparativaPlot)
}

# Guardem els gràfics
ggsave(filename = paste("Grafics/RewardPlot_",nom_arxiu_1,".pdf"), 
       plot = RewardPlot, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))

ggsave(filename = paste("Grafics/LossPlot_",nom_arxiu_1,".pdf"), 
       plot = LossPlot, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))

ggsave(filename = paste("Grafics/Comparative_",nom_arxiu_1,
                        "(verd)_vs_",nom_arxiu_2,"(blau).pdf"), 
       plot = ComparativaPlot, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))