# Importa packages
require(stringr)
require(ggplot2)

# Importa txt document
data <- read.delim2("resultats.txt", header = FALSE, sep = "\n", dec = ".")

# Creem el dataframe que emplenarem
plotData <- data.frame(1:10*10000)
# Neteja les dades per extreure les recompenses
raw_rewards <- data[str_detect(data[,1], "ep_rew_mean"),]

plotData[,2]<- as.numeric(str_extract(raw_rewards, "[[:digit:]]+.+[[:digit:]]"))


# El mateix però amb el loss. El primer valor és NA, perquè no té amb què comparar
raw_loss <- data[str_detect(data[,1], " loss "),]

plotData[,3]<- c(NA, as.numeric(str_extract(raw_loss, "[[:digit:]]+.+[[:digit:]]")))

names(plotData) <- c("Posicio","Rewards", "Loss")

# Fem gràfics
p<- ggplot(plotData,aes(x=Posicio, y = Rewards)) +
  geom_line() +
  #geom_errorbar(aes(ymax=Limit*100, ymin=Limit*100), color = 'black')+
  labs(x = "Steps", y = "Reward")  

print(p)

# Fem gràfics
q<- ggplot(plotData,aes(x=Posicio, y = Loss)) +
  geom_line() +
  #geom_errorbar(aes(ymax=Limit*100, ymin=Limit*100), color = 'black')+
  labs(x = "Steps", y = "Loss") 

print(q)


