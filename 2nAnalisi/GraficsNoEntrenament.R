require(ggplot2)

data <- read.csv("2nAnalisi/Llibre1.csv", header = TRUE, sep = ";", dec = ",")

# Fem grafics
# Recompenses
RewardPlot1<- ggplot(data,aes(x=Timesteps, y = Primer.model)) + 
  geom_line() + # Marquem el tipus de grafic
  labs(x = "Steps", y = "Reward") # 

RewardPlot2<- ggplot(data,aes(x=Timesteps, y = Segon.model)) + 
  geom_line() + # Marquem el tipus de grafic
  labs(x = "Steps", y = "Reward") # 

# Creem el gràfic comparatiu
RewardPlotComparativa <- ggplot(data,aes(x=Timesteps)) + 
  
  # Linia del codi que hem creat nosaltres
  geom_line(aes(y=Primer.model),color = "darkgreen") +
  
  # Linia del codi d'Atenea
  geom_line(aes(y=Segon.model),color="steelblue") +
  
  # Etiquetes del gràfic
  labs(x = "Steps", y = "Reward") 

if(FALSE){
  print(RewardPlot1)
  print(RewardPlot2)
  print(RewardPlotComparativa)
}

# Guardem els gràfics
ggsave(filename = paste("2nAnalisi/RewardPlot1.pdf"), 
       plot = RewardPlot1, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))

ggsave(filename = paste("2nAnalisi/RewardPlot2.pdf"), 
       plot = RewardPlot2, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))

ggsave(filename = paste("2nAnalisi/RewardPlotComparativa.pdf"), 
       plot = RewardPlotComparativa, device = "pdf", 
       width = 8.27, height = 4.5, units = c("in"))
