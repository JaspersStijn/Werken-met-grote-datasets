## --------------------------
## Werken met grote datasets
#     Van onderzoeksvraag tot evaluatie (workshop 21-3-2026 UHasselt)
## --------------------------

# In dit document voorzien we de R code die we ook in de PowerPoint tijdens de workshop gebruiken. De oefeningen worden in een apart
# document uitgewerkt.

## Laden van de R libraries die we nodig hebben
##----------------------------------------------

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidytuesdayR)
library(BSDA)

##-------------------------------------------
## Deel I: onderzoeksvraag en data verzamelen
##-------------------------------------------

# Geen R code, maar een voorbeeld van een dataset uit de "TidyTuesday database":
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/readme.md

tt_object = tt_load("2026-03-10")
tt_object$absolute_judgements
tt_object$pairwise_comparisons
tt_object$respondent_metadata

##----------------------------------------------------------
## Deel II: soorten veranderlijken en samenvattende getallen
##----------------------------------------------------------

load("Datasets/pizzasize.Rdata")
head(pizzasize)

sort(table(pizzasize$CrustDescription),decreasing=TRUE)[1]
median(pizzasize$Diameter)
mean(pizzasize$Diameter)
quantile(pizzasize$Diameter, 0.75, type = 2)
var(pizzasize$Diameter)
sd(pizzasize$Diameter)


# We voegen zelf een ordinale variabele toe aan de dataset, namelijk "Satisfaction" 

SatisfactionDominos <- c(rep(1, 75), rep(2,25), rep(3,12), rep(4, 8), rep(5,5))
SatisfactionEagle <- c(rep(1, 5), rep(2,8), rep(3,12),rep(4, 25), rep(5,75))

pizzasize <- pizzasize[order(pizzasize$Store),]
pizzasize$Satisfaction <- c(SatisfactionDominos,SatisfactionEagle)
pizzasize$Satisfaction <- factor(pizzasize$Satisfaction, levels=c(1:5), ordered=TRUE,
                                 labels = c("Heel tevreden", "Tevreden", "Neutraal", "Niet tevreden","Helemaal niet tevreden"))

range(SatisfactionDominos)
quantile(SatisfactionDominos, 0.75, type = 2) - quantile(SatisfactionDominos, 0.25, type = 2)	


##-------------------------------------
## Deel III: visualisatie en exploratie
##-------------------------------------

# base package
hist(pizzasize$Diameter)
text(x=26.7,y=50,"Piek 1",col="red")
text(x=29,y=63,"Piek 2",col="red")

# ggplot2 package
ggplot(data=pizzasize, aes(x=Diameter) ) + geom_boxplot()
ggplot(data=pizzasize, aes(x=Diameter) ) + geom_histogram()

# 1 Numerieke variabele: Diameter
ggplot(data=pizzasize, aes(x=Diameter)) + 
  geom_boxplot() +
  ggtitle("Boxplot van de geobserveerde pizza diameters")+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) 
ggplot(data=pizzasize, aes(x=Diameter,y=..density..) ) + geom_histogram()

# 1 categorische variabele: Topping
ggplot(data=pizzasize,aes(x=Topping))+geom_bar()

pizzasize_summary = pizzasize%>%group_by(Topping)%>%summarise(Count=n())
ggplot(data=pizzasize_summary,aes(x=Topping,y=Count))+geom_point()+ 
  geom_segment(aes(x=Topping, y=0, yend=Count), 
               size=1, data=pizzasize_summary , linetype="dotted") 

# 1 categorische - 1 numerieke variabele
ggplot(data=pizzasize,aes(x=Diameter,y=Store))+geom_boxplot()

ggplot(data=pizzasize, aes(x=Diameter,y=..density..,fill=Store) ) + 
  geom_histogram()

ggplot(data=pizzasize, aes(x=Diameter,y=..density..,fill=Store) ) + 
  geom_histogram(position="identity", alpha=0.8)

ggplot(data=pizzasize, aes(x=Diameter,y=..density..,fill=Store) ) + 
  geom_histogram(position="dodge")


# 2 categorische variabelen

ggplot(data=pizzasize,aes(x=Topping,fill=Store))+geom_bar(position = "dodge")
ggplot(data=pizzasize,aes(x=Topping,fill=Store))+geom_bar(position = "stack")



## Oefeningen

# 1.  Onderzoek de variabele “CrustDescription”
### Maak een gepaste grafische voorstelling, waarbij je al dan niet een opsplitsing maakt per winkel


###Gebruikt elke winkel dezelfde benamingen?


  
# 2.   Onderzoek of de diameters van de pizza’s afhangen van de topping
### Maak een gepaste grafische voorstelling, waarbij je eerst de winkel negeert

### Maak een gepaste grafische voorstelling, waarbij je ook de winkel includeert


## Extra Oefeningen

# We schonen de data verder op, op basis van de bevindingen in onze visualisaties



# Maak opniew bovenstaande visulisaties, maar nu met de uniforme variabelen


##----------------------------------------------------------
## Deel IV: Hypothesetoetsen en Betrouwbaarheidsintervallen
##----------------------------------------------------------

## Een toets voor het populatiegemiddelde (µ) in R

# T-toets
t.test(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54, mu=12, alternative="less")

# Z-toets
z.test(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54, mu=12, 
       sigma.x = sd(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54), alternative="less")

# Betrouwbaarheidsinterval
t.test(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54, mu=12, alternative="two.sided", conf.level = 0.95)$conf.int

# Manuele berekeningen
mean <- mean(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54)
sd <- sd(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54)
n <- length(pizzasize$Diameter[pizzasize$Store=="EagleBoys"])
t <- (mean-12)/(sd/sqrt(n))
t
pt(t,n-1)

Tgrens <- qt(0.025,n-1,lower.tail = FALSE)
BIondergrens <- mean - Tgrens * sd / sqrt(n)
BIbovengrens <- mean + Tgrens * sd / sqrt(n)
c(BIondergrens,BIbovengrens)


## Een toets voor de populatieproportie (𝝅) in R

length(which(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54<12))

# Chi²-toets
prop.test(122, 125, p=0.95, alternative="greater", correct = F)

# Betrouwbaarheidsinterval
prop.test(122, 125, p=0.95, alternative="two.sided", correct = F, conf.level = 0.95)$conf.int

# Manuele berekeningen (voor Z-toets)
Z <- (122/125-0.95)/sqrt(0.95*(1-0.95)/125)
Z; Z^2
pnorm(Z, lower.tail = FALSE)
Zgrens <- qnorm(0.025,lower.tail = FALSE)
BIondergrens <- 122/125 - Zgrens * sqrt(122/125*(1-122/125)/125)
BIbovengrens <- 122/125 + Zgrens * sqrt(122/125*(1-122/125)/125)
c(BIondergrens,BIbovengrens)


## Toetsen voor twee gemiddelden in R
t.test(pizzasize$Diameter[pizzasize$Store=="EagleBoys"]/2.54, 
       pizzasize$Diameter[pizzasize$Store=="Dominos"]/2.54, alternative="greater")


## Oefeningen

# Bereken een 99% betrouwbaarheidsinterval voor de grootte van Domino’s pizza’s (in cm)

# De website van Domino’s vermeld dat een “Thin & Crispy pizza” 30 cm groot is. 
# Klopt deze bewering volgens de data in de gegeven dataset?

# Je verwacht dat “DeepPan” pizza’s (met een dikke korst) door de bereidingswijze wel een kleinere diameter zullen hebben dan pizza’s met een dunnere korst.
# Ga deze bewering na voor Domino’s pizza’s als je vergelijkt met een klassieke korst (“ClassicCrust”)
# Ga deze bewering na voor Domino’s pizza’s als je vergelijkt met een dunne korst (“ThinNCrispy”)