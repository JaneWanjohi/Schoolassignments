library(UpSetR)
library(naniar)

library(mice)
library(stringr)

fuma4 <- read.csv("~/Documents/AIMS2022/Review block 3/Data Analysis/Group Assignment/Datasets/fuma4.csv", header=TRUE)

attach(fuma4)
names(fuma4)
head(fuma4)
View(fuma4)
str(fuma4)

colSums(is.na(fuma4))
#no missing values

#categorical variables to factors
f_var <- c('Sex', 'Type.de.sole', 'Type.d.essai', 'Traitement')
fuma4[f_var] <- lapply(fuma4[f_var], factor) 
str(fuma4)
attach(fuma4)
summary(fuma4)

#visualize missing values
library(ggplot2)
vis_miss(fuma4)
gg_miss_upset(fuma4)
gg_miss_var(fuma4, show_pct = TRUE)

#data with the village names and treatments
#df = fuma4[c(2,5)]
#View(df)

colSums(is.na(fuma4))
#data withl ongitudes and latitudes
#df1 = fuma4[c(3,4)]
#View(df1)

#df_new <- mice(fuma4, method = 'cart')
#df_new
#View(df_new)



df <- na.omit(fuma4)
head(df)

View(df_final)

colSums(is.na(df))
summary(df)



#"Sex"                 "Type.de.sole"        "Latitude"            "Longitude"           "Type.d.essai"      
# "Traitement"          "Superficie.essai.m2" "NPL"                 "PDE.kg"              "PDE.kg.ha"    
yieldDiff <- df$PDE.kg.ha[df$Traitement=="traitement"]-df$PDE.kg.ha[df$Traitement=="controle"]
yieldMean <- (df$PDE.kg.ha[df$Traitement=="traitement"]+df$PDE.kg.ha[df$Traitement=="controle"])/2
df$small <- as.numeric(df$NPL<390)

View(df)
###the yield difference and the means
fumax <- subset(df, subset=(Traitement=="traitement"))
fumax$yieldDiff <-yieldDiff; fumax$yieldMean <- yieldMean
head(fumax)
str(fumax)

library(dplyr)


###data proportions
# Create data for the graph.
x <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

# Give the chart file a name.
png(file = "city.png")

# Plot the chart.
pie(fumax$Traitement)

ind <- glm(NPL ~ . , poisson, data = fumax)
y <- xtabs(fitted(ind) ~  Sex + small + Type.de.sole, fumax)
plot(y)

unique(fumax$Superficie.essai.m2)
unique(fumax$NPL)

##create 2 columns of propotion table of nlp and the total area covered
fumax$prop <- fumax$NPL/fumax$Superficie.essai.m2
head(fumax)
fumax$nonprop <- 1 - fumax$prop
attach(fumax)
head(fumax)

y <- cbind(fumax$prop,fumax$nonprop)
single <- glm(y ~ Latitude, family = binomial,data = fumax)
summary(single)
stargazer(single)
xtable(single)
xtable(single1)
xtable(single2)
xtable(single3)
xtable(single4)
single1 <- glm(y ~ Longitude, family = binomial,data = fumax)
summary(single1)

single2 <- glm(y ~ Sex, family = binomial,data = fumax)
xtable(single2)

single3 <- glm(y ~ Type.de.sole, family = binomial,data = fumax)
summary(single3)

single4 <- glm(y ~ small, family = binomial,data = fumax)
summary(single4)
log_Latitude <- log(fumax$Latitude)
log_Longitude <- log(fumax$Longitude)
log_Longitude
log_Type.de.sole <- log(fumax$Type.de.sole)
log_small <- log(fumax$small)
log_Sex <- log(fumax$Sex)

logmod <- glm(y ~ log_Longitude +log_Latitude , family = binomial,data = fumax)
summary(logmod)
logmod

library(stargazer)
comb_Sex <- glm(y ~ Latitude + Longitude + Sex + small , family = binomial,data = fumax)
summary(comb_1)  
stargazer(comb_Sex)
xtable(single,single1,single2,single3)

comb <- glm(y ~ Latitude + Longitude + Sex + small + Type.de.sole , family = binomial,data = fumax)
xtable(comb)              

comb_2 <- glm(y ~ Latitude + Longitude, family = binomial,data = fumax)
summary(comb_2)

comb_3 <- glm(y ~ Latitude + Longitude + Sex, family = binomial,data = fumax)
summary(comb_3)

comb_4 <- glm(y ~ Latitude*Longitude, family = binomial,data = fumax)
summary(comb_4)

comb_5 <- glm(y ~ Latitude + Longitude*small, family = binomial,data = fumax)
summary(comb_5)

comb_6 <-  glm(y ~ Latitude + Type.de.sole*small, family = binomial,data = fumax)
summary(comb_6)

comb_7 <-  glm(y ~ Latitude + Type.de.sole*Sex, family = binomial,data = fumax)
summary(comb_7)

comb_8 <- glm(y ~ Longitude + Type.de.sole*Sex, family = binomial,data = fumax)
summary(comb_8)

comb_9 <- glm(y ~ Longitude  + small, family = binomial,data = fumax)
summary(comb_9)

comb_10 <- glm(y ~ Sex + small, family = binomial,data = fumax)
summary(comb_10)

comb_11 <- glm(y ~ Longitude + Sex + small, family = binomial,data = fumax)
comb_11

comb_1 <- glm(y ~ Latitude + Longitude + Sex + small , family = binomial,data = fumax)
summary(comb_1)  



interaction.plot(
  x.factor = fumax$Type.de.sole,
  trace.factor = fumax$Sex,
  response = fumax$prop,
  fun = mean,
  ylab = "ProportionNPL",
  xlab = "Type of Soil",
  trace.label = "Sex",
  col = c("blue", "maroon"),
  lyt = 1,
  lwd = 3
)

interaction.plot(
  x.factor = fumax$Type.de.sole,
  trace.factor = fumax$small,
  response = fumax$prop,
  fun = mean,
  ylab = "ProportionNPL",
  xlab = "Type of Soil",
  trace.label = "Sex",
  col = c("blue", "maroon"),
  lyt = 1,
  lwd = 3
)
interaction.plot(
  x.factor = fumax$small,
  trace.factor = fumax$Sex,
  response = fumax$prop,
  fun = mean,
  ylab = "Proportion",
  xlab = "Small",
  trace.label = "Sex",
  col = c("blue", "maroon"),
  lyt = 1,
  lwd = 3
)



anova(si, parr, test = "Chisq")