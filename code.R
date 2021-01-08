#librarie utilisé:
# - effects
# - e1071
# - carret

#chargement des données
PimaIndiansDiabetes <- read.table("data_PimaIndiansDiabetes.csv", sep = ";", header=TRUE)

#decomposition en echantillon d'apprentissage (60%) et de test(40%)
n <- nrow(PimaIndiansDiabetes)
train_index <- sample(x = 1:n, size = round(0.6 * n), replace = FALSE)
train_data <- PimaIndiansDiabetes[train_index,]
test_data <- PimaIndiansDiabetes[-train_index,]

#apprentisage du modèle avec l'échantillon d'apprentisage
# modèle de régression logistique
log_reg <- glm(diabetes ~ ., data = train_data, family="binomial")
summary(log_reg)

#coefficient odds-ratio et graphique de chaque effets
exp(coef(log_reg))
plot(allEffects(log_reg))


#on cherche un meilleur modèle via une backward selection
back_sel <- step(log_reg, direction="backward")
summary(back_sel)

# prediction sur echantillon de test
# fitted probabilities
hat_pi <- predict(back_sel,newdata = test_data, type="response")
# fitted values (threshold 50%)
hat_y <- as.integer(hat_pi > 0.5)

#Matrice de confusion
confusionMatrix(data = as.factor(hat_y),
                reference = as.factor(test_data$diabetes),
                positive = "1")

#def fonction pour la sensibilité et la spécificité
threshold <- seq(0, 1, 0.001)
f_sensibility <- function(threshold, hat_pi, df) {
  out <- sum(as.integer(hat_pi > threshold) == 1 &
               df$diabetes == 1)/sum(df$diabetes == 1)
  return(out)
}

f_specificity <- function(threshold, hat_pi, df) {
  out <- sum(as.integer(hat_pi > threshold) == 0 &
               df$diabetes == 0)/sum(df$diabetes == 0)
  return(out)
}

# calcul
sens <- sapply(threshold, f_sensibility, hat_pi = hat_pi, df = test_data)
spec <- sapply(threshold, f_specificity, hat_pi = hat_pi, df = test_data)

# sensibility et specificity en fonction du threshold
data2plot <- data.frame(threshold=rep(threshold,2),
                        value=c(sens, spec),
                        tag=rep(c("sensitivity", "specificity"),
                                each = length(threshold)))
ggplot(data2plot, aes(x=threshold, y=value)) +
  geom_line(aes(col=tag)) +
  theme_bw() + theme(legend.title = element_blank())

# graphique auc et valeur
data2plot <- data.frame(threshold=threshold,
                        sensitivity=sens,
                        specificity=spec)
ggplot(data2plot, aes(x=1 - specificity, y=sensitivity)) +
  geom_line() + geom_area(fill="red", alpha=0.2, position = 'identity') + theme_bw()

auc(test_data$diabetes, hat_pi)
