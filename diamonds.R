diamond.data <- read.csv("/Users/ameliabrus/Downloads/datasets.csv")
summary(diamond.data$price)

#wstępna analiza danych
#carat
boxplot(diamond.data$carat)
hist(diamond.data$carat)
summary(diamond.data$carat)

#price
boxplot(diamond.data$price)
hist(diamond.data$price)
summary(diamond.data$price)

#xyz
par(mfrow=c(1, 3), mar=c(1, 4, 1, 1))
boxplot(diamond.data$x)
boxplot(diamond.data$y)
boxplot(diamond.data$z)

par(mfrow=c(1, 3), mar=c(2, 4, 1, 1))
hist(diamond.data$x)
hist(diamond.data$y)
hist(diamond.data$z)

#depth i table 
par(mfrow=c(1, 2), mar=c(1, 4, 1, 1))
boxplot(diamond.data$depth)
boxplot(diamond.data$table)

par(mfrow=c(1, 2), mar=c(2, 4, 1, 1))
hist(diamond.data$depth)
hist(diamond.data$table)

boxplot(diamond.data[, c("carat", "price", "depth", "table", "x", "y", "z")])

#sprawdzenie ile jest NA - usuwamy NA, ponieważ 
#model predykcji nie jest w wykonać obliczeń na danych z wartościami pustymi
colSums(is.na(diamond.data))
diamond.data <- na.omit(diamond.data)

#usuwamy wartości odstające
#carat
diamond.data <- diamond.data[-which(diamond.data$carat > 20), ]
boxplot(diamond.data$carat)

#price
diamond.data <- diamond.data[-which(diamond.data$price > 1000000), ]
boxplot(diamond.data$price)

#depth
diamond.data <- diamond.data[-which(diamond.data$depth  > 75 | diamond.data$depth < 45 ), ]
boxplot(diamond.data$depth)

#table
diamond.data <- diamond.data[-which(diamond.data$table  > 150 | diamond.data$table < 20 ), ]
boxplot(diamond.data$table)

#x 
diamond.data <- diamond.data[-which(diamond.data$x < 1), ]
boxplot(diamond.data$x)

#y - brak odstających obserwacji
boxplot(diamond.data$y)

#z
diamond.data <- diamond.data[-which(diamond.data$z > 2000), ]
boxplot(diamond.data$z)

#podumowania kolumn chr - wszystkie kategorie są używane 
table(diamond.data$cut)
barplot(table(diamond.data$cut))

table(diamond.data$color)
barplot(table(diamond.data$color))

table(diamond.data$clarity)
barplot(table(diamond.data$clarity))

#zmiana zmiennych chr na factor 
diamond.data$cut <- as.factor(diamond.data$cut)
diamond.data$color <- as.factor(diamond.data$color)
diamond.data$clarity <- as.factor(diamond.data$clarity)

#tworzenie modelu - pomijamy depth i table ponieważ są one pomiarami tak samo jak x, y i z, 
#a to wymiary x, y i z są istotne statystycznie
model_diamond <- lm(price ~ carat + cut + color + clarity + x + y + z , data = diamond.data)
summary(model_diamond)
#weryfikacja modelu 
#1. analiza współczynników 
#kolor - color D jest najlepszym kolorem, więc wartości parametrów innych kolorów są ujemne
#clarity - clarity IF oznacza 100% przejrzystość - najlepsze clarity, posiada najwyższą wartość współczynnika
#2. wszystkie dane są istotne na poziomie istotności 0.05
#3. dopasowanie modelu R^2 = 0.9163 - bardzo dobre dopasowanie modelu do danych 
#4. wspóliniowość - aby uniknąć problemu wspóliniowści usuwamy zmienne depth i table, 
#które tworzą ten problem z wymiarami x, y i z 
library(car)
vif(model_diamond)

#pozostaje problem współliniowości pomiędzy zmienną carat i zmiennymi xyz 
#z tego powodu pozbywamy się  x, y, z, które powodują współliniowość i wracamy do depth i table
model_diamond <- lm(price ~ carat + cut + color + clarity + depth + table , data = diamond.data)
summary(model_diamond)
library(car)
vif(model_diamond) #brak problemu współliniowości
#5. heteroskedastyczność i autokorelacja 
library(whitestrap)
white_test(model_diamond)
#problem heteroskedastyczności
library(lmtest)
dwtest(model_diamond)
# brak problemu autokorelacji 

#tworzenie predykcji ex post 
#podział danych na dwie grupy 
set.seed(47)
library(caret)
expost <- createDataPartition(diamond.data$price, p = 0.8, list = FALSE)
train.data <- diamond.data[expost, ]
test.data <- diamond.data[-expost, ]


#model dla danych train
model_diamond <- lm(price ~ carat + cut + color + clarity + depth + table, data = train.data)
summary(model_diamond)

prediction_model <- predict(model_diamond, newdata = test.data)
cor(prediction_model, test.data$price)

comparison <- data.frame(actual = test.data$price, predicted = prediction_model)

plot(comparison$actual, comparison$predicted,
xlab = "Rzeczywista cena", ylab = "Przewidywana cena",
main = "Porównanie cen rzeczywistych i przewidywanych")
abline(0, 1, col = "blue2", lwd = 4)



