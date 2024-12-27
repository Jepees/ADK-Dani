install.packages("reshape2")
install.packages("ggplot2")
install.packages("pROC")

library(ggplot2)
library(reshape2)
library(pROC)

lokasifile <- "https://raw.githubusercontent.com/Jepees/ADK-Dani/refs/heads/main/Dataset/Planes.csv"
data <- read.csv(lokasifile)

#Eksploarasi data
str(data)

listkolom <- colnames(data)
listkolom

numerik <- c("Age",
            "Flight.Distance",
            "Departure.Delay.in.Minutes",
            "Arrival.Delay.in.Minutes"
            )


kategorik <- c("Gender",
                "Type.of.Travel",
                "Class",
                "Inflight.wifi.service",
                "Inflight.entertainment",
                "satisfaction"
                )

# Statistika Deskriptif Data Numerik
lapply(data[numerik], summary)

# Distribusi Data Kategorik
for (kolom in kategorik) {
    print(table(data[[kolom]]))
    print(paste("Distribusi nilai pada kolom:", kolom))
    }

# Penyederhanaan kategori
data$Inflight.entertainment[data$Inflight.entertainment %in% c(0,1)] <- "Low"
data$Inflight.entertainment[data$Inflight.entertainment %in% c(2,3)] <- "Medium"
data$Inflight.entertainment[data$Inflight.entertainment %in% c(4,5)] <- "High"
data$Inflight.wifi.service[data$Inflight.wifi.service %in% c(0, 1)] <- "Low"
data$Inflight.wifi.service[data$Inflight.wifi.service %in% c(2,3)] <- "Medium"
data$Inflight.wifi.service[data$Inflight.wifi.service %in% c(4,5)] <- "High"
data$Class[data$Class %in% c("Eco Plus")] <- "Eco"

# Menyesuaikan variabel target agar menjadi biner
data$satisfaction <- ifelse(data$satisfaction == "satisfied", 1, 0)

# model dasar (menjumlahkan semua variabel yang ada
model_dasar <- glm(satisfaction ~ 
                    Gender + 
                    Age +
                    Flight.Distance +
                    Departure.Delay.in.Minutes +
                    Arrival.Delay.in.Minutes +
                    Type.of.Travel +
                    Class +
                    Inflight.wifi.service +
                    Inflight.entertainment, 
                    data = data, family = binomial)

# Memeriksa multikolinearitas
car::vif(model_dasar)   # Cek VIF
numerik_data <- data[, sapply(data, is.numeric)]
# Korelasi untuk nilai yang lengkap
cor_matrix <- cor(numerik_data, use = "complete.obs")  
cor_matrix

## Transformasi matriks korelasi ke format long
cor_melt <- melt(cor_matrix)

## Plot heatmap korelasi
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(   low = "blue", high = "maroon", mid = "white", 
                            midpoint = 0, limit = c(-1, 1), space = "Lab",
                            name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    coord_fixed() +
    labs(title = "Correlation Heatmap", x = "", y = "")

# Model dasar tanpa multikolinearitas
model_dasar2 <- glm(satisfaction ~ 
                    Gender + 
                    Age +
                    Flight.Distance +
                    Departure.Delay.in.Minutes +
                    Type.of.Travel +
                    Class +
                    Inflight.wifi.service +
                    Inflight.entertainment, 
                    data = data, family = binomial)
car::vif(model_dasar2)

# Model Building
## Metode Stepwise Backward
model_back<-step(model_dasar2, test = "Chisq")
summary(model_back)

## Metode Stepwise Forward 
model_awal<-glm(satisfaction ~ 1, data = data, family = binomial)
model_forward <- step(  model_awal, 
                        scope = list(lower = model_awal, upper = model_dasar2), 
                        direction = "forward")
summary(model_forward)

# Memeriksa apakah model sama
anova(model_back,model_forward,test = "Chisq")
## Diperoleh model sama

# Diperoleh model sederhana
model_1 <- glm(satisfaction ~ 
                Gender + 
                Age +
                Flight.Distance +
                Departure.Delay.in.Minutes +
                Type.of.Travel +
                Class +
                Inflight.wifi.service +
                Inflight.entertainment, 
                data = data, family = binomial)
summary(model_1)

# Klasifikasi tabel model pertama
yhat1 <- fitted(model_1) # prediksi peluang
pred1_50 <- ifelse(yhat1 >= 0.50, 1, 0) # prediksi 0/1
case1<-data$satisfaction

confusion_matrix1 <- table(pred1_50, case1)
confusion_matrix1  # confusion matrix

TP1 <- confusion_matrix1[2, 2]  # True Positive
TN1 <- confusion_matrix1[1, 1]  # True Negative

# Akurasi model pertama
akurasi1 <- (TP1 + TN1) / sum(confusion_matrix1)
print(paste("Akurasi:", akurasi1))

# Transformasi tabel menjadi data frame
confusion_long1 <- melt(confusion_matrix1)
colnames(confusion_long1) <- c("Predicted", "Observed", "Count")

# Heatmap Confusion Matrix model pertama
ggplot(confusion_long1, aes(x = Observed, y = Predicted, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
    geom_text(aes(label = Count), color = "white", size = 5) +  # Menampilkan nilai
    labs(title = "Confusion Matrix Heatmap", x = "Observed", y = "Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# kurva ROC model pertama
roc1 <-roc(case1,yhat1) 
plot.roc(roc1,main="ROC", print.auc=TRUE,legacy.axes=TRUE)

# Model dengan interaksi
model_2 <- glm(satisfaction ~ 
                Gender + 
                Age +
                Departure.Delay.in.Minutes +
                Type.of.Travel +
                Class +
                Inflight.wifi.service +
                Flight.Distance*Inflight.entertainment
                , data = data, family = binomial)
summary((model_2))

# Klasifikasi tabel model interaksi
yhat2 <- fitted(model_2) # prediksi peluang
pred2_50 <- ifelse(yhat2 >= 0.50, 1, 0) # prediksi 0/1
case2<-data$satisfaction

confusion_matrix2 <- table(pred2_50, case2)
confusion_matrix2

TP2 <- confusion_matrix2[2, 2]  # True Positive
TN2 <- confusion_matrix2[1, 1]  # True Negative

# Akurasi model interaksi
akurasi2 <- (TP2 + TN2) / sum(confusion_matrix2)
print(paste("Akurasi:", akurasi2))

# Transformasi tabel menjadi data frame
confusion_long2 <- melt(confusion_matrix2)
colnames(confusion_long2) <- c("Predicted", "Observed", "Count")

# Heatmap Confusion Matrix model pertama
ggplot(confusion_long2, aes(x = Observed, y = Predicted, fill = Count)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
    geom_text(aes(label = Count), color = "white", size = 5) +  # Menampilkan nilai
    labs(title = "Confusion Matrix Heatmap", x = "Observed", y = "Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# kurva ROC model interaksi
roc2 <-roc(case2,yhat2) 
plot.roc(roc2,main="ROC", print.auc=TRUE,legacy.axes=TRUE)

# Membandingkan model dengan LRT
anova(model_1,model_2,test = "Chisq")

# Summary Model Akhir
summary((model_2))

