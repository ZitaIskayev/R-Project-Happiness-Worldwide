data <- read.csv("happiness report 2018 final.csv")
# הגדרת משתנים מתוך הנתונים
x1 <- data$Generosity..X1.
x2 <- data$GDP..X2.
x3 <- data$Healthy.life.expectancy..X3.
x4 <- data$Freedom..X4.
x5 <- data$Social.support..x5.
y <- data$score..Y.
# יצירת מסגרת נתונים עם המשתנים שהגדרת
data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5)
# בניית המודל הלינארי, כולל x5
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
# הצגת סיכום המודל
summary(model)
#-------------------------------------------------------------------------------
#residual table 
summary(data)
#--------------------------------------------------------------------------------
# יצירת מטריצת תרשימי פיזור באמצעות pairs
pairs(data)
#--------------------------------------------------------------------------------
# חיזוי הערכים באמצעות המודל
y_hat <- predict(model)
# ממוצע הערכים המוסברים
y_mean <- mean(data$y)
# חישוב SSR
SSR <- sum((y_hat - y_mean)^2)
# הצגת התוצאה
SSR
#-------------------------------------------------------------------------------סטיית תקן
summary(data)
sd(data$y)
sd(data$x1)
sd(data$x2)
sd(data$x3)
sd(data$x4)
sd(data$x5)
#-------------------------------------------------------------------------------#Cov Matrix
x1 <- c(data$x1)
x2 <- c(data$x2)
x3 <- c(data$x3)
x4<- c(data$x4)
x5<-c(data$x5)
y <- c(data$y)
data <- data.frame(y, x1, x2, x3, x4,x5)
cov_matrix <- cov(data)
print(cov_matrix)
variances <- diag(cov_matrix)
print(variances)
#-------------------------------------------------------------------------------Anova table
# חישוב טבלת ANOVA עבור המודל
anova_table <- anova(model)
# הצגת טבלת ANOVA
anova_table
#-------------------------------------------------------------------------------חישוב MSE וMSR
MSR <- anova(model)["x1", "Mean Sq"]
MSE <- summary(model)$sigma^2
# הצגת התוצאות
MSR
MSE
#-------------------------------------------------------------------------------חישוב SST
y_mean <- mean(data$y)
SST <- sum((data$y - y_mean)^2)
SST
#-------------------------------------------------------------------------------#F Critic
numerator_df <- 5  # דרגות חופש בין
denominator_df <- 143  # דרגות חופש 
alpha <- 0.05  # רמת מובהקות
# חישוב ה-F קריטי
f_critical <- qf(1 - alpha, numerator_df, denominator_df)
f_critical
#-------------------------------------------------------------------------------
# בניית המודל הלינארי ללא x1
model_reduced <- lm(y ~ x2 + x3 + x4 + x5, data = data)
# הצגת סיכום המודל המופשט (ללא x1)
summary(model_reduced)
# שליפת השאריות מהמודל
residuals_reduced <- residuals(model_reduced)
# ביצוע מבחן קולמוגורוב-סמירנוב להשוואה עם התפלגות נורמלית
ks_test_result <- ks.test(residuals_reduced, "pnorm", mean(residuals_reduced), sd(residuals_reduced))
# הצגת התוצאות
ks_test_result
#-------------------------------------------------------------------------------
# שליפת השאריות מהמודל
residuals_reduced <- residuals(model_reduced)
# יצירת תרשים QQ
qqnorm(residuals_reduced, main = "QQ Plot of Residuals")
qqline(residuals_reduced, col = "red", lwd = 2)
#-------------------------------------------------------------------------------COV למודל החדש
x2 <- c(data$x2)
x3 <- c(data$x3)
x4<- c(data$x4)
x5<-c(data$x5)
y <- c(data$y)
data <- data.frame(y, x2, x3, x4,x5)
cov_matrix <- cov(data)
print(cov_matrix)
variances <- diag(cov_matrix)
print(variances)
#-------------------------------------------------------------------------------#COR MATRIX NEW MODEL
Cor_matrix<-cor(data)
print(Cor_matrix)
