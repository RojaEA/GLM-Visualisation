library(usethis)
library(devtools)
library(dplyr)
library(data.table)
library(ggplot2)
library(rgl)


# Data Preparation...............................................................
data <- data.table(SSMs_allHamburgIndia_cars)

data2w <- data.table(SSMs_allWheelers)

data2 <- data2w %>% 
  dplyr::select("TMD", "MinDistance", "RScollide", "Crash")
data2 <- na.omit(data2)
data2 <- data.frame(data2)
data2["Crash"][data2["Crash"] == 1] <- "pos"
data2["Crash"][data2["Crash"] == 0] <- "neg"
data2
data2$Crash <- as.factor(data2$Crash)


# GML model building............................................................
model_glm <- glm(Crash ~ MinDistance + TMD + RScollide, data = data2, family = "binomial")
summary(model_glm)

# Hoslem Test on the model......................................................
library(ResourceSelection)
hoslem.test(data2$Crash, fitted(model_glm))


#AUC Test on the model..........................................................
library(e1071)
library(lattice)
library(caret)
library(pROC)

get_logistic_pred = function(model_glm, data2, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(model_glm , newdata = data2, type = "response")
  ifelse(probs > cut, pos, neg)
}

test_pred_10 = get_logistic_pred(model_glm, data2, res = "default", 
                                 pos = "pos", neg = "neg", cut = 0.1)
test_pred_50 = get_logistic_pred(model_glm, data2, res = "default", 
                                 pos = "pos", neg = "neg", cut = 0.5)
test_pred_90 = get_logistic_pred(model_glm, data2, res = "default", 
                                 pos = "pos", neg = "neg", cut = 0.9)

test_tab_10 = table(predicted = test_pred_10, actual = data2$Crash)
test_tab_50 = table(predicted = test_pred_50, actual = data2$Crash)
test_tab_90 = table(predicted = test_pred_90, actual = data2$Crash)


test_con_mat_10 = confusionMatrix(test_tab_10, positive = "pos")
test_con_mat_50 = confusionMatrix(test_tab_50, positive = "pos")
test_con_mat_90 = confusionMatrix(test_tab_90, positive = "pos")

metrics <- rbind(
  c(test_con_mat_10$overall["Accuracy"], 
    test_con_mat_10$byClass["Sensitivity"], 
    test_con_mat_10$byClass["Specificity"]),
  
  c(test_con_mat_50$overall["Accuracy"], 
    test_con_mat_50$byClass["Sensitivity"], 
    test_con_mat_50$byClass["Specificity"]),
  
  c(test_con_mat_90$overall["Accuracy"], 
    test_con_mat_90$byClass["Sensitivity"], 
    test_con_mat_90$byClass["Specificity"])
)

rownames(metrics) <- c("c = 0.10", "c = 0.50", "c = 0.90")
metrics


test_prob <- predict(model_glm, newdata = data2, type = "response")
test_roc <- roc(data2$Crash ~ test_prob, plot = TRUE, print.auc = TRUE)

as.numeric(test_roc$auc)
plot()


# Predict the probability (p) of crash.conflict occured
probabilities <- predict(model_glm, type = "response")
range(probabilities)

predicted.classes <- ifelse(probabilities >= 0.52342, "pos", "neg")
head(predicted.classes)

mydata <- data2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 30) +
  geom_smooth(method = "glm") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y", shrink = TRUE, dir = "h")

#Test the outliers in the dataset...............................................
plot(model_glm, which = 4, id.n = 3)
model.data <- augment(model_glm) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Crash), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)


#perspective plot of the model..................................................
library(MASS)
library(graphics)
# Color palette (100 colors)
n.cols <- 100 # number of colours
palette <- colorRampPalette(c('blue', 'green'))(n.cols)
facetcol <- cut(RScollide, n.cols)

persp(model_glm, theta = 50, what = "prediction",
      ylab = "Time to TCP (s)" ,
      xlab = "Min. Distance (m)" ,
      zlab = "Probability of Conflict",
      main = "perspective plot, theta=50, phi=25",
      phi = 25, expand = 0.5, 
      r = sqrt(4), d = 1,  border = TRUE, ltheta = 10, col="lightblue3",
      #col=palette[facetcol], # to add color bar either RScollide or model prediction
      lphi = 60, shade = .5, nticks = 5, ticktype = "detailed", cex.lab = 1, 
      cex.axis = .8, tck = -0.001)



# Visualization of a logistic regression model in Log odds scale.................
library(visreg)

p1 <- visreg(model_glm, "TMD", xlab="Time to TCP (s)", ylab="Log odds (Conflict)",cex.lab = 1.3)

p2 <- visreg(model_glm, "MinDistance", xlab="Min. Distance (m)", ylab="",cex.lab = 1.3)

p3 <- visreg(model_glm, "RScollide", xlab="Conflicting Speed (m/s)", ylab="",cex.lab = 1.3)

par(mfrow=c(1,3), mai=c(0.6,0.6,0.2,0.05))

