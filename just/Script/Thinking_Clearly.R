### The list of packages
library(here)
library(ggplot2)
library(tidyverse)
here()
chapter_5_schooling_earn <- read_csv(here('Files for Data Exercises', 'SchoolingEarnings.csv'))
str(chapter_5_schooling_earn)

#5.1
reg_m1 <- lm(data = chapter_5_schooling_earn, earnings~schooling)
summary(reg_m1)

# scatterplot
ggplot(data = chapter_5_schooling_earn)+
  geom_point(mapping = aes(y = earnings, x = schooling))+
  theme_classic()
ggsave(here('Images', 'Figure 1.jpeg'))

#let's have a look at predicted values

predicted_data <- data.frame(predicted = predict(reg_m1),  
                        observed = chapter_5_schooling_earn$earnings, schooling = chapter_5_schooling_earn$schooling)

# plot predicted values and actual values
ggplot(predicted_data) +
  geom_point(mapping = aes(y = predicted, x = observed)) +
  geom_abline(color = "green")+
  theme_classic()
ggsave(here('Images', 'Figure 2.jpeg'))

# polynomials
reg_m2 <- lm(data = chapter_5_schooling_earn, earnings~schooling+I(schooling^2) + I(schooling^3)+ I(schooling^4))
summary(reg_m2)

### ggplot version
ggplot(data=chapter_5_schooling_earn, aes(schooling,earnings)) +
  geom_point() + 
  geom_smooth(method="lm", formula=y~I(x^4)+I(x^3)+I(x^2)) +
  theme_classic()
ggsave(here( 'Images', 'Figure 3.jpeg'))


# base R
jpeg(filename = here("Images", "Figure 4.jpeg"), width = 8, height = 8, units = 'in', res = 500)
plot(x=chapter_5_schooling_earn$schooling, y=chapter_5_schooling_earn$earnings, pch=20, col="grey", xlab = 'Schooling', ylab = 'Earnings')

lines(chapter_5_schooling_earn$schooling, predict(lm(earnings~schooling, data=chapter_5_schooling_earn)), type="l", col="orange1", lwd=2)
lines(chapter_5_schooling_earn$schooling, predict(lm(earnings~I(schooling^2), data=chapter_5_schooling_earn)), type="l", col="pink1", lwd=2)
lines(chapter_5_schooling_earn$schooling, predict(lm(earnings~I(schooling^3), data=chapter_5_schooling_earn)), type="l", col="black", lwd=2)
lines(chapter_5_schooling_earn$schooling, predict(lm(earnings~I(schooling^4), data=chapter_5_schooling_earn)), type="l", col="yellow2", lwd=2)
lines(chapter_5_schooling_earn$schooling, predict(lm(earnings~poly(schooling,4)+poly(schooling,3)+poly(schooling,2)+poly(schooling,1), data=chapter_5_schooling_earn)), type="l", col="blue", lwd=2)
legend("topleft", 
       legend = c("y~x,  - linear","y~x^2", "y~x^3", "y~x^4", "y~x^4 + x^3+x^2 + x"), 
       col = c("orange","pink",'black', "yellow", "blue"),
       lty = 1, lwd=3
) 
dev.off()


##### Now we run regressions and draw plots for different schooling ranges
chapter_5_schooling_earn <- chapter_5_schooling_earn %>% 
  mutate(school_completion = ifelse(schooling>12, "College", "High School"))

ggplot(data = chapter_5_schooling_earn, aes(x = schooling, y = earnings))+
  geom_point(aes(colour = school_completion))+
  geom_smooth(method=lm, aes(fill=school_completion), se=TRUE, fullrange=FALSE)+
  theme_classic()
ggsave(here('Images', 'Figure 5.jpeg'))

#### New analyses to be continued
# I have to use data for those who have 12 or less years of schooling and predict for those who have more than 12 years of schooling
test_model <- lm(data = chapter_5_schooling_earn[chapter_5_schooling_earn$school_completion=="High School",], formula = earnings~schooling)
prediction_colleg <- predict(test_model, chapter_5_schooling_earn[chapter_5_schooling_earn$school_completion=="College",], type = 'response')


colleges <- chapter_5_schooling_earn[chapter_5_schooling_earn$school_completion=="College", 'earnings']
sqrt(mean((prediction_colleg-colleges$earnings))^2) # error is 5.903655


ggplot(data = chapter_5_schooling_earn, aes(x = schooling, y = earnings))+
  geom_point(aes(colour = school_completion))+
  geom_smooth(method=lm, aes(fill=school_completion), se=TRUE, fullrange=TRUE)+
  theme_classic()
ggsave(here('Images', 'Figure 6.jpeg'))


