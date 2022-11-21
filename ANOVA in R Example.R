library(tidyverse)
library(report)
library(multcomp)
library(ggstatsplot)
library(caret)

#Question 1 of Assignment 1

#Visualize the data for Question 1.
ggplot(data, mapping = aes(x=FamilyType, y=FamilyIncome, color('FamilyType'))) + 
  geom_violin(aes(fill=FamilyType),
              na.rm = TRUE)


#Question 1 ANOVA Test for Family Type & Family Income
#Summary of Means and Standard Deviations
group_by(data, FamilyType) %>%
  summarise(
    mean = mean(FamilyIncome, na.rm = TRUE),
    sd = sd(FamilyIncome, na.rm = TRUE)
  )
#Create ANOVA
FamilyIncome_anova <- aov(FamilyIncome ~ FamilyType,
                          data = data
)

#Test for Normality and Homogeneity
par(mfrow = c(1, 2))
plot(FamilyIncome_anova, Which =3)
plot(FamilyIncome_anova, which = 2)
#Residual Plot shows that: homogeneity is good as plot 1 shows a flat horizontal line.
#Data is not normally distributed though as plot 2 data deviates from dashed line.

#Summary of ANOVA Test
summary(FamilyIncome_anova)

#Report of ANOVA Test
report(FamilyIncome_anova)

#TukeyHSD Post Hoc 
TukeyHSD(FamilyIncome_anova)
plot(TukeyHSD(FamilyIncome_anova))

#ANOVA Model
ggbetweenstats(
  data = data,
  x = FamilyType,
  y = FamilyIncome,
  type = "parametric",
  var.equal = TRUE,
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

