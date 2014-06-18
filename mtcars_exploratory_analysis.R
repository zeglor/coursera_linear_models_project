data(mtcars)
?mtcars

pairs(mtcars)
# from this plot we can see mpg dependency on cyl (negative), disp(negative),
# hp(negative), drat(positive and fuzzy), wt(negative), qsec(fuzzy, positive),
# vs(positive and fuzzy), am(positive and fuzzy), carb(negative)

plot(mtcars$wt, mtcars$mpg, col = mtcars$am+1)
mdl_all <- lm(mpg ~ ., data=mtcars)
mdl <- lm(mpg ~ wt, data=mtcars)
mdl2 <- lm(mpg ~ wt*as.factor(am), data=mtcars)

#abline(mdl)
summary(mdl_all)
#summary(mdl)
summary(mdl2)
mdl2$coeff[2]
abline(c(mdl2$coeff[1], mdl2$coeff[2]), col="black", lwd=3)
abline(c(mdl2$coeff[1] + mdl2$coeff[3], mdl2$coeff[2] + mdl2$coeff[4]), col="red", lwd=3)

confint(mdl_all)
anova(mdl_all)
vcov(mdl_all)
layout(matrix(c(1,2,3,4),2,2))
plot(mdl_all)
ncvTest(mdl_all)
pairs(mtcars)