# Deflection temperatures for formulation 1 and 2
f1 <- c(206, 193, 192, 188, 207, 210, 205, 185, 194, 187, 189, 178)
f2 <- c(177, 176, 198, 197, 185, 188, 206, 200, 189, 201, 197, 203)

# Q-Q plot for formulation 1
qqnorm(f1)
qqline(f1, col = 2)

# Q-Q plot for formulation 2
qqnorm(f2)
qqline(f2, col = 2)

var.test(f1,f2)



# Two-Sample T-Test
t_test_result <- t.test(f2, f1, alternative = "greater",paired = FALSE,
                        var.equal = TRUE, conf.level = 0.95)

# Display the T-Test Results
print("Two-Sample T-Test:")
print(t_test_result)


#From the two-sample t test above, since the p-value = 0.6333 we fail to 
#reject Ho. There is a insignificant evidence that the deflection temperature 
#under load for formulation 2 exceeds that of formula 1.


#b)
#s1 = sd(f1)
#s2 = sd(f2)
#sp_squre = (11*s1^2 + 11*s2^2)/(12+12-2)

# Confidence interval for the difference in means
conf_interval <- t_test_result$conf.int
print("Confidence Interval:")
print(conf_interval)

#From the confidence interval above, we can see that the confidence interval
#contains 0. It means we fail to reject H0 which is the same result as part a.
#So the confidence interval support my answer above.

#a)
#Ho u <= 225 vs Ha: u >225

hours <- c(159, 224, 222,149, 280, 379, 362, 260, 101, 179, 168, 485, 212, 264,
           250, 170)

t.test(hours, alternative = 'greater', mu = 225)

#From the graph above we know that since p-value = 0.257 which is bigger than 
#0.05, we fail to reject Ho. There is an insignificant evidence that the hours 
#to repait an electric instrument is greater than 225.



  conf.level = 0.95)

c)
qqnorm(hours)
qqline(hours)

#From the q-qplot above, it looks the distribution is a positive skewed and have
#havier tails.Since all points are not near the stright line, the distribution 
#is not normally distributed.
d)





4)

order1<- c(5.73,5.80,8.42,6.84,6.43,8.76,6.32,7.62,6.59,7.67)
order2<- c(6.08,6.22,7.99,7.44,6.48,7.99,6.32,7.60,6.03,7.52)
difference = order1 - order2
qqnorm(difference)
qqline(difference)
t.test(order1,order2, alternative = "two.sided", paired = TRUE,var.equal = FALSE,
     
5)
# Ho:u1 <= u2 Ha = u1 > u2
lower_temp <- c(11.176,7.089,8.097,11.739,11.291,10.759,6.467,8.315)
higher_temp <- c(5.623,6.748,7.461,7.015,8.133,7.418,3.772,8.963)

t_test = t.test(lower_temp, higher_temp, alternative = "greater", paired = FALSE, var.equal
       = TRUE,conf.level = 0.95)

t_test
#From the graph above we know that since p-value = 0.009424 which is much smaller
#than 0.05, we reject Ho. There is an significant evidence that higher baking 
#temperature result in wafers with a lower mean photoresist thickness.
t_test$conf.int


6)








