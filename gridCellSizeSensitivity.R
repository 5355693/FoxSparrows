# Supplementary analysis of effect of grid size on results.

## 5-km grid
###Filter this file to remove the rows (observations) without a grid number:
### these are pelagic checklists. The grid was limited to the land area of the study area, so observations
### without grids occurred over open ocean.
write.csv(checklists.unique, file = "uniqueChecklists.csv")
uniqueChecklistsGridded5km <- read.csv(file = "uniqueChecklistsGridded5km.csv")

#turn this into a 2-column successes/failures frame:
props5km <- uniqueChecklistsGridded5km %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(FOSPp = max(FOSPp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, FOSPp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

props5km <- props5km %>%
  mutate(success = ifelse(FOSPp == 1,n,0))

props5km <- props5km %>%
  mutate(failures = ifelse(FOSPp == 0,n,0))

props5km <- props5km %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))

#Make a long version for plotting in ggplot
props5km.long <- gather(props5km, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = props5km.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without FOSP",
       subtitle = "northern and western counties of Maine & NH")

#Model incidence ~ time
FOSP5km.df <- data.frame(success = props5km$success, failure = props5km$failures,
                      year = props5km$YEAR, effort = props5km$effort)

FOSP5km.m1 <- glm(cbind(success,failure) ~ year + effort, family = binomial, data = FOSP5km.df)
summary(FOSP5km.m1)


FOSP5km.m2 <- glm(cbind(success,failure) ~ year, family = binomial, data = FOSP5km.df)
summary(FOSP5km.m2)
confint(FOSP5km.m2)
# Test for overdispersion:
## None, estimated at 0.85
1-pchisq((sum(residuals(object = FOSP5km.m2, type = "pearson")^2)), 12) # p = 0.60

FOSP5km.m2QB <- glm(cbind(success,failure) ~ year, family = quasibinomial,data = FOSP5km.df)
summary(FOSP5km.m2QB)

# The best model is the simpler model, with an effect of Year only:
anova(FOSP5km.m1, FOSP5km.m2, test = "Chisq")

# predict values of y from xv and m2
newdata <- data.frame(year = seq(from = 2003, to = 2016, by = 0.01), effort = rep(1000,1301))
yvals5km <- predict(FOSP5km.m2, newdata = newdata, type = "response",se.fit = TRUE)
yvals5km$upperci <- yvals5km$fit + yvals5km$se.fit*2
yvals5km$lowerci <- yvals5km$fit - yvals5km$se.fit*2


par(mar = c(5.1,5.1,4.1,2.1))
plot(x = props5km$YEAR, y = props5km$proportion, ylab = "Proportion of checklists\nreporting Fox Sparrow",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvals$fit, col = "black", lwd = 2)
lines(newdata$year, yvals$lowerci, col = "black", lwd = 2, lty = 3)
lines(newdata$year, yvals$upperci, col = "black", lwd = 2, lty = 3)


# Repeat approach for BITH:
#turn this into a 2-column successes/failures frame:
propsBith5km <- uniqueChecklistsGridded5km %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(BITHp = max(BITHp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, BITHp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

propsBith5km <- propsBith5km %>%
  mutate(success = ifelse(BITHp == 1,n,0))

propsBith5km <- propsBith5km %>%
  mutate(failures = ifelse(BITHp == 0,n,0))

propsBith5km <- propsBith5km %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))

#Make a long version for plotting in ggplot
propsBith5km.long <- gather(propsBith5km, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = propsBith5km.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                    stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without BITH",
       subtitle = "northern and western counties of Maine & NH")

#Model incidence ~ time
BITH5km.df <- data.frame(success = propsBith5km$success, failure = propsBith5km$failures,
                      year = propsBith5km$YEAR, effort = propsBith5km$effort)

BITH5km.m1 <- glm(cbind(success,failure) ~ year + effort, family = binomial, data = BITH5km.df)
summary(BITH5km.m1)

BITH5km.m2 <- glm(cbind(success,failure) ~ year, family = binomial, data = BITH5km.df)
summary(BITH5km.m2)
confint(BITH5km.m2)

BITH5km.m2OD <- glm(cbind(success,failure) ~ year, family = quasibinomial, data = BITH5km.df)
summary(BITH5km.m2OD)
confint(BITH5km.m2OD)
# The best model is the simpler model, with an effect of Year only:
anova(BITH5km.m1, BITH5km.m2, test = "Chisq")
1-pchisq((sum(residuals(object = BITH5km.m2, type = "pearson")^2)), 12) # p = 0.08

# predict values of y from xv and m2
yvalsBith5km <- predict(BITH5km.m2, newdata = newdata, type = "response",se.fit = TRUE)
yvalsBith5km$upperci <- yvalsBith5km$fit + yvalsBith5km$se.fit*2
yvalsBith5km$lowerci <- yvalsBith5km$fit - yvalsBith5km$se.fit*2


par(mar = c(5.1,5.1,4.1,2.1))
plot(x = propsBith5km$YEAR, y = propsBith5km$proportion, ylab = "Proportion of checklists\nreporting Bicknell's Thrush",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBith5km$fit, col = "red", lwd = 2)
lines(newdata$year, yvalsBith5km$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$year, yvalsBith5km$upperci, col = "red", lwd = 2, lty = 3)

# and for BLPW:
#turn this into a 2-column successes/failures frame:
propsBlpw5km <- uniqueChecklistsGridded5km %>%
  filter(COUNTRY == "United States") %>%
  filter(YEAR > 2002) %>%
  group_by(YEAR,grid.id) %>%
  summarize(BLPWp = max(BLPWp), effort = sum(EFFORT_HRS)) %>%
  group_by(YEAR, BLPWp) %>%
  summarize(n = n_distinct(grid.id), effort = sum(effort))

propsBlpw5km <- propsBlpw5km %>%
  mutate(success = ifelse(BLPWp == 1,n,0))

propsBlpw5km <- propsBlpw5km %>%
  mutate(failures = ifelse(BLPWp == 0,n,0))

propsBlpw5km <- propsBlpw5km %>%
  group_by(YEAR) %>%
  summarize(success = max(success), failures = max(failures),
            effort = sum(effort)) %>%
  mutate(proportion = success/(success+failures))


#Make a long version for plotting in ggplot
propsBlpw5km.long <- gather(propsBlpw5km, outcome, number, success:failures, factor_key = TRUE)
ggplot(data = propsBlpw5km.long, aes(x = YEAR, y = number)) + geom_bar(aes(fill = outcome), position = "dodge", 
                                                                    stat = "identity") + 
  xlab("Year") + ylab("Number of grids") + 
  labs(title = "eBird checklists with and without BLPW",
       subtitle = "northern and western counties of Maine & NH")

#Model incidence ~ time

BLPW5km.df <- data.frame(success = propsBlpw5km$success, failure = propsBlpw5km$failures,
                      year = propsBlpw5km$YEAR, effort = propsBlpw5km$effort)

BLPW5km.m1 <- glm(cbind(success,failure) ~ year + effort, family = binomial, data = BLPW.df)
summary(BLPW5km.m1)
confint(BLPW5km.m1)
BLPW5km.m2 <- glm(cbind(success,failure) ~ year, family = binomial, data = BLPW5km.df)
summary(BLPW5km.m2)
confint(BLPW5km.m2)

BLPW5km.m3 <- glm(cbind(success,failure) ~ year + I(year^2) + effort, family = binomial, data = BLPW5km.df)
summary(BLPW5km.m3)

# The best model is the simpler model, with an effect of Year only:
anova(BLPW5km.m1, BLPW5km.m2, test = "Chisq")
1-pchisq((sum(residuals(object = BLPW5km.m2, type = "pearson")^2)), 12) # p = 0.11
# predict values of y from xv and m2
yvalsBlpw5km <- predict(BLPW5km.m2, newdata = newdata, type = "response",se.fit = TRUE)
yvalsBlpw5km$upperci <- yvalsBlpw5km$fit + yvalsBlpw5km$se.fit*2
yvalsBlpw5km$lowerci <- yvalsBlpw5km$fit - yvalsBlpw5km$se.fit*2

par(mar = c(5.1,5.1,4.1,2.1))
plot(x = propsBlpw5km$YEAR, y = propsBlpw5km$proportion, ylab = "Proportion of checklists\nreporting Blackpoll Warbler",
     xlab = "Year",
     pch = 16, col = "blue", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBlpw5km$fit, col = "red", lwd = 2)
lines(newdata$year, yvalsBlpw5km$lowerci, col = "red", lwd = 2, lty = 3)
lines(newdata$year, yvalsBlpw5km$upperci, col = "red", lwd = 2, lty = 3)

## As a PDF
## Plot together:
### Fox Sparrow
pdf(file = "FigureS1.pdf", width = 4, height = 6)
par(mar = c(5.1,5.1,0.75,2.1))
par(mfrow = c(3,1))
plot(x = props5km$YEAR, y = props5km$proportion, ylab = "Proportion of cells\nreporting Fox Sparrow",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvals5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvals5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvals5km$upperci, col = "black", lwd = 2, lty = 5)

### Bicknell's Thrush:
plot(x = propsBith5km$YEAR, y = propsBith5km$proportion, ylab = "Proportion of cells\nreporting Bicknell's Thrush",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBith5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvalsBith5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvalsBith5km$upperci, col = "black", lwd = 2, lty = 5)


### Blackpoll Warbler
plot(x = propsBlpw5km$YEAR, y = propsBlpw5km$proportion, ylab = "Proportion of cells\nreporting Blackpoll Warbler",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBlpw5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvalsBlpw5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvalsBlpw5km$upperci, col = "black", lwd = 2, lty = 5)
dev.off()

# As a tiff:
tiff(filename = "FigureS1.tiff", width = 4, height = 6, units = "in", res = 300)
par(mar = c(5.1,5.1,0.75,2.1))
par(mfrow = c(3,1))
plot(x = props5km$YEAR, y = props5km$proportion, ylab = "Proportion of cells\nreporting Fox Sparrow",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvals5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvals5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvals5km$upperci, col = "black", lwd = 2, lty = 5)

### Bicknell's Thrush:
plot(x = propsBith5km$YEAR, y = propsBith5km$proportion, ylab = "Proportion of cells\nreporting Bicknell's Thrush",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBith5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvalsBith5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvalsBith5km$upperci, col = "black", lwd = 2, lty = 5)


### Blackpoll Warbler
plot(x = propsBlpw5km$YEAR, y = propsBlpw5km$proportion, ylab = "Proportion of cells\nreporting Blackpoll Warbler",
     xlab = "Year",
     pch = 16, col = "black", lwd = 2)

# add the fitted line
lines(newdata$year, yvalsBlpw5km$fit, col = "black", lwd = 2)
lines(newdata$year, yvalsBlpw5km$lowerci, col = "black", lwd = 2, lty = 5)
lines(newdata$year, yvalsBlpw5km$upperci, col = "black", lwd = 2, lty = 5)
dev.off()