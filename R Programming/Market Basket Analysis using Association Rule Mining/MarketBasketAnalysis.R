##install.packages("arules")
library(arules)

##As it is a transactional dataset read.csv won't be a useful
groceries<-read.transactions("groceries.csv", sep=",")  

##Summary of the sparse matrix created 
summary(groceries)

##Inspecting first five transaction
inspect(groceries[1:5])

##Proportion of transaction for first few items
itemFrequency(groceries[, 1:5])

##Plot of most frequent items (Image 1)
itemFrequencyPlot(groceries, support = 0.075)

##Top N items (Image 2)
itemFrequencyPlot(groceries, topN = 20)

##Implementation of Apriori algorithm
rules<-apriori(groceries, parameter=list(support=0.006, confidence=0.25,
                                         minlen=2))
##Rules
rules

##Summary of rules created
summary(rules)

##Analyzing rules
inspect(rules[1:3])

##Sorting arules based on lift
inspect(sort(rules, by= "lift")[1:5])

##Rules for berries
berryrules<-subset(rules, items %in% "berries")
inspect(berryrules)
