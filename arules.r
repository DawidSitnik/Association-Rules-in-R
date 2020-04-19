#Mushroom dataset -  Wednesday group

data(Mushroom)
?Mushroom
str(Mushroom)
summary(Mushroom)
inspect(head(Mushroom))
head(Mushroom, 10)

#sorting according  relative support values
freqTbl = sort(freqTbl, decreasing= TRUE)

#printing only items having support > 20%
print(freqTbl[freqTbl > 0.2])
#as we can see, VeilType is always partial, so this attribute doesn't include any information to our dataset

#number of elements with support >= 5%
length(freqTbl[freqTbl>=0.05])
#there are only 70 elements with frequency greater or equal to 0.05

#chart
itemFrequencyPlot(Mushroom, type ="relative", support= 0.4)

#Discovering rules for different values of minimum support - determination of
#changes in the number of detected rules;
lengths = matrix(0L, 1, 100)
for (i in 0:99)
{
  lengths[i] = length(freqTbl[freqTbl>=i/100])
}
v1 = unlist(lengths)
v1 = sort(v1, decreasing = T, index.return = T)

plot(v1$x, type = "l", xaxt = "n")
axis(1, at = v1$ix, label = names(v1$x))


#2. Frequent itemsets
#setting parameters of Apriori algorithm
aParam  = new("APparameter", "support" =0.3, "minlen"= 1, target ="frequent itemsets")
#adjusting values of parameters
aParam@confidence <-0.7
print(aParam)
str(aParam)
#frequent itemsets discovery - Apriori algorithm
asets <-apriori(Mushroom,aParam)
#analysis of discovered frequent itemsets
length(asets)
summary(asets)
#print associantion rulez with length greater than 5 and sort them in descending order
inspect(head(sort(asets[size(asets)>5], by="support"),10))
#the most frequent set of rules meeting those requirements has support = 0.49 wthh count of 4016

size(asets)
str(asets)

#charts
plot(head(sort(asets[size(asets)>5], by="support"),10), method = "graph")
plot(head(sort(asets[size(asets)>5], by="support"),10), method = "paracoord", control = list(reorder = TRUE))

#maximal itemsets
#display 10 maximal rules with the biggest support
maxSets <- asets[is.maximal(asets)==TRUE]
inspect(head(sort(maxSets, by="support"),10))
summary(maxSets)


aParam@target ="rules"
aParam@minlen = 2L
aParam@confidence = 0.8
aRules <-apriori(Mushroom,aParam)

#Assessment of discovered rules with the lift parameter less than 1 and greater
#than 1 in terms of their usefulness for practical use

#removing reduntant rules (rules with the same consequent and confidence but with less items in the antecedent
aRules <- aRules[is.redundant(aRules) == FALSE]
summary(notRedun)
inspect(notRedun[1:10])

#10 maximal rules with lift parameter >1.0 sorted by lift value
rulesLift1.0 <- subset(aRules, subset = lift > 1.0)
inspect(head(sort(rulesLift1.0, by="lift", decreasing = TRUE),10))
plot(rulesLift1.0, measure = c("support", "lift"), shading = "confidence")

#10 minimal rules with lift parameter >1.0 sorted by lift value
rulesLift1.0 <- subset(aRules, subset = lift < 1.0)
inspect(head(sort(rulesLift1.0, by="lift", decreasing = FALSE),10))
plot(rulesLift1.0, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)

#checking how taking maximal rules only influences support, lift and confidence
rulesLift1.0 <- subset(aRules[is.maximal(aRules) == TRUE], subset = lift > 1.0)
inspect(head(sort(rulesLift1.0, by="lift", decreasing = TRUE),10))
plot(rulesLift1.0, measure = c("support", "lift"), shading = "confidence")

#10 minimal rules with lift parameter >1.0 sorted by lift value
rulesLift1.0 <- subset(aRules[is.maximal(aRules) == TRUE], subset = lift < 1.0)
inspect(head(sort(rulesLift1.0, by="lift", decreasing = FALSE),10))
plot(rulesLift1.0, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)

#the lift is a useful measure which is a measure of the performance of a targeting model
#(association rule) at predicting or classifying cases as having an enhanced response
#(with respect to the population as a whole), measured against a random choice targeting model.
#It means that if we have a set of rules with high lift it can be used for class prediction.

#In opposite case, if the lift is smaller than one, it means that having a set o rules, we can exclude some
#class predictions, knowing that the certain set of attributes in the body reduces probability of its head

#Comparison of the average confidence value of "short" rules and "long" rules

#without excluding rules which are not maximal and redundant
summary(asets[size(asets)==1]) #mean confidence: 0.9419
summary(asets[size(asets)==2]) #mean confidence: 0.9213
summary(asets[size(asets)==3]) #mean confidence: 0.9230
summary(asets[size(asets)==4]) #mean confidence: 0.9213
summary(asets[size(asets)==5]) #mean confidence: 0.9263
summary(asets[size(asets)==6]) #mean confidence: 0.9452
summary(asets[size(asets)==7]) #mean confidence: 0.9600
summary(asets[size(asets)==8]) #mean confidence: 0.9731
summary(asets[size(asets)==9]) #mean confidence: 0.9797

mean_confidence = c(0.9419, 0.9213, 0.9230, 0.9213, 0.9263, 0.9452, 0.9600, 0.9731, 0.9797)
size = c(1:9)
plot( size, mean_confidence, type='l')

#in most of the cases longer the size, greater the mean confidence is
#we got mean = 0.94 for size = 1, but it was due to the small size of elements with size = 1, which was only 5
#for the rest of cases

#for not redundant and maximal rules
asets_maximal_not_redundant = subset(aRules[is.maximal(aRules) == TRUE])
asets_maximal_not_redundant = subset(aRules[is.redundant(asets_maximal_not_redundant) == FALSE])

#without excluding rules which are not maximal and redundant
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==2]) #mean confidence: 0.9414
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==3]) #mean confidence: 0.9442
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==4]) #mean confidence: 0.9456
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==5]) #mean confidence: 0.9485
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==6]) #mean confidence: 0.9719
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==7]) #mean confidence: 0.9637
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==8]) #mean confidence: 0.9731
summary(asets_maximal_not_redundant[size(asets_maximal_not_redundant)==9]) #mean confidence: 0.9797

mean_confidence = c( 0.9414, 0.9442, 0.9456, 0.9485, 0.9719, 0.9637, 0.9731, 0.9797)
size = c(2:9)
plot( size, mean_confidence, type='l')

summary(asets_maximal_not_redundant)
#for not redundant and maximal set of rules, mean confidence also increase with the size of rule.
#this rule is not preserved for rules with size = 6, when mean confidence supprisingly increases to the value greater than for size = 7
