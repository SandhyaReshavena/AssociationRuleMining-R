#Association Rule Mining is an Unsupervised Non-linear algorithm to reveal how the items are associated with each other.
#Association Rule Mining majorly used by retailers, grocery stores, an online marketplace that has a large transactional database.
#Also used in online social media, marketplace, and e-commerce websites know what you buy next using recommendations engines.
#There are three common ways to measure association:Support, Confidence and Lift
#Support: Defines how popular an item is, as measured in the proportion of transactions in which an item set appears.
#Confidence: Defines how likely item Y is purchased when item X is purchased, expressed as {X -> Y}.
#Thus it is measured by the proportion of transaction with item X in which item Y also appears. 
#Lift: Defines how likely item Y is purchased when item X is purchased while controlling for how popular item Y is.

# Sandhya an example test case for your reference before we getting into the main project (You can find these definitions and example case in online as well)
#Example:
#A customer does 4 transactions with you. In the first transaction, she buys 1 apple, 1 beer, 1 rice, and 1 chicken. 
#In the second transaction, she buys 1 apple, 1 beer, 1 rice. In the third transaction, she buys 1 apple, 1 beer only. 
#In fourth transactions, she buys 1 apple and 1 orange.

#Support(Apple) = 4/4 

#So, Support of {Apple} is 4 out of 4 or 100%

#Confidence(Apple -> Beer) =  Support(Apple, Beer)/Support(Apple) = (3/4)/(4/4) = 3/4

#So, Confidence of {Apple -> Beer} is 3 out of 4 or 75%

#Lift(Beer -> Rice) = Support(Beer, Rice)/(Support(Beer) * Support(Rice)) = (2/4)/(3/4) * (2/4) = 1.33

#So, Lift value is greater than 1 implies Rice is likely to be bought if Beer is bought.

# Let's build recommendation engine with our groceries data set (Use this data file in SAS as well, as that you can compare results.)
install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
# Import data set

data = read.transactions("groceries.csv", header=FALSE)

# Summart Stats

#To view the transactions, use the inspect() function instead.
inspect(head(data))

# number of items in each observation
size(head(data,3)) 

#To see the most frequently appeared items in the transactions

frequentItems <- eclat (data, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentItems)

# Plot Frequently appered items
itemFrequencyPlot(data, topN = 10)

#To get the product recommendation rules
# Fitting model
# Training Apriori on the data set
set.seed = 220 # Setting seed
Market_Basket = apriori(data = data, 
                        parameter = list(support = 0.004, 
                                         confidence = 0.2))
# Notes: If you want to get stronger rules, you have to increase the confidence. If you want lengthier rules increase the maxlen parameter. If you want to eliminate shorter rules, decrease the minlen parameter.

# Visualizing the results:
#Sort Rules by Confidence and Lift
# show the support, lift and confidence for all rules
inspect(sort(Market_Basket, by = 'lift')[1:10])         # 'high-lift' rules    
inspect(sort(Market_Basket, by = 'confidence')[1:10])  #  'high-confidence' rules.

#Analyzing the Results
#The rules with confidence of 1 (see above inspect sorted by Confidence) imply that, whenever the LHS item was purchased, the RHS item was also purchased 100% of the time.
#A rule with a lift of 26.7 (see above inspect sorted by Lift) imply that, the items in LHS and RHS are 26.7 times more likely to be purchased together.

# To run the below plot install and activate the package : arulesViz
library(arulesViz)
plot(Market_Basket, method = "graph", 
     measure = "confidence", shading = "lift")


# Extra Analysis: If you wanted to find out what customers had purchased before buying ‘bakery’ (Frequently purchased item after shopping bags as we do not have to do ant analysis on bags we choose Bakery, and also high confidence) . This will help you understand the patterns that led to the purchase of ‘bakery’.
# To get rules that lead to buying 'bakery'
extra_rules <- apriori (data=data, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="bakery"), control = list (verbose=F))
inspect(sort(extra_rules, by = 'confidence')[1:10])


# To find out the Customers who bought ‘BAKERY’ also bought 
extra_rules_2 <- apriori (data=data, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="bakery"), control = list (verbose=F)) # those who bought 'bakery' also bought..
inspect(sort(extra_rules_2, by = 'confidence'))
# From above results we can see that customer also brought vegetables and whole milk with bakery items.

