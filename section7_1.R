# How to evaluate our model? With Confusion Matrix
options(pillar.sigfig = 7)

data  <- tribble(
  ~Person, ~favourite_pet, ~predicted_pet,
  "Lewis", "cat", "cat",
  "Alex", "cat", "cat",
  "Jono", "rabbit", "rabbit",
  "Sasha", "dog", "dog",
  "Steve", "dog", "rabbit",
  "Gavin", "dog", "rabbit",
  "Dee", "cat", "dog",
  "Max", "cat", "rabbit",
  "Alison", "rabbit", "cat"
)


grouped_observations <- count(data, favourite_pet, predicted_pet, name = 'count')
# perform hierachical group-by for the two columns, then counts 
# observations in the groups

findings_AdxTeam <- spread(grouped_observations, key = favourite_pet, value = count, fill= 0)
# so the key becomes the column names, the value becomes the values in those columns, and
# the index is the remaining column (predicted_pet)

print(findings_AdxTeam)
# so how you read this is, row by row.
# e.g. for predictions of fav pet to be cat, the actual favourites had 2 ppl say cat, 
# but 1 person liked rabbit actually (Alison).  

#############################################################
# How to summarise this? We can look at a single level (cat or dog or rabbit), 
# and see if our classifier got it right or not.
# We call this the Confusion Matrix.
# It is a 2x2 matrix of TP, FP, FN, TN.

data <- mutate(data, actual_fav_cat = ifelse(favourite_pet == "cat", "yes", "no"),
               predicted_fav_cat = ifelse(predicted_pet == "cat", "yes", "no"))

grouped_observations2 <- count(data, predicted_fav_cat, actual_fav_cat, name ="count")
print(grouped_observations2)

confusion_matrix <- spread(grouped_observations2, key = actual_fav_cat, value = count, fill = 0)
print(confusion_matrix)

# actual_yes - pred_yes is true positive (TP),
# actual_no - pred_no is true negative (TN).
# actual_yes - pred_no is false negative (FN).
# actual_no - pred_yes is false positive (FP).

# sensitivity = TP/(TP+FN)
# specificity = TN/(TN+FP)
# false +ve rate (FPR) = FP/(FP + TN)
# true +ve rate (TPR) = TP/(TP + FN)

# precision = TP/(TP + FP)
# recacll = TPR





# 