# Descriptive practice


#1. Write two sentences. Save each as a seperate object in R. 

require(quanteda)
txt_exercise <- c(sentence1 = "I am going to starve to death.",
                  sentence2 = "I really want to eat tacos.")

#2. Combine them into a corpus
corpus_ex <- corpus(txt_exercise)

#3. Make this corpus into a dfm with all pre-processing options at their defaults.
dfm_ex <- dfm(corpus_ex)

#4. Now save a second dfm, this time with stopwords removed.
?dfm
dfm_ex_stop <- dfm(corpus_ex, remove = c(stopwords("english")))

#5. Calculate the TTR for each of these dfms (use textstat_lexdiv). Which is higher?
?textstat_lexdiv
textstat_lexdiv(dfm_ex)
textstat_lexdiv(dfm_ex_stop)
#TTR is higher for the dfm in which stop words are removed 
#6. Calculate the Manhattan distance between the two sentences you've constructed (by hand!)

abs(.8571429 - 1) + abs(1 - 1)
#manhattan distance = 0.1428571