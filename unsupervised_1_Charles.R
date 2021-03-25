# Set up workspace
rm(list = ls())

setwd("C:/Users/kevin/Documents/GitHub/TAD_2021/R lessons/")

# Loading packages
#install.packages("factoextra")

library(quanteda)
library(factoextra)
library(dplyr)

## 1 PCA is an old technique and exists within base R

# 1.1  function in base R for PCA:
# see: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
?prcomp # uses the singular value decomposition approach: examines the covariances/correlations between individuals

# Remember to center your data! (default = TRUE) -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!



# 1.2 Example
data("data_corpus_inaugural")
inaugural <- corpus_subset(data_corpus_inaugural, Year > "1900-01-01")

inaugural_dfm <- dfm(inaugural, 
                     stem = T, 
                     remove_punct = T, 
                     remove = stopwords("english")
)

#transform into a matrix in order to use pcomp() function
inaugural_mat <- convert(inaugural_dfm, to = "matrix") # convert to matrix

# run pca with center and scale TRUE so all variables are all on same scale
inaugural_pca <- prcomp(inaugural_mat, center = TRUE, scale = TRUE)

# visualize eigenvalues (scree plot: shows percentage of variance explained by each dimension)
# we would like to see that a higher % of the variance is explained by a smaller set of dimensions 
# a good rule of thumb in choosing principle components are the ones where there are additional gains. For 
# example, choose 4 here because the difference between choosing 5 and 6 is pretty low (diminishing marginal explanatory power)
fviz_eig(inaugural_pca, addlabels = TRUE)

# Loadings for each variable: columns contain the eigenvectors
inaugural_pca$rotation[1:10, 1:5]
dim(inaugural_pca$rotation)

# Q: can we interpret the dimensions?
# top loadings on PC1
pc_loadings <- inaugural_pca$rotation

# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  # these should be orthogonal

# token loadings
N <- 10
pc2_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,2])) %>% arrange(-loading)
pc2_loading$loading <- scale(pc2_loading$loading, center = TRUE)
pc2_loading <- rbind(top_n(pc2_loading, N, loading),top_n(pc2_loading, -N, loading))
pc2_loading <- transform(pc2_loading, token = factor(token, levels = unique(token)))

# plot top tokens according to absolute loading values
ggplot(pc2_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc2_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=3),
        axis.title.y = element_text(size=3, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))

#words with highest correlation are words that signify generic appeals to citizenship 
#farthest negative are words that are about policy (practical, pragmatic words)
#the blessing and curse of unsupervised models is that its up to us to assign a label to these principle components 

# Value of the rotated data: your "new", dimensionality reduced data
View(inaugural_pca$x)  # each observation 

#if we accept the pragmatic interpretation of PC1, then Taft is really really really pragmatic 
#if in fact Taft's address was very pragmatic, then this lens "face validity" to this approach 
## we've run the numbers and found something that makes sense, which should make us more confident that 
## the overall document has been successfully implemented 
#Trump is not extreme in any dimension, which suggests that he is a departure 

# retrieve most similar documents
#install.packages("text2vec")
library(text2vec)

# function computes cosine similarity between query and all documents and returns N most similar
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}

# apply to document retrieval
nearest_neighbors(query = "2017-Trump", low_dim_space = inaugural_pca$x, N = 5, norm = "l2")

##### Exercise ######

# Question 1: Update the code so that it calculates the terms with the 
#top loadings on PC2. What is the theme of this dimension?
## Very hard to say: positively correlated words seem to be associated with foreign affairs, crisis, and hope. For example, autocracy, 
## warfare, new-world, indebtedness, aftermath, lifeblood, nation-wide, wreckage, unprepared
## words that are negatively correlated are even harder to extrapolate from. Some seem to be about the calendar, such as time,
## year, day while others are about difficulty and support, such as plight, struggle, hand, and intend

# find dimension is closest to time 


# Question 2: Who are the 5 people Obama's inaugural address is most
#close to in 2013? What about Trump in 2017?
## Obama's 2013 is most similar to 2009-Obama, 2001-Bush, 1997-Clinton, 1993-Clinton, and 2017-Trump 
## Trump's is most similar to 45 Roosevelt, 93 Clinton, 69 Nixon, 97 Clinton and 1905 Roosevelt 
## 