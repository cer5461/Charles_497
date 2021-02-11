# Manupulating text in R

#1. Find a sentence online. Save it as a string. 

require(quanteda)

cia <- 'Targeting Officers identify the people, relationships, and organizations with access to information needed to find opportunities to disrupt threats to U.S. interests.'

#2. Select only the third word of the sentence. Save it as a new string.
ciathirdword <- substr(cia, 20,27)

#3. Choose a letter that appears in your sentence. Use the gsub command to replace all instances of that letter with a period. 

gsub('e', '.', cia)

