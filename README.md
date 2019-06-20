# Stack_analysis  
Unfortunatelly all .R files have polish commentary.  

## Project Description
This project was done in pairs. We were supposed to choose at least 3 data sets with one being at least 100 MB and show any intresting phenomena connected with it. Directories containe files that were effect of analysis done in R. They are used by Rshiny for presenting resoults.  

### the resoults can be found in Presentation_final.pdf and in Rshiny web apps  


We focused on two problems.  

## Tag analysis in Rshiny
### files: Taganalysis.R
This analysis can very easly show various intresting behaviours of stack forum.  
We looked e.g. for things that were trending in DataScience, Music and Gaming forums but tags from different forums hold their own unique data. For example let's look at Gaming forum.  
Looking at a graph of number of posts with specyfic tag (e.g. terraria) with time axis can very efficently show month of updates of a game. It can be done with many popular games that updates are big and rare enough.  
This is one unique behaviour of gaming forum, and every other forum has many of it own's. Many of them can be found thanks to Tag Analysis. 


## Succesful method of finding experts on stack forum
### files: Students.R, Experts.R
This is more important part of the project and many intresting ideas (that are not mentioned in presentation) can be found in .R files. The hardest part was to find the values and aspects that could be correlated with expertise. First we looked for a good linear correlation between the vaues that, but we quicly understood was that Spearman's correlation was better method and it describes resoults more accurately. 

If you still have any question, I recommend looking into presentation and and the into the .R files,  
if you still can not find what you are looking for please leave a issue. 
