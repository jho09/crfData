library(tidyverse) 
crf.data<-read_csv("crfData.csv")

# Steps:
# interaction
# simple main effects
# paired comparisons

crf.data$anxiety <- as.factor(crf.data$anxiety) 
crf.data$preparation <- as.factor(crf.data$preparation)

levels(crf.data$anxiety) <- list("Low Anxiety"=1, 
                                 "High Anxiety"=2) 

levels(crf.data$preparation) <- list("Low Preparation"=1,
                                     "Medium Preparation"=2,
                                     "High Preparation"=3)


# Setting contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# Run the analysis
crf.lm <- lm(mark ~ anxiety * preparation, data=crf.data)

# apaTables
library(apaTables)
apa.aov.table(crf.lm,filename="Table1.doc")

apa.2way.table(iv1=preparation, iv2=anxiety, dv=mark, data=crf.data,
               show.marginal.means = TRUE, filename="Table2.doc")

install.packages("phia",dep=TRUE)
library(phia)

testInteractions(crf.lm, fixed="anxiety", across="preparation",adjustment="none")

