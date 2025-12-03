## ----echo = FALSE, warning=FALSE, message = FALSE, results = 'hide'-----------
cat("this will be hidden; use for general initializations.\n")
library(superb)
options("superb.feedback" = c("warnings"))
library(ggplot2)

## -----------------------------------------------------------------------------
fmri <- read.csv(url("https://raw.githubusercontent.com/mwaskom/seaborn-data/de49440879bea4d563ccefe671fd7584cba08983/fmri.csv"))

## ----fig.height=4, fig.width=7, fig.cap = "**Figure 1**. Plot of the fMRI data with standalone confidence intervals."----
superb( 
    signal ~ timepoint + region + event | subject,
    fmri,
    plotLayout = "lineBand",
    pointParams = list(size=1,color="black"),
    lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:18) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green")) +
theme_bw() + ylim(-0.15, +0.35) 

## ----fig.height=4, fig.width=7, fig.cap = "**Figure 2**. Plot of the fMRI data with Cousineau-Morey decorrelation."----
superb( 
    signal ~ timepoint + region + event | subject,
    fmri,
    adjustments = list(decorrelation = "CM"), ## only new line
    plotLayout = "lineBand",
    pointParams = list(size=1,color="black"),
    lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:19) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green"))+
theme_bw() + ylim(-0.15, +0.35) + 
showSignificance(c(6,7)+1, 0.305, -0.02, "n.s.?", panel=list(event=2)) 

## ----fig.height=4, fig.width=7, fig.cap = "**Figure 3**. Plot of the fMRI data with local decorrelation."----
superb( 
    signal ~ timepoint + region + event | subject,
    fmri,
    adjustments = list(decorrelation = "LD2"),  ## CM replaced with LD2
    plotLayout = "lineBand",
    pointParams = list(size=1,color="black"),
    lineParams = list(color="purple")
) + scale_x_discrete(name="Time", labels = 0:19) + 
scale_discrete_manual(aesthetic =c("fill","colour"), 
                      labels = c("frontal","parietal"), 
                      values = c("red","green"))+
theme_bw() + ylim(-0.15, +0.35) + 
showSignificance(c(6,7)+1, 0.305, -0.02, "**!", panel=list(event=2)) 

## ----ThisChunk----------------------------------------------------------------
# First, extract the two sets of data, ordered by subject identifier:
d1 <- dplyr::filter(fmri, event=="stim" & region=="parietal" & timepoint==6)
d1 <- d1[order(d1$subject),]
d2 <- dplyr::filter(fmri, event=="stim" & region=="parietal" & timepoint==7)
d2 <- d2[order(d2$subject),]

# Second, run a paired t-test
t.test(d1$signal, d2$signal, paired=TRUE)

