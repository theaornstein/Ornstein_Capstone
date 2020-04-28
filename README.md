---
title: "Ornstein_Thea_Capstone"
author: "Ornstein_Thea"
date: "4/22/2020"
output: html_document
---


**1) Fibroblasts possess the ability to sense and exert mechanical forces in their micro-environment. The degree of traction forces (tension) that these cells apply has been demonstrated to be correlated with substrate stiffness, elasticity and cell spread area [1]. The actin cytoskeleton is important in traction force generation and actin polymerization can be prevented using the drug Latrunculin. The primary receptors responsible for transmitting cytoskeletal forces to the extracellular matrix are the integrins. To measure single integrin tension, the Salaita lab has previously designed an irreversible DNA-based tension probe [2]. The assay works in the following way: when a cell exerts tension through a single integrin ligand (which is presented to the cell by the probe), a fluorophore becomes unquenched and a fluorescent signal is observed. Since the probe is irreversible, the fluorescent signal will continue, and the probe will not refold (to quench the fluorophore). Thus, an equivalent measure for the degree of mechanical activity (traction force generation) that a single cell is exerting on its substrate is integrated fluorescent intensity (IFI; fluorescent intensity normalized for cell area) of the whole cell.  Although Latrunculin has been shown to decrease traction forces when cellular forces are measured with traction force microscopy (a bulk measurement), it is unknown whether this decrease will be resolved on the single integrin receptor level. When the appropriate dosage of Latrunculin is applied to fibroblasts, which abolishes actin polymerization, integrin-mediated traction forces are expected to decrease as measured using irreversible DNA-based tension probe. This is a significant experiment since it establishes an important negative control for future experiments and can be used as a tool to ask questions about the role of other cytoskeleton filaments (microtubules and intermediate filaments) in traction force generation.**

**Citations: [1]Polacheck, W. J. & Chen, C. S., Nature methods 13:(5) 2016. [2]Zhang, Y. et al., Nat Commun. 55:5167, 2014**

**2) Although Latrunculin has been shown to decrease traction forces when cellular forces are measured with traction force microscopy, it is unknown whether this decrease will be observed on the single integrin receptor level.**

**3) If immortalized, mouse embryonic fibroblasts (NIH 3T3) are treated with the optimal dosage of Latrunculin for abolishing actin polymerization, then single integrin tension (or integrated fluorescent intensity) should decrease when compared to the tension (integrated fluorescent intensity) measured from vehicle-treated NIH 3T3 cells.**

**4) The dependent variable will be integrated fluorescent intensity (IFI) which is a continuous, measured variable. The independent (predictor) variable will be treatment with two levels: Latrunculin-treated and vehicle-treated NIH 3T3 cells. The independent variable is a discrete, sorted variable.**

**5) Null: The mean difference of IFI between levels of the predictor (latrunculin-treated or vehicle-treated NIH 3T3 cells) will be greater than or equal to zero. Alternative: The mean difference of IFI between levels of the predictor (latrunculin-treated or vehicle-treated NIH 3T3 cells) will be less than zero.**

**6) To test the hypothesis in item #5, I will choose a one-sided, paired t-test. This is appropriate for the experimental variables since t-tests are suitable for a continuous, measured dependent variable, which in this experiment is IFI. A paired t-test is chosen over an unpaired t-test since the experiment uses an immortalized cell line in which all cells are assumed to be identical and thus are intrinsically linked. An alternative approach would be using a non-parametric test if the data appeared to not follow a normal distribution. We expect the collected measurements from this experiment to be normally distributed, thus a paired t-test is chosen for this simulation. A one-sided paired t-test is specifically chosen since we know from previous literature that Latrunculin should decrease traction forces and IFI. ** 

**7) The experiment will consist of three independent replicates, completed a week apart from one another. The experiments will all be performed on Wednesdays. Each replicate will use new reagents to randomize the blocks. The cells in all replicates and for each treatment level will be from three successive passage (5,6,7). This introduces some randomness to the blocks by using different passage numbers. To complete this, vials of passages 5,6,7 of NIH 3T3 cells will be frozen and put in cryopreservation. Two-days prior to the day of the experiment (Monday), the cells will be taken out of cryopreservation, thawed and cultured in two, T-25 flasks. One flask will contain media with the Latrunculin drug (optimal dosage assumed to be worked out in other preliminary experiments) and the other flask will contain media with the vehicle control. The flasks will be labeled to know which cells are drug-treated and which are vehicle-treated.** 

**For one independent replicate (i.e. week one), the two treatment conditions (Latrunculin and vehicle) will be completed in parallel. 30,000 cells will be placed on a single surface with the tension sensors. The surface assignment for each cell treatment will be randomly assigned and imaging will be done without knowing the cell treatment type for that specific surface. All surfaces are assumed to be identical, meaning the same number of tension sensors are functional on each surface. Three biological replicates will be completed and averaged in the final analysis. Thus, on the first Wednesday, six surfaces will be imaged and IFI will be measured (3 of drug-treated and 3 of vehicle-treated). Cells will be incubated on the tension sensor surfaces for thirty minutes prior to imaging and all imaging must be completed ten minutes after the start time. Single cells will be imaged and used in analysis. This procedure will be repeated for the next two weeks in order to complete three independent replicates.**

**For image analysis and IFI measurement acquisition, images will be individually thresholded by subtracting the mean background from three rectangular sections of the image (areas of surface with no cells). ImageJ/Fiji will be used for all image analysis and analysis will be completed without knowing which cell treatment is captured in the image (we will use the same random assignment labels used during imaging). After thresholding, cells will be traced and IFI will be measured. An excel spreadsheet will be compiled for each independent replicate and once all experimentation is complete, a compiled spreadsheet will be created. In terms of decision thresholds, alpha will be set to 0.05 (type1 error will be 5%) and power will be set to 0.80 (type2 error will be 20%). The null hypothesis will be rejected if the p-value is less than 0.05.**

**In this experimental protocol, an independent replicate will be defined by time (completing the replicates on different weeks) and passage (each replicate is on a successive passage).**


**8) For the simulated data, 20000000 was used as the average for the vehicle-treatment and 10000000 was used as the average for the Latrunculin-treatment. The standard deviation used for both was 5000000. These numbers come from unpublished experiments I conducted in my current lab.**

```{r message=TRUE, warning=TRUE, include=FALSE}

library(tidyverse)
library(readxl)
library(Hmisc)
library(cowplot)
```

```{r}
set.seed(1234)

vehicle <-rnorm(100,20000000,5000000)
drug<-rnorm(100,18000000,5000000)

data<-tibble(vehicle.treated=vehicle, Latrunculin.treated=drug, id=paste("#", c(1:100), sep="")) %>%
  pivot_longer(cols=c(vehicle.treated, Latrunculin.treated),
               names_to="Predictor",
               values_to="Response")

data.d <- data %>% 
  select(id, Predictor, Response) %>% 
  group_by(id) %>% 
  summarise(dif=diff(Response))


p1 <- ggplot(data, aes(Predictor, Response, group=id)) +
  geom_point(size=4) +
  geom_line(color="violet") +
    theme_half_open(12) +
    theme(plot.margin = margin(10, 2, 4, 2))+
  scale_color_viridis_d(begin=0, end=0.8) +
  labs(x="Treatment Type", y="Integrated Fluorescent Intensity (a.u.)")

p2 <- ggplot(data.d, aes(x=factor("Latrunculin.treated - vehicle.treated"), y=dif))+
  geom_point(size=4) + 
  stat_summary(fun.data = mean_cl_normal,
               fun.args = list(conf.int=0.99),
               geom="crossbar", 
               width=0.2, 
               color="black"
               )+
  geom_hline(aes(yintercept=0), 
             color="violet", size=2, 
             linetype="dashed")+
    theme_half_open(12) +
    theme(plot.margin = margin(10, 2, 4, 2))+
    scale_color_viridis_d(begin=0, end=0.8) +
    labs(x="Treatment Type", y="Integrated Fluorescent Intensity (a.u.)")

result<-plot_grid(p1, p2, labels="AUTO")
result

```


**9) A n of 100 will give a power of 0.804. A two sided alternative hypothesis was used in the simualtion to be more conservative. **

```{r}
set.seed(12345)

t.pwr <- function(n){

# Vehicle.treated
  m1=20000000 
  sd1=5000000

# Latrunculin.treated
  m2= 18000000 
  sd2=5000000 
  
  alpha=0.05 
  ssims=1000
  
  p.values <- c()
  i <- 1
  repeat{
    x=rnorm(n, m1, sd1); 
    y=rnorm(n, m2, sd2);
    
    p <- t.test(x, y, 
                paired=T, 
                alternative="two.sided", 
                var.equal=T,
                conf.level=1-alpha)$p.value
    p.values[i] <- p
    
    if (i==ssims) break
    i = i+1
    
    pwr <- length(which(p.values<alpha))/ssims
  }
  return(pwr)
  
}

t.pwr(100)
```
