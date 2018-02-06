# Set working directory 
setwd("C:/Users/simon/Google Drev/Uni/2018 - Eye tracking")

# Libraries
library(dplyr)
library(stringr)
library(groupdata2)
library(cvms)
library(lmerTest)

# Load datafiles
fix=read.csv("FixationsV1.csv")
sac=read.csv("SaccadesV1.csv")
samp=read.csv("SamplesV1.csv")

# Subset data
fix_vis = subset(fix, Task == "VisualSearch")
fix_soc = subset(fix, Task == "SocialEngagement")

# Adding condition to Visual search
for (i in 1:nrow(fix_vis)){
  if (fix_vis$SearchOrder[i] == "1" & fix_vis$Trial[i] < 6) {
    fix_vis$Condition[i] = "Star"
  }
  if (fix_vis$SearchOrder[i] == "1" & fix_vis$Trial[i] > 5) {
    fix_vis$Condition[i] = "Count"
  }
  if (fix_vis$SearchOrder[i] == "2" & fix_vis$Trial[i] < 6) {
    fix_vis$Condition[i] = "Count"
  }
  if (fix_vis$SearchOrder[i] == "2" & fix_vis$Trial[i] > 5) {
    fix_vis$Condition[i] = "Star"
  }
}

# Adding condition to social engagement
# Load logfiles
folder = "PupilsLogs/"
fileList = list.files(path=folder, pattern="*.csv")
setwd("PupilsLogs")
pupil = do.call(rbind, lapply(fileList, function(x) read.csv(x, stringsAsFactors = F)))
setwd("C:/Users/simon/Google Drev/Uni/2018 - Eye tracking")

pupil$X = NULL

pupil$Trial = list(1, 2, 3, 4, 5, 6, 7, 8)

# Rename columns 
pupil = rename(pupil, "ParticipantID" = "subject")
  
# Merge files
fix_soc=merge(pupil, fix_soc)
  
# To get gender from video
fix_soc$ActorGender = str_extract(fix_soc$video, "\\w{1}")

# To get directness
for (i in 1:nrow(fix_soc)){
  if (grepl("div", fix_soc$video[i])) {
    fix_soc$Directness[i] = "Divergent"}
  if (grepl("dir", fix_soc$video[i])) {
    fix_soc$Directness[i] = "Direct"}
}

# To get ostensiveness
for (i in 1:nrow(fix_soc)){
  if (grepl("+o", fix_soc$video[i])) {
    fix_soc$Ostensiveness[i] = "Yes"}
  if (grepl("-o", fix_soc$video[i])) {
    fix_soc$Ostensiveness[i] = "No"}
}

# Replicate the above code to use on the other files

# Subset data
sac_vis = subset(sac, Task == "VisualSearch")
sac_soc = subset(sac, Task == "SocialEngagement")

# Adding condition to Visual search
for (i in 1:nrow(sac_vis)){
  if (sac_vis$SearchOrder[i] == "1" & sac_vis$Trial[i] < 6) {
    sac_vis$Condition[i] = "Star"
  }
  if (sac_vis$SearchOrder[i] == "1" & sac_vis$Trial[i] > 5) {
    sac_vis$Condition[i] = "Count"
  }
  if (sac_vis$SearchOrder[i] == "2" & sac_vis$Trial[i] < 6) {
    sac_vis$Condition[i] = "Count"
  }
  if (sac_vis$SearchOrder[i] == "2" & sac_vis$Trial[i] > 5) {
    sac_vis$Condition[i] = "Star"
  }
}

# Merge files
sac_soc=merge(pupil, sac_soc)

# To get gender from video
sac_soc$ActorGender = str_extract(sac_soc$video, "\\w{1}")

# To get directness
for (i in 1:nrow(sac_soc)){
  if (grepl("div", sac_soc$video[i])) {
    fix_soc$Directness[i] = "Divergent"}
  if (grepl("dir", sac_soc$video[i])) {
    sac_soc$Directness[i] = "Direct"}
}

# To get ostensiveness
for (i in 1:nrow(sac_soc)){
  if (grepl("+o", sac_soc$video[i])) {
    sac_soc$Ostensiveness[i] = "Yes"}
  if (grepl("-o", sac_soc$video[i])) {
    sac_soc$Ostensiveness[i] = "No"}
}

# To do the same for sample data

# Subset data
samp_vis = subset(samp, Task == "VisualSearch")
samp_soc = subset(samp, Task == "SocialEngagement")

# Adding condition to Visual search
for (i in 1:nrow(samp_vis)){
  if (samp_vis$SearchOrder[i] == "1" & samp_vis$Trial[i] < 6) {
    samp_vis$Condition[i] = "Star"
  }
  if (samp_vis$SearchOrder[i] == "1" & samp_vis$Trial[i] > 5) {
    samp_vis$Condition[i] = "Count"
  }
  if (samp_vis$SearchOrder[i] == "2" & samp_vis$Trial[i] < 6) {
    samp_vis$Condition[i] = "Count"
  }
  if (samp_vis$SearchOrder[i] == "2" & samp_vis$Trial[i] > 5) {
    samp_vis$Condition[i] = "Star"
  }
}

# Merge files
samp_soc=merge(pupil, samp_soc)

# To get gender from video
samp_soc$ActorGender = str_extract(samp_soc$video, "\\w{1}")

# To get directness
for (i in 1:nrow(samp_soc)){
  if (grepl("div", samp_soc$video[i])) {
    fix_soc$Directness[i] = "Divergent"}
  if (grepl("dir", samp_soc$video[i])) {
    samp_soc$Directness[i] = "Direct"}
}

# To get ostensiveness
for (i in 1:nrow(samp_soc)){
  if (grepl("+o", samp_soc$video[i])) {
    samp_soc$Ostensiveness[i] = "Yes"}
  if (grepl("-o", samp_soc$video[i])) {
    samp_soc$Ostensiveness[i] = "No"}
}

# Models to cross validate
m1 = lmer(Duration ~ Condition * Trial + (1+Trial|ParticipantID), fix_vis)
m2 = lmer(Duration ~ Condition + Trial + (1+Trial|ParticipantID), fix_vis)
m3 = lmer(Duration ~ Condition + (1|ParticipantID), fix_vis)

set.seed(1) # For reproducibility

data <- fold(fix_vis, k = 3,
             cat_col = 'Condition',
             id_col = 'ParticipantID') %>% 
  arrange(.folds)

mixed_models <- c("Duration ~ Condition * Trial + (1+Trial+ Condition|ParticipantID)", "Duration ~ Condition + Trial + (1+Condition + Trial|ParticipantID)", "Duration ~ Condition + (1 + Condition|ParticipantID)")

# To crossvalidate on normal data
CV1 <- cross_validate(data, mixed_models, folds_col = '.folds', family='gaussian', REML = FALSE)
CV1

# To crossvalidate on log data
CV2 <- cross_validate(data, "Duration ~ Condition", folds_col = '.folds', family='gaussian', link = "log", REML = FALSE)
CV2

# To create heat map for search
FixationsV2 = read.csv("FixationsV2.csv")
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
library(jpeg)
library(grid)
library(ggplot2)
img = readJPEG("EyeTrackingScripts/foraging/ng090ws.jpg")
g = rasterGrob(img, interpolate = TRUE) # make the image readable to R

ggplot(subset(FixationsV2, Task=='VisualSearch' & ParticipantID=='6_3_m2' & Trial==6), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) +
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")

# To heat map for count
img2 = readJPEG("EyeTrackingScripts/foraging/ng150ws.jpg")
g = rasterGrob(img2, interpolate = TRUE) # make the image readable to R

ggplot(subset(FixationsV2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==8), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) +
  ylim(0, 1080) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) + #xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour=FALSE, n=1000) + 
  scale_alpha(range = c(0.1, 0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans="sqrt")

# To create a scan path
x= subset(FixationsV2, Task=='VisualSearch' & ParticipantID=='3_1_f1' & Trial==8)
x=x[order(x$Fixation),]
ggplot(x, aes(x = PositionX, y = 1081-PositionY)) +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-0, ymax=1080) +
  geom_point(size = x$Duration/25, alpha = 0.5, color = "red") +
  geom_path(size = 1, alpha = 0.3) +
  geom_text(aes(label =Fixation, size = 5)) 

# To visualize the emotional engagement experiment
SamplesV2=read.csv("SamplesV2.csv")

ggplot(subset(SamplesV2, Task == "SocialEngagement"), aes(x=TrialTime, y=PupilSize, color = ParticipantGender)) + 
  geom_smooth() + facet_grid(~ActorGender) + xlim(0,6000)

ggplot(subset(SamplesV2, Task == "SocialEngagement"), aes(x=TrialTime, y=PupilSize, color = Directionality)) + 
  geom_smooth() + facet_grid(~Ostension) + xlim(0,6000)
