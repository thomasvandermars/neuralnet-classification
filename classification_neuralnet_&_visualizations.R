# import statements
library(here)
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)
library(mltools)
library(NeuralNetTools)
library(Rcpp)
library(RSNNS)
library(data.table)
library(viridis)
library(RColorBrewer)
library(fmsb)

source(here("utils.R"))

############# PARAMETERS ############# 

data_folder <- "data"
data_file <- "wines.xlsx"

############# IMPORT DATA ############# 

# if we detect an .xlsx file
if (strsplit(data_file, ".", fixed = TRUE)[[1]][-1] == "xlsx") {
  # read it as an .xlsx file
  data <- read_excel(here(data_folder, data_file))
} else if (strsplit(data_file, ".", fixed = TRUE)[[1]][-1] == "txt") {
  # read it as a .txt file
  data <- read.table(file = here(data_folder, data_file), sep = ",", header = TRUE) 
} else {
  print("Dataset file has to be a .xlsx or .txt extension!")
}

############## REMOVE UNWANTED VARIABLES ##############

data[c("OD280_OD315_of_diluted_wines", "Alcalinity_of_ash")] <- NULL

############## REMOVE SPACES FROM VARIABLE NAMES ############## 

names(data) <- sub(" ", "_", names(data))

############## HANDLE MISSING DATA ############## 

if (any(is.na(data))) {
  data <- missing_data_handling(x = data, method = "omit")
}

############## EVALUATE CORRELATIONS BETWEEN VARIABLES ##############

cor_matrix <- cor(data)
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_matrix <- reshape2::melt(cor_matrix, na.rm = TRUE)

ggplot(data = cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  ggtitle("Correlation Matrix") +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "orange", high = "purple", mid = "seashell1", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))

############## EVALUATE RELATIONSHIP BETWEEN SPECIFIC VARIABLES ##############

# order the data by the variable that is used for sizing bubbles (descending order)
# makes sure that largest bubbles are plotted first...
data_ordered <- data[order(-data$Proline), ]

# factorize wine types as the variable used for coloring the bubbles
Wine_Types <- as.factor(data$Wine_Type)

ggplot(data_ordered, aes(x=Flavanoids, y=Alcohol, size = Proline, color = Wine_Types, fill = Wine_Types)) +
  geom_point(alpha = 0.5) +
  scale_size(range = c(.1, 16), name="Proline")+
  ggtitle("Wine Types") + 
  #scale_color_viridis(discrete = TRUE, option="viridis") + 
  scale_colour_brewer(palette = "Dark2") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.background = element_rect(fill = "white", color = 'black', linewidth = 1.0),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted', linewidth = 0.5),
        panel.grid.minor = element_line(color = 'grey', linetype = 'dotted', linewidth = 0.5),
        legend.position = "right",
        legend.background = element_rect(fill = "white", # Background
                                         linetype = 'dashed',
                                         colour = 1))

############## NORMALIZE & ENCODE INPUT & TARGET VARIABLES ############## 

# extract input variables & normalize on [0,1] scale
X <- as.data.frame(apply(X = data[,-1], # exclude the target label 
                         MARGIN = 2, # apply the function along the column dimension 
                         FUN = normalize))

# extract target variable and create a one-hot encoding.
Y <- one_hot(data.table::as.data.table(as.factor(data$Wine_Type)))
colnames(Y) <- c("Wine_Type_1", "Wine_Type_2", "Wine_Type_3")

############## RADARPLOT WITH STANDARDIZED VARIABLE MEANS ############## 

temp <- aggregate(X, list(data$Wine_Type), FUN=mean) 
rownames(temp) <- c("Wine Type #1", "Wine Type #2", "Wine Type #3")
temp <- temp[2:length(colnames(temp))]
temp <- rbind(rep(max(temp),length(colnames(temp))) , rep(min(temp),length(colnames(temp))), temp)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart(temp, 
           axistype = 0,
           seg = 6, # number of segments. 4 is default.
           pty = 16, # point symbol. 16 is default.
           pcol = colors_border, 
           pfcol = colors_in, 
           plwd = 2, # line-thickness 
           plty = 1, # line-style
           vlcex = 0.5, # font size
           centerzero = TRUE,
           title = "Standardized [0-1] Variable Means") 

# Add a legend
legend(x = 1.5, 
       y = 0.2, 
       legend = rownames(temp)[3:length(rownames(temp))], 
       bty = "n", 
       pch = 20 , 
       col = colors_in , 
       text.col = "grey", 
       cex = 0.9, 
       pt.cex = 2)

############## TRAIN/TEST PARTITION ############## 

YX_split <- partition(x = cbind(Y, X), fraction_training = 0.90)

X_train <- YX_split$train[,(ncol(Y)+1):(ncol(X)+ncol(Y))]
Y_train <- YX_split$train[,1:ncol(Y)]
X_test <- YX_split$test[,(ncol(Y)+1):(ncol(X)+ncol(Y))]
Y_test <- YX_split$test[,1:ncol(Y)]

rm(YX_split)

############## TRAIN NEURAL NETWORK FOR CLASSIFICATION ##############

set.seed(10) # set random seed for reproducible results

nn <- mlp(x = X_train,
          y = Y_train,
          size = c(10,5),
          maxit = 200, # maximum of iterations (epochs) to learn
          inputsTest = X_test,
          targetsTest = Y_test) 

############## COMPUTE CLASSIFICATION ACCURACY ##############  

train_accuracy <- compute_accuracy(model = nn, x = X_train, y = Y_train)
test_accuracy <- compute_accuracy(model = nn, x = X_test, y = Y_test)
paste(c("Train Accuracy: ", round(train_accuracy*100,2), "%"), collapse = "")
paste(c("Test Accuracy: ", round(test_accuracy*100,2), "%"), collapse = "")

############## PLOT NEURAL NETWORK ##############

plotnet(mod_in = nn, # neural network object
        nid = TRUE, # neural interpretation diagram is plotted, default TRUE
        all_out = TRUE, # names of response variables for which connections are plotted
        all_in = TRUE, # names of input variables for which connections are plotted
        rel_rsc = c(1, 7), # scaling range for the width of connection weights
        circle_cex = 3.5, # size of nodes, default 5
        node_labs = TRUE, # labels are plotted directly on nodes
        var_labs = TRUE, #  if variable names are plotted next to nodes
        line_stag = NULL, # distance of connection weights from nodes
        cex_val = 0.8, # size of text labels, default 1
        alpha_val = 0.3, # transparency of connections, default 1
        circle_col = "lightblue", # color of nodes
        pos_col = "green", # positive connection weights
        neg_col = "red", # negative connection weights
        bord_col = "black", # border color around nodes
        max_sp = TRUE, # space between nodes in each layer is maximized
        pad_x = 0.75, #  padding on the x-axis, <1 increase padding and >1 decrease padding
        prune_col = NULL, # color of pruned connections
        prune_lty = "dashed") # line type for pruned connections