################################################################################
# Set Up
################################################################################

# remove.packages("ddandrda)
# install.packages("devtools")
# devtools::install_github("hannahblo/ddandrda")


library(ddandrda)
library(OpenML)
library(dplyr)
library(gurobi)
library(oofos)



################################################################################
# Old!!! Data Set - ignore the next part
################################################################################
# 
# library(prefmod) # Just help out until we got a data set
# # Information about the data set, page 5f
# # https://cran.r-project.org/web/packages/prefmod/prefmod.pdf
# data_set <- prefmod::cemspc
# head(data_set)
# View(data_set)
# # The data set contains a mistake:
# data_set <- data_set[!is.na(rowSums(data_set)), ]
# # There is a mistake in the data set, see
# # https://www.academia.edu/17714335/Corrigendum_Modelling_the_effect_of_subject
# # -specific_covariates_in_paired_comparison_studies_with_an_application_to_
# # university_rankings
# data_set[ ,15] <- 2 - data_set[ ,15]
# 
# # Converting the data set into matrices which represent the partial orders
# # note that not necessarily every matrix represents a antisymmetric relation
# 
# convert_to_matrix <- function(row_values, set_NA_to = NA) {
#   # set_NA_to, can equal 0, 1, 2 or NA,
#   #   0: incomparable, 1: first is better, 2: second better
# 
#   # Result: Matrix wrapped up in a list with rows/columns refer to
#   #       (1:"London", 2:"Paris", 3:"Milano", 4:"St. Gallen", 5:"Barcelona", 6:"Stockholm")
# 
# 
#   graph_mat <- matrix(rep(-1, 6*6), nrow = 6)
#   diag(graph_mat) <- 1
# 
#   comparison_index <- c(c(1,2), c(1,3), c(2,3), c(1,4), c(2,4), c(3,4),
#                         c(1,5), c(2,5), c(3,5), c(4,5),
#                         c(1,6), c(2,6), c(3,6), c(4,6), c(5,6))
#   # vergleich dies mit der Definition von V1- V15 auf Seite 6 von
#   # https://cran.r-project.org/web/packages/prefmod/prefmod.pdf
# 
#   for (i in 1:15) {
#     if (is.na(row_values[i])) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- (set_NA_to %% 2) # modulo 2
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- max(set_NA_to - 1, 0)
#     } else if (row_values[i] == 0) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 1
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 0
#     } else if (row_values[i] == 1) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 0
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 0
#     } else if (row_values[i] == 2) {
#       graph_mat[comparison_index[i*2 - 1], comparison_index[i*2]] <- 0
#       graph_mat[comparison_index[i*2], comparison_index[i*2 - 1]] <- 1
#     }
#   }
#   return(as.matrix(graph_mat))
# }
# 
# # convertieren der einzelnen Zeilen zu matrix
# graph_mat <- list()
# index <- 1
# for (index_loop in 1:dim(data_set)[1]) {
#   graph_mat[[index_loop]] <- convert_to_matrix(data_set[index_loop, ])
# }
# 
# 
# # check which one are dags
# check_po <- unlist(lapply(graph_mat, test_if_porder)) #  ddandrda::
# data_po <- graph_mat[check_po]
# 
# # Overview
# dim(data_set) # [1] 212  17
# length(data_po) # 98




################################################################################
# ML Data Set Preparation
################################################################################
datasets <- listOMLDataSets()

rel_datasets <- datasets %>% filter(status == "active" &
                                     number.of.classes == 2 &
                                     number.of.features < 100 &
                                     number.of.instances < 1000 &
                                     number.of.instances > 100 &
                                     number.of.instances.with.missing.values == 0 &
                                     max.nominal.att.distinct.values < 5
)
flows <- listOMLFlows()


#### flows 2333 (rpart), 2330 (ranger), 2409 (knn), 2337 (xgboost), 2408 (glmnet), 2336(svm), 2317(logit), 2313 lda
flows_for_paper <- c(2333, 2330, 2409, 2337, 2408, 2336, 2317, 2313)
outls = list()
for (i in 1:length(flows_for_paper)) {
  temp <- listOMLRunEvaluations(flow.id = flows_for_paper[i], limit = 10000)
  if (i == 1) {
    datasets = temp$data.name
  } else {
    datasets = intersect(datasets,temp$data.name)
  }
  outls[[i]] = temp
}
data_final <- do.call('rbind', outls)
data_final <-  data_final[data_final$data.name %in% datasets,]
data_final <- data_final %>% group_by(flow.id, data.name) %>% slice(n())

data_final <- data_final[order(data_final$data.name), ]

View(data_final)
dim(data_final) # [1] 1616   30

colnames(data_final)
# [1] "run.id"                        "task.id"                      
# [3] "setup.id"                      "flow.id"                      
# [5] "flow.name"                     "flow.version"                 
# [7] "flow.source"                   "learner.name"                 
# [9] "data.name"                     "upload.time"                  
# [11] "area.under.roc.curve"          "average.cost"                 
# [13] "f.measure"                     "kappa"                        
# [15] "kb.relative.information.score" "mean.absolute.error"          
# [17] "mean.prior.absolute.error"     "number.of.instances"          
# [19] "precision"                     "predictive.accuracy"          
# [21] "prior.entropy"                 "recall"                       
# [23] "relative.absolute.error"       "root.mean.prior.squared.error"
# [25] "root.mean.squared.error"       "root.relative.squared.error"  
# [27] "total.cost"                    "usercpu.time.millis"          
# [29] "usercpu.time.millis.testing"   "usercpu.time.millis.training" 

learner_unique <- unique(data_final$learner.name)
length(learner_unique)
learner_unique
# [1] "classif.lda"      "classif.multinom" "classif.ranger"  
# [4] "classif.rpart"    "classif.svm"      "classif.xgboost" 
# [7] "classif.glmnet"   "classif.kknn"  

data_set_unique <- unique(data_final$data.name)
length(data_set_unique) # [1] 202

data_set_eval <- data_final[, c("data.name", "learner.name", 
                              "f.measure", "predictive.accuracy",
                              "area.under.roc.curve", 
                              "root.mean.squared.error")]
data_set_eval[ ,"root.mean.squared.error"] <- 
  1 - data_set_eval[ ,"root.mean.squared.error"]

data_set_eval <- data_set_eval[data_set_eval$learner.name %in%
                                       c("classif.ranger",
                                         "classif.rpart", 
                                         "classif.svm", 
                                         "classif.xgboost",  
                                         "classif.glmnet",
                                         "classif.kknn"), 
                               ]


convert_to_matrix <- function(single_data_eval) {
  list_learner <- single_data_eval$learner.name
  if (length(list_learner) != length(unique(list_learner))) {
    print(single_data_eval[ ,"data.name"]) 
    stop("Fehler im obigen Datensatz")
  }
  number_learner <- length(list_learner)
  graph_mat <- matrix(rep(0, number_learner * number_learner), 
                      nrow = number_learner)
  rownames(graph_mat) <- colnames(graph_mat) <- list_learner
  diag(graph_mat) <- 1
  
  for (learner_i in list_learner) {
    learner_base <- single_data_eval[
      which(single_data_eval$learner.name == learner_i), 
      -c(1, 2)]
    for (learner_j in list_learner) {
      learner_comp <- single_data_eval[
        which(single_data_eval$learner.name == learner_j),
        -c(1, 2)]
      if (all(learner_base < learner_comp)) {
        graph_mat[learner_i, learner_j] <- 1
      }
    }
  }
  return(graph_mat)
}

single_data_eval <- data_set_eval[seq(1,6), ]
convert_to_matrix(single_data_eval)
list_mat_porders_ml <- rep(NA, length(data_set_unique))

for (i in seq(1, length(data_set_unique))) {
  list_mat_porders_ml[i] <- list(convert_to_matrix(data_set_eval[seq((i - 1) * 6 + 1, 
                                                     i * 6), ]))
}

length(list_mat_porders_ml) # 202
length(unique(list_mat_porders_ml)) # 170
Reduce("|", list_mat_porders_ml)
Reduce("&", list_mat_porders_ml)
Reduce("+", list_mat_porders_ml)

# Formal context given by the partial orders in list_mat
fc_ml_porder <- ddandrda::compute_conceptual_scaling(input_porder = list_mat_porders_ml)

# VC dimension is upper bound
# see package oofos on github.com/schollmeyer 
ml_porder_model <- oofos::compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model, list(outputflag = 0))

vc_fc_ml_porder$objval # Vc dimension is

################################################################################
# Descriptive Analysis
################################################################################

# Compute the ufg
# ufg_depth <- ddandrda::compute_ufg_depth(list_mat_porders_ml,
#                                          list_mat_porders_ml,
#                                          max_card_ufg = 3)



# descriptive part


