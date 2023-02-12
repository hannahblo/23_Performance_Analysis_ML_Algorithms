# R-Code for Theorem 7 of the paper that states:

# D_n can not be represented as a function of the
# sum-statistics 𝑤_(𝑎,𝑏).

# All partial orders as a 8complemented) formal context
porder_all <- ddandrda::compute_all_partial_orders(3, list = FALSE, complemented = TRUE)

# All partial orders as a list
list_porder_all <- ddandrda::compute_all_partial_orders(3, list = TRUE, complemented = FALSE)

# Data set 1
data_context_1 <- porder_all[c(6, 7, 4), ]
# All partial orders without data set 1
porder_rest_1 <- porder_all[-c(6, 7, 4), ]
list_porder_all[c(6, 7, 4)]

# Data set 2
data_context_2 <- porder_all[c(6, 3, 8), ]
# All partial orders without data set 2
porder_rest_2 <- porder_all[-c(6, 3, 8), ]

list_porder_all[c(6, 7, 4)]


# relevant ufg-premises for data set 1
ufgs_1 <- oofos::enumerate_ufg_premises(whole_context = rbind(
  data_context_1, porder_rest_1
), n_row_context = 3)

# relevant ufg-premises for data set 1
ufgs_2 <- oofos::enumerate_ufg_premises(whole_context = rbind(
  data_context_2, porder_rest_2
), n_row_context = 3)

ufgs_1

# [[1]]
# [1] 1 3
#
# [[2]]
# [1] 2 3

# {p_1,p_3} imply {p_1}
# but {p_2,p_3} does not imply {p_1} because (y_1,y_3) \in p_2 and
# (y_1,y_3) \in p_2 but (y_1,y_3) \notin p_1
# (p_1,..p_3) refer here to p_1,.., p_3 in the paper

# therefore, the ufg-depth of p_1 is (1/3)^2 /[ (1/3)^2 +(1/3)^2 ] = 1/2



ufgs_2

# [[1]]
# [1] 1 2
#
# [[2]]
# [1] 1 2 3
#
# [[3]]
# [1] 1 3
#
# [[4]]
# [1] 2 3

# {p_1,p_2} imply   {p_1}, {p-1,p_2,p_3} imply {p_1}, {p_1,p_3} imply {p_1}
# but {p_2,p_3} do not imply {p_1} because (y_1,y_3) \in p_2 and
# (y_1,y_3) \in p_2 but (y_1,y_3) \notin p_1
# (p_1,..p_3) refer here to \tilde{p_1},.., \tilde{p_3} in the paper

# Terefore the depth of p_1 is (2*(1/3)^2+(1/3)^3) / (3*(1/3)^2+(1/3)^3) =0.7

# This means that the depth functions associated with data set 1 and dat set 2
# are different.
# On the other hand, both data sets have the same sum statistics:

sum_statistics_1 <- Reduce("+", list_porder_all[c(6, 7, 4)])
sum_statistics_2 <- Reduce("+", list_porder_all[c(6, 3, 8)])

all(sum_statistics_1 == sum_statistics_2)
