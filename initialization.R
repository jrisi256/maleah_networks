library(purrr)
set.seed(420)

# nr_a, number of rows, number of people in the simulation
# nr_quals, number of features of a person
# nr_pc, number of personal characteristics
# nr_foci_membership, number of foci an individual can be a part of
# total number of columns = nr_quals + nr_foci_membership
InitializeIndividuals <- function(nr_a, nr_quals, nr_pc, nr_foci_membership) {
    matrix1 <- matrix(0, nrow = nr_a, ncol = nr_quals + nr_foci_membership)
    
    # number of personal characteristics, uniform
    matrix1[, 1:nr_pc] <- floor(runif(nr_pc * nr_a, min = 0, max = nr_pc + 1))
    
    # gender, uniform
    matrix1[, nr_pc + 1] <- floor(runif(nr_a, min = 0, max = 2))
    
    # age, beta
    matrix1[, nr_pc + 2] <- floor(rbeta(nr_a, 1.75, 3) * 10)
    
    # location, standard normal
    matrix1[, (nr_pc + 3):(nr_pc + 5)] <- rnorm(3 * nr_a)
    return(matrix1)
}

# nr_foci, number of foci, number of rows
# nr_slots, number of chairs or slots in the focus, number of columns
InitializeFoci <- function(nr_foci, nr_slots) {
    matrix1 <- matrix(1, nrow = nr_foci, ncol = nr_slots)
}

JoinFocus <- function(row_indices,
                      col_indices,
                      individual_matrix,
                      focus_matrix) {
    apply(focus_matrix, 1, function(x) {sum(x)})
}

person_matrix <- InitializeIndividuals(1000, 15, 10, 5)
foci_matrix <- InitializeFoci(50, 10)

temp_matrix <- JoinFocus(nrow(person_matrix),
                         rep(16, nrow(person_matrix)),
                         person_matrix,
                         foci_matrix)
