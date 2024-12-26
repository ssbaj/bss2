find_col <- function(dataset_name, ...) {

if (base::missing(dataset_name)) {
cat(" \033[1;36m# Examples ---------- \033[0m", '\n' )
return( cat("\033[1;36m COMMAND: find_col(df, v1, v2) \033[0m", '\n') )
}

dataset_name <- deparse(substitute(dataset_name))
dataset <- get(dataset_name)

var_names <- as.character(substitute(list(...)))[-1]

column_indices <- match(var_names, names(dataset))

return(setNames(column_indices, var_names))
}

