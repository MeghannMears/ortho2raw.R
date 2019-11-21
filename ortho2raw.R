# converts model coefficients from models with orthogonal polynomials 
# into their raw values

# inputs: the model using orthogonal polynomials and the data frame used
# as input to that model

# output: a data frame containing the model variables, polynomial degree,
# gamma (transformation matrix), and orthogonal and raw coefficients

# https://stats.stackexchange.com/questions/31858/recovering-raw-coefficients-and-variances-from-orthogonal-polynomial-regression
# was essential in creating this

ortho2raw <- function(input_model, vars) {

	library(stringr)
#	browser()

	# if statement to extract the model coefficients, as averaged models 
	# have a different summary$coefficients to single models)
	if (colnames(summary(input_model)$coefficients)[1] == "Estimate") { 
		model_coefs <- t(summary(input_model)$coefficients)[1,]
	} else {
		model_coefs <- summary(input_model)$coefficients[1,]
	}

	# get names of coefficients (without poly() garble) and reduce
	# input data frame to only necessary columns
	coef_names<- names(model_coefs)
	coef_names <- unique(gsub("poly\\(", "", gsub(", 2\\)\\[, .\\]", "", coef_names)))
	vars<-vars[,which(names(vars) %in% coef_names)]

	# initialise data frame for results
	results<-data.frame(vars=names(vars), poly=0, gammas=0, orthogonal=0, raw=0)

	# count degree of polynomial used in model for each variable
	for (i in 1:dim(results)[1]) {
		results$poly[i]<-sum(str_count(names(model_coefs), sprintf("poly\\(%s", toString(results$vars[i]))))
	}

	# calculate transformation coefficients (gammas)
	# loop through variable names
	for (i in 1:dim(vars)[2]) {
		# get polynomial degree of variable
		d <- results$poly[which(results$vars==names(vars)[i])]
		# only do if there is a polynomial
		if (d > 0) {
			x <- vars[,i]
			x.p <- outer(x, 0:d, `^`); colnames(x.p) <- c("Intercept", paste0("x.", 1:d)) # Non-orthogonal polynomials matrix
			z <- poly(x, d) # Orthogonal polynomials matrix
			xform <- lm(cbind(1, z) ~ x.p-1) # Regress the non-orthogonal and orthogonal matrices
			gamma <- coef(xform) # Extract the coefficients i.e. the multipliers for transformation
			results$gammas[which(results$vars==names(vars)[i])] <- list(gamma)
		}
	}

	# transform coefficients
	# loop through variables
	for (j in 1:dim(results)[1]) {
		# initialise variable to hold orthogonal coefs and set the first one
		# to the intercept (as this is the same for all variables)
		ortho<-vector(length=results$poly[j]+1)
		ortho[1]<- summary(input_model)$coefficients[1,1]		
		# only do if there is a polynomial
		if (results$poly[j] > 0 ) {
			# get the orthogonal polynomial coefficients
			for (i in 1:results$poly[j]) {
				ortho[i+1] <- model_coefs[which(names(model_coefs)==sprintf("poly(%s, 2)[, %i]", names(vars)[j], i))]		
			}
			# add orthogonal coefficients to results table
			results$orthogonal[j]<-list(ortho)
			# perform matrix multiplication to get the transformed coefficients 
			results$raw[j]<- list(as.vector(results$gammas[j][[1]] %*% unlist(results$orthogonal[j])))
		}
	}
	return(results)
}
