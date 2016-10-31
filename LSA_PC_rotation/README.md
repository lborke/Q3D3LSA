
[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **LSA_PC_rotation** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet : LSA_PC_rotation

Published in : Q3-D3-LSA

Description : 'The function "LSA.PC.rotation" determines the proper sign / rotation of the LSA
components (semantic space principal components) and extracts the top words (top_terms) of each
component (PC) for the given LSA space which is created from the term document matrix TDM of the
Quantlets by means of the SVD process. The (positive and accordingly negative) part is chosen from
those terms, where the biggest subtotal is concentrated since the singular vectors are unique up to
scalar multiples of modulus one, in the case of real matrices +/- 1. Finally, the top words with
the highest weights are taken as "prototypes" for the semantic space principal components’ topics
(PC topics). That way the function "LSA.PC.rotation" allows the determination of possible labels
for the LSA PC’s. The function requires as input the LSA space which is provided via the function
"lsa" (from the R package lsa). The remaining parameters of "LSA.PC.rotation" are optional allowing
finer control of the output (number of PC’s, terms/topic etc.)'

Keywords : 'data mining, text mining, term document matrix, quantnet, lsa, svd,
descriptive-statistics, singular values, semantic space principal components, singular vectors,
topic'

See also : LSA_basics, LSA_kernel, LSA_basics_hist_box, LSA_heatmaps_factors, LSA_heatmaps_sum

Author : Lukas Borke

Submitted : 31.10.2016 by Lukas Borke

Output : 'Returns a list with PC_labels (PC topics: named vectors) and SV_weights (vector of
singular values)'

```


### R Code:
```r

# Clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# Install and load packages
libraries = c("lsa")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


(obj.names = load("TDM_Q3D3LSA.RData", .GlobalEnv))


# LSA space creation

system.time( LSA_space <- lsa(m_a) )
summary(LSA_space)

------------------------------------------------
# LSA Extended / Interpretation
------------------------------------------------

LSA.PC.rotation = function(LSA_space, auto_rotation = TRUE, n = 10, top_terms = 5, weight_round = 2){
	pc_labels = c()
	sv_weights = c()
	
	U   = LSA_space$tk
	sv  = LSA_space$sk

	for (i in 1:n) {
		PC = U[,i]
		
		if (auto_rotation) {
			if (sum(PC[PC > 0]) + sum(PC[PC < 0]) >= 0) {
				res = PC[PC > 0]
			} else {
				res = -PC[PC < 0]
			}
		} else {
			res = PC[PC > 0]
		}
		
		res = sort(res, decreasing = TRUE)[1:top_terms] * sv[i]
		res = round(res, weight_round)
		
		SV_weight = round(sv[i], weight_round)
		
		pc_labels = c(pc_labels, list(res))
		sv_weights = c(sv_weights, SV_weight)
	}

	#return(pc_labels)
	return(list(PC_labels = pc_labels, SV_weights = sv_weights))
}


LSA_PC = LSA.PC.rotation(LSA_space)

LSA_PC = LSA.PC.rotation(LSA_space, auto_rotation = F)


str(LSA_PC)

LSA_PC$PC_labels
LSA_PC$SV_weights

```
