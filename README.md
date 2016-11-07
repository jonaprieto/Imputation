# Dealing with Missing Data using a Selection Hybrid Algorithm

This is the repository to expose a *hybrid* algorithm for missing values imputation
in categorical data, **HSI algorithm**. The algorithm is based on a selective
criteria that finds a model to run a classification task in order to perform the
imputation of missing values. This algorithm uses ideas from *Rough Set Theory*
like some criteria used in *ROUSTIDA* algorithm, *VTRIDA* algorithm and variants.

HSI algorithm can be seen as a template where there are improvements and
customization for other scenarios that the user can implement.
Experimentation shows better accuracy of missing values imputation tests using
the algorithm on small real data set from the *UCI Database repository*.
