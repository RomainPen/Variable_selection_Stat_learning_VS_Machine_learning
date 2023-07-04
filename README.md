# Variable_selection_Stat_learning_VS_Machine_learning

In various contexts, empirical studies are frequently devoted to the analysis of the relationship between some dependent variable and a multitude of explanatory variables
that may influence the dependent variable. Mostly, information on a large number of explanatory variables is gathered and during the data analysis phase these are ’statistically screened’ to find the most important ones for an appropriate description of the
relationship. But, when faced with a large number of variables, how to choose a set of relevant variables ? High dimensional data sets actually disperse data. This has a tendency
to skew traditional data analysis methods. Indeed, when dealing with a large number
of variables, selecting the best subset of predictors that are not redundant, not particularly explanatory, or non-significant information for the model becomes nearly impossible.

In this situation, variable selection techniques are commonly used as part of statistical
modeling of the observed data to limit the number of explanatory variables and to find
the best-fitting subsets of variables. In other words and more precisely, to obtain an
appropriate model that (i) leads to stable parameter estimates, (ii) accurately predicts
future values, and (iii) allows for a comprehensible interpretation because only a few important explanatory variables are chosen [1]. There are numerous approaches for selecting
variables. These methods can be divided into two main categories : Statistical Learning
and Machine Learning. Statistical learning is more concerned with interpretability and
description, whereas Machine Learning is more concerned with prediction.

The purpose of this paper is to describe and test the reliability of different Statistical
Learning and Machine Learning methods. To do so, we will apply each selection process
to our 1000 models of 100 observations each (including 50 explanatory variables with 5
that explain well and 45 that explain poorly), previously built by 4 different Data Generating Processes (DGP). Data Generating Processes follow certain hypotheses such as
whether or not there is a correlation, whether or not there are outliers. This gives us the
opportunity to put up an evaluation procedure. Indeed, we will know which variables
must have been selected and which should not have been when performing variable selection algorithms on each data set. In other words, the purpose of this evaluation technique
is to assess the subset’s performance and relevance. Eventually, thanks to this amount of
knowledge on the data, we will be able to choose the best model for variable selection.

This paper’s overall structure has been divided into six sections : the models, the
method for extracting the results, and the results themselves are divided into two subsections, Statistical Learning and M achine Learning, respectively. The models that
will be used in the paper are described in detail in Section 2. Section 3 is focused on
the methods applied on SAS software to simulate data and extract results. In Section 4,
these results are detailed for each model. In Section 5, we will make an application on
the diabetes data. Finally, Section 6 will conclude with a discussion of the paper’s overall
results.
