# NFLT journal repository

This repository contains binary class, multi-class, and regression datasets alongside the R scripts to show empirically that the no free lunch theorem of statistical machine learning is indeed valid for every learning problem.

# Implementation

For the purpose of showing tangible practical evidence that the NFLT is indeed valid, we trained fifteen (15) different models chosen from linear and/or non-linear, parametric and/or non-parametric on different binary class, multi-class, and regression datasets by using 80% of data and the remaining 20% for model performance. It was seen from the evaluation (misclassification rate) of the models using the test set that the performance of each learning model is different for various datasets that were involved.


# Abstract

Throughout the relatively young yet tremendously fascinating history of statistical machine learning, data science and artificial intelligence, there has constantly been a fascination and interest in the possibility of creating/finding the holy grail in the form of a learning machine and hypothesis/function space that uniformly outperforms all other machines on all possible datasets. The so-called no free lunch theorem (NFLT) of which many different formulations and incarnations exist, is an intriguing and sometimes controversial result, that establishes that such a holy grail does not exist, namely that no learning machine exists that outperforms all possible learning machines on all datasets.
In this paper, we provided a comprehensive empirical demonstration of the NFLT by comparing the predictive performances of a wide variety of machine learning algorithms/methods on a wide variety of qualitative and quantitative different datasets. Our research work conclusively demonstrates great evidence in favor of the NFLT by using the overall ranking of methods and their corresponding learning machines. It is noteworthy however that while evidence from various datasets and methods support the NFLT emphatically, some learning machines like Random Forest, Adaptive Boosting and Support Vector machine appear to emerge as methods with the overall tendency to yield predictive performances almost always among the best.
From the point of a data science practitioner, our work does implicitly insinuate that in predictive analytics, the methods that emerge as often predictive among the best should be seriously considered even though we do not suggest ignoring or even neglecting the other techniques.
**Keywords**: Data science, Statistical Learning, Prediction, Predictive Performance, No Free Lunch Theorem (NFLT), Learning Machine, Estimation, Dataset, Function space
