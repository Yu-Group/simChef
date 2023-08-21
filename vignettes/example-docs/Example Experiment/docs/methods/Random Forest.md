Given some training covariate data $\mathbf{X}$ and response vector $\mathbf{y}$, we fit a random forest on $\mathbf{X}$ to predict $\mathbf{y}$. At a high-level, a random forest is an ensemble of classification or regression trees (CART) that are fitted independently of one another on bootstrapped samples of the training data. Further, each CART model is fitted by performing recursive axis-aligned binary splits, where the optimal split at each node is chosen from a random subsample of features to minimize an impurity decrease criterion. 

To make predictions, CART identifies the unique leaf node containing the test data point and predicts the mean response (of training data) in that node. For a random forest, the predictions are averaged across all CARTs in the forest.

For further details, we refer to [Breiman (2001)](https://link.springer.com/content/pdf/10.1023/A:1010933404324.pdf).

<span style="color: blue">
	[In practice, documentation of methods should answer the questions “what” and “why”. That is, “what” is the method, and “why” are we using/studying it? As this simulation experiment is a contrived example, we omit the “why” here.]
</span>
