Given the true responses $\mathbf{y} \in \mathbb{R}^n$ and predicted responses $\mathbf{\hat{y}} \in \mathbb{R}^n$ from various methods, we evaluate several prediction accuracy metrics, namely:

- Root Mean Squared Error (RMSE): $\sqrt{\frac{1}{n} || \mathbf{y} - \mathbf{\hat{y}} ||_2^2}$
- Mean Absolute Error (MAE): $\frac{1}{n} || \mathbf{y} - \mathbf{\hat{y}} ||_1$
- R-squared ($R^2$): $1 - \frac{|| \mathbf{y} - \mathbf{\hat{y}} ||_2^2}{|| \mathbf{y} - \mathbf{\bar{y}} ||_2^2}$

We choose to evaluate both RMSE and MAE as these can convey different messages in the presence of outliers. Further, $R^2$ provides a convenient normalization of RMSE that can often be more easily interpreted.

<span style="color: blue">
	[In practice, documentation of evaluation metrics should answer the questions “what” and “why”. That is, “what” is the metric, and “why” are we using/studying it?]
</span>
