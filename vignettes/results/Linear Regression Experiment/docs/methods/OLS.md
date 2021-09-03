Given some data $\mathbf{X}$ and $\mathbf{y}$, we fit ordinary least squares (OLS) and examine the p-values for each coefficient in the model. The p-values are computed using a two-sided t-test (see `summary.lm()`).

Elaborating further on the testing, we are interested in testing:

\begin{align*}
H_0: \beta_i = 0 \quad vs. \quad H_1: \beta_i \neq 0.
\end{align*}

To test this, we compute the observed T-statistic, defined as

\begin{align*}
T = \frac{\hat{\beta}_i}{\hat{SE}(\hat{\beta_i})},
\end{align*}

and then compute the two-sided p-value under the t distribution with $n - p - 1$ degrees of freedom. If the p-value is lower than some significance value $\alpha$, then there is sufficient evidence to reject the null hypothesis $H_0$. Otherwise, there is not sufficient evidence, and we fail to reject the null hypothesis.

<span style="color: blue">
	[In practice, documentation of methods should answer the questions “what” and “why”. That is, “what” is the method, and “why” are we using/studying it? As this simulation experiment is a contrived example, we omit the “why” here.]
</span>
