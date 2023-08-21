Given some training covariate data $\mathbf{X}$ and response vector $\mathbf{y}$, we fit a linear regression model (i.e., ordinary least squares) on $\mathbf{X}$ to predict $\mathbf{y}$ by minimizing the following objective:

\begin{align*}
\boldsymbol{\hat{\beta}} = \text{argmin}_{\boldsymbol{\beta}} || \mathbf{y} - \mathbf{X} \boldsymbol{\beta} ||_2^2.
\end{align*}

Then, to make prediction given some test data $\mathbf{X}^{\text{test}}$, we compute:

\begin{align*}
\boldsymbol{\hat{y}}^{\text{test}} = \mathbf{X}^{\text{test}} \boldsymbol{\hat{\beta}}.
\end{align*}

<span style="color: blue">
	[In practice, documentation of methods should answer the questions “what” and “why”. That is, “what” is the method, and “why” are we using/studying it? As this simulation experiment is a contrived example, we omit the “why” here.]
</span>
