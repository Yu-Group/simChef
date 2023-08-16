We simulate the (test and training) covariate/design matrix $\mathbf{X} \in \mathbb{R}^{n \times p}$ from a standard normal distribution and the response vector $\mathbf{y} \in \mathbb{R}^n$ from a linear model. Specifically,

\begin{align*}
\mathbf{y} = \mathbf{X} \boldsymbol{\beta} + \boldsymbol{\epsilon},\\
\end{align*}

where
\begin{align*}
& \mathbf{X}_{ij} \stackrel{iid}{\sim} N\left(0, 1\right) \text{ for all } i = 1, \ldots, n \text{ and } j = 1, \ldots, p, \\
& \boldsymbol{\epsilon}_i \stackrel{iid}{\sim} N(0, \sigma^2) \text{ for all } i = 1, \ldots, n.
\end{align*}

**Default Parameters in DGP**

- Number of training samples: $n_{\text{train}} = 200$
- Number of test samples: $n_{\text{test}} = 200$
- Number of features: $p = 2$
- Amount of noise: $\sigma = 1$
- Coefficients: $\boldsymbol{\beta} = (1, 0)^\top$

<span style="color: blue">
	[In practice, documentation of DGPs should answer the questions “what” and “why”. That is, “what” is the DGP, and “why” are we using/studying it? As this simulation experiment is a contrived example, we omit the “why” here.]
</span>
