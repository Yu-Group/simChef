# pasteMd works properly

    In the Linear Gaussian DGP, we simulate the feature/design matrix $\mathbf{X} \in \mathbb{R}^{n \times p}$ from a normal distribution and the response vector $\mathbf{y} \in \mathbb{R}^n$ from a linear model. Specifically,
    
    \begin{gather*}\mathbf{X} \sim N\left(\mathbf{0}, \begin{pmatrix} 1 & \rho \\ \rho & 1 \end{pmatrix}\right), \\\mathbf{y} = \mathbf{X} \boldsymbol{\beta} + \boldsymbol{\epsilon},\\\boldsymbol{\epsilon} \sim N(\mathbf{0}, \sigma^2 \mathbf{I}_n)\end{gather*}
    
    **Default Parameters in DGP**
    
    
    - Number of samples: $n = 200$
    - Number of features: $p = 2$
    - Correlation among features: $\rho = 0$
    - Amount of noise: $\sigma = 1$
    - Coefficients: $\boldsymbol{\beta} = (1, 0)^\top$
    
    <span style="color: blue">	[In practice, documentation of DGPs should answer the questions “what” and “why”. That is, “what” is the DGP, and “why” are we using/studying it? As this simulation experiment is a contrived example, we omit the “why” here.]</span>

