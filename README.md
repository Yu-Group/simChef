# simChef

## Overview

The goal of `simChef` is to help you quickly cook up a fully-realized,
high-quality, reproducible, and transparently documented simulation study using
an intuitive and tidy grammar of simulation experiments:

```r
experiment <- create_experiment() %>%
  add_dgp(dgp1) %>%
  add_dgp(dgp2) %>%
  add_method(method1) %>%
  add_vary_across(
    dgp = dgp1,
    n = c(100, 1000, 10000)
  ) %>%
  add_vary_across(
    method = method1,
    lambda = c(0.1, 0.5, 1.0)
  )

results <- experiment %>%
  run_experiment()
```

### Installation

`simChef` is under active development. To install the package directly from
GitHub, please use:

```r
devtools::install_github("Yu-Group/simChef")
```

### Concepts

In `simChef`, simulation studies are decomposed into **five** intuitive
concepts: experiments, data-generating processes, methods, evaluations, and
visualizations. A simulation can either be contained in a single **experiment**
or divided into multiple self-contained experiments which are like small
simulations studies in their own right. Every experiment is in turn composed of
four parts, two of which are optional (but highly recommended):
**data-generating processes** (DGPs), **methods**, **evaluation** (optional),
and **visualization** (optional).

`simChef` takes an object-oriented approach to encapsulate these simulation
concepts, using [`R6`](https://r6.r-lib.org/index.html) classes to make them
concrete. The five main objects are:

- `Experiment`: corresponds to the **experiment** concept. As you can probably
  guess, this class is the main powerhouse of the simulation, collecting related
  DGPs and methods, keeping track of what parameters to vary, checkpointing and
  saving results, and producing evaluations metrics, visualizations, and
  documentation so that the simulation's findings can be understood and easily
  communicated. Moreover, it uses [`future`](https://future.futureverse.org/) to
  compute experimental replicates in parallel using whatever resources you
  choose.
- `DGP`: corresponds to the **data-generating process** concept. DGPs simply
  generate synthetic data in a reproducible and flexible manner, in the size and
  manner that you specify. For a library of preset but highly customizable DGPs,
  including support for data-driven DGPs to give added realism to your synthetic
  data, `simChef` has a sibling R package,
  [`dgpoix`](https://yu-group.github.io/dgpoix) (currently in early
  development).
- `Method`: corresponds to the **method** concept, which can be either a
  baseline, a target of the simulation study, or any means by which to transform
  the raw synthetic data. Together with DGPs, methods make up the main
  computational course of the `simChef` meal.
- `Evaluator`: corresponds to the **evaluation** concept. When computation of
  experimental replicates has completed, evaluators receive the results and
  summarize them to produce meaningful statistics about the experiment, or
  simply transform the results (e.g., using summary statistics). This is an
  optional step, but without it the experiment's results can be much more
  difficult to understand and communicate.
- `Visualizer`: corresponds to the **visualization concept**. These
  visualizations can be applied directly to the raw experimental replicates'
  outputs, can instead work with the evaluation transformations/summaries, or
  both. Visualizers can output anything that can be rendered in an R Markdown
  document: static or interactive plots, tables, strings and captured output,
  markdown, generic HTML, etc.
  
### Simulation study documentation

When all of this is put together, the `Experiment` class can output an R
Markdown document that is structured to provide a well-organized summary of a
simulation study. Moreover, this document can contain multiple experiments,
simply by using a common output path with each `Experiment` in the study. When
the simulation is complete, you can use the `render_docs()` helper to generate
the documentation:

```r
render_docs(experiment)
```

This results in an HTML document like the one shown below:

![Interactive R Markdown simulation documentation](man/figures/simchef.gif)

For more examples, including an interactive version of the simulation study
documentation, see `vignette("simChef")`.

## Origins of `simChef`

In their 2020 paper "Veridical Data Science", Yu and Kumbier propose the
predictability, computability, and stability (PCS) framework, a workflow and
documentation for "responsible, reliable, reproducible, and transparent results
across the data science life cycle". Under the umbrella of the PCS framework, we
began the process of deriving a set of guidelines tailored specifically for
simulation studies, inspired by both high-quality simulation studies from the
literature and our own simulations to examine the statistical properties of
methods within the PCS framework.

While creating our own simulations, we soon found that no existing R package
could fully satisfy our developing requirements. What began as a toolbox for our
own simulations became `simChef`. We believe these tools will be useful for
anyone intending to create their own simulation studies in R.

### Thinking like a chef

The development of `simChef` has been guided by our love of... cooking? Perhaps
surprisingly, we found that cooking serves as useful analogy for the process of
creating a simulation study. Consider the following components of a high-quality
meal:

- **Nutritious and delicious ingredients** -- All good meals start with good
  ingredients, and the same is true of simulation experiments. If realistic
  simulation data (entirely synthetic or driven by real-world data) is not
  available, then there is no hope of producing high-quality simulations.
  Creating realistic synthetic data is the primary goal of our sibling package
  [`dgpoix`](https://yu-group.github.io/dgpoix/), which was initially integrated
  into `simChef`.
- **Skill and experience of the chef** -- Just as every chef's cooking is
  informed by the handful of cuisines in which they specialize, simulation
  experiments are motivated by scientific questions from a particular domain.
  Just as a chef does not have to become an expert knifemaker before cooking
  their first meal, nor should the domain scientist have to waste time writing
  boilerplate code to for the computation and documentation of their
  simulations. `simChef` takes care of the details of running your experiments
  across the potentially large number of data and method perturbations you care
  about, freeing up time for you to focus on your scientific question.
- **High-quality tools in the kitchen** -- Our package should be like an
  excellent chef's knife or other kitchen essential. If a chef's knife doesn't
  cut straight or isn't sharpened, then kitchen speed and safety suffers, as
  does the final presentation. `simChef` won't cook a good simulation experiment
  for you, but it will get you there with less effort and higher-quality
  presentation while helping you follow best-practices like reproducibility with
  minimal effort on your part. No sharpening required!
- **A high-quality meal is possible in almost any environment** -- While the
  scale of a delicious meal may be limited by environment, high-quality meals
  are not only found in the world's Michelin-starred restaurants but also in
  home kitchens and street food carts around the world. An effective simulation
  framework should also be agnostic to environment, and `simChef` runs equally
  well on your laptop as on a high-performance computing cluster.
- **Appetizing and approachable presentation** -- Ultimately, a chef prepares
  food for a specific audience, and presentation is almost equal in importance to
  the underlying substance of the meal. However, a chef doesn't have to build
  the plate on which they serve their food. `simChef` provides tools to turn
  your simulation experiment results into effective displays of quantitative
  information which are populated within preset and customizable R Markdown
  templates.

## Roadmap

- [x] Implement an abstract API to allow for a grammar of simulation
      experiments.
- [x] Run experimental replicates in parallel and agnostic to computational
      backend via the R package `future`.
- [x] Output an automated R Markdown report summarizing the results of an Experiment.
- [x] Allow for varying simulation experiments across arbitrary parameters of
      DGPs and Methods.
- [x] Cache results to avoid re-running already computed components of an
      Experiment.
- [x] Checkpoint simulations to avoid losing progress in the case of unexpected
      problems, e.g. node failure.
- [x] Gracefully handle errors from user-defined functions and return partial
      results error information for user to inspect upon completion.
- [x] Incorporate `progressr` for simulation progress updates.
- [x] Include a set of off-the-shelf DGPs (moved to
      [`dgpoix`](https://yu-group.github.io/dgpoix/)), Evaluators, and
      Visualizers to allow users to quickly run their methods in a number of
      common types of simulations.
- [x] Allow for user customization of the final R Markdown report (e.g. a customized
      R Markdown template/theme, order of Evaluator and Visualizer displays).
- [ ] Give user the ability to choose which tasks are distributed to parallel
      workers, i.e. simulation replicates, DGPs, Methods, or combinations of the
      three.
- [ ] Enable nested parallelization, e.g. one may paralellize across DGPs using
      multiple nodes on a cluster and parallelize across simulation replicates
      using the CPU cores within each node.
- [ ] Publish to CRAN.

## Related R packages

Below, we examine the main functionality of a number of existing tools for
running reproducible simulation experiments that are currently available on
[CRAN](https://cran.r-project.org/) and have been updated within the last couple
of years.

- [`batchtools`](https://mllg.github.io/batchtools/index.html) implements
  abstractions for "problems" (similar to our DGP concept), "algorithms" (Method
  in `simChef`), and "experiments". In addition to shared-memory computation via
  the `parallel` and `snow` packages, it also provides a number of utilities for
  working with high performance computing batch systems such as Slurm and
  Torque, which `simChef` supports via the `future.batchtools` package.
- [`SimDesign`](https://cran.r-project.org/web/packages/SimDesign/index.html)
  provides helper functions to define experimental conditions and then pass
  those experimental conditions to a user-defined data generation function,
  analysis function, and summary function. The package also provides a number of
  these functions for the user to choose from. Each experimental condition can
  be run over many replicates, computing results in parallel via the `parallel`
  package.
- [`simhelpers`](https://meghapsimatrix.github.io/simhelpers/index.html) defines
  functions to calculate Monte Carlo standard errors of simulation performance
  metrics, generate skeleton simulation code, and evaluate in parallel across
  simulation parameters via the `future` package.
- The [`simTool`](http://marselscheer.github.io/simTool/index.html) package has
  two main functions: `expand_tibble()` and `eval_tibble()`. The former wraps
  the base R function `expand.grid()` to create a cartesian product of
  simulation functions and parameters, while the latter evaluates those
  functions in parallel via the `parallel` package.
- The [`parSim`](https://github.com/SachaEpskamp/parSim) package implements a
  single function of the same name which allows for parallelization of arbitrary
  R expressions across replicates and simulation conditions. `parSim` uses the
  `snow` package to setup parallel backends.
- [`rsimsum`](https://ellessenne.github.io/rsimsum/index.html) is an R
  implementation of the Stata command `simsum` and provides helper functions for
  summarizing and visualizing the results of a simulation study.
