## Welcome to `simChef`!

`simChef` helps you to cook up the code necessary to create a fully-realized,
high-quality, reproducible, and transparently documented simulation experiment.

## Background

In their 2019 paper "Veridical Data Science", Yu and Kumbier propose the
predictability, computability, and stability (PCS) framework, a workflow and
documentation for "responsible, reliable, reproducible, and transparent results
across the data science life cycle". Under the umbrella of the PCS framework, we
began the process of deriving a set of guidelines tailored specifically for
simulation experiments along with examples of high-quality simulation
experiments from the literature and examples designed specifically to examine
the statistical properties of methods within the PCS framework.

In creating our own experiments, we soon found that no existing R package could
fully satisfy our developing requirements. `simChef` is our attempt to provide
both a testbed and a computational home for the ongoing development of the PCS
simulation guidelines, an intuitive recipe for veridical simulation experiments.
We believe these tools will be useful for anyone intending to create their own
simulation experiments in R.

Let's use the components of a high-quality meal as analogies a high-quality
simulation experiment:

- **Nutritious and delicious ingredients** -- All good meals start with good
  ingredients and the same is true of simulation experiments. If realistic
  simulation data (synthetic or derived from real-world data) is not available,
  then there is no hope of producing high-quality simulations. `simChef` lowers
  the difficulty of creating realistic simulation scenarios and makes
  incorporating real data a breeze.
- **Skill and experience of the chef** -- Just as every chef's cooking is
  informed by the handful of cuisines in which they specialize, simulation
  experiments are motivated by scientific questions from a particular domain.
  `simChef` takes care of the details of running your experiments across the
  potentially large number of data and model perturbations you care about so
  that you can focus on your scientific question.
- **High-quality tools in the kitchen** -- Our package should be like an
  excellent chef's knife or other kitchen essential. If a chef's knife doesn't
  cut straight or isn't sharpened, then kitchen speed and safety suffers, as
  does the final presentation. `simChef` won't cook a good simulation experiment
  for you, but it will get you there with less effort and higher-quality
  presentation while helping you follow best-practice guidelines with minimal
  effort on your part. No sharpening required!
- **A high-quality mean is possible in almost any environment** -- While the
  scale of a delicious meal may be limited by environment, high-quality meals
  are not only found in the world's Michelin-starred restaurants but also in
  home kitchens and street food carts around the world. An effective simulation
  framework should also be agnostic to environment, and `simChef` runs equally
  well on your laptop as on a high-performance computing cluster.
- **Appetizing and approachable presentation** -- Ultimately, a chef prepares
  food for a specific audience and presentation is almost equal in importance to
  the underlying substance of the meal. However, a chef doesn't have to build
  the plate on which they serve their food. `simChef` provides tools to turn
  your simulation experiment results into effective displays of quantitative
  information which are populated within preset and customizable R Markdown
  templates.

## A grammar of simulation experiments

When we designed the API for `simChef`, we placed a strong emphasis on creating
a meaningful API that would enable an intuitive grammar of simulation
experiments. For example, the following creates an experiment, adds
data-generating processes and a method, adds parameters of the data-generating
process and method to vary across, and finally runs the experiment.

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

For more details, run `vignette("simChef")`.

### API Definitions

- **Simulation**: A collection of experiments.
- **Simulation Experiment**: A collection of related data-generating processes, methods, method
  evaluators, and result plotters. An experiment is designed to answer a
  well-defined question.
- **Data-generating process (DGP)**: A DGP generates data (duh!). This could be
  entirely synthetic data, or it could be derived from some real-world dataset
  with a perturbation of some sort (e.g., added noise of various types,
  {re,sub}sampling, transformations, etc.).
- **Method**: Methods are often (but not always!) the target of the scientific
  questions we are trying to answer via the simulation experiments. They take in
  data and return a result of some sort (e.g., statistical estimates,
  predictions, confidence/perturbation intervals,
  significance/interpretability/importance metrics, etc.)
- **Experiment Evaluator**: An experiment evaluator takes the results from many
  independent replicates of the experimental pipeline, i.e. generate some data
  and compute results using all compatible methods for that data, repeating for
  every DGP in the experiment.
- **Visualizer**: A visualizer communicates results from an evaluator in some
  user-friendly way. For example, a visualizer might create tables, plots, or
  even an R Markdown snippet to display within a larger document.

## Roadmap

- [x] Implement an abstract API to allow for a grammar of simulation
      experiments.
- [x] Run simulation experiments in parallel and agnostic to computational
      backend via the R package `future`.
- [x] Output an R Markdown report summarizing the results of an Experiment.
- [x] Allow for varying simulation experiments across arbitrary parameters of
      DGPs and Methods.
- [x] Give user the ability to choose which tasks are distributed to parallel
      workers, i.e. simulation replicates, DGPs, Methods, or combinations of the
      three.
- [ ] Cache results to avoid re-running already computed components of an
      Experiment.
- [ ] Gracefully handle errors from user-defined functions and returns error
      information for user to inspect upon completion.
- [ ] Checkpoint simulations to avoid losing progress in the case of unexpected
      problems, e.g. node failure.
- [ ] Include a set of off-the-shelf DGPs and Methods to allow users to quickly
      run their methods in a number of realistic scenarios compared against a
      number of high-quality methodologies for those scenarios.
- [ ] Include a set of off-the-shelf Visualizers that create plots, tables, and
      other snippets that can be included in the output R Markdown report.
- [ ] Provide settings to effectively organize Visualizers in the final R
      Markdown report.
- [ ] Enable nested parallelization, e.g. one may paralellize across DGPs using
      multiple nodes on a cluster and parallelize across simulation replicates
      using the CPU cores within each node.
- [ ] Initialize a new simulation experiment interactively via a helper
      function, incorporating best practice nudges derived from the PCS
      simulation guidelines.
- [ ] Let user incorporate their own R Markdown report template.

## Related R packages

There are a number of existing tools in R to run reproducible simulation
experiments. [TODO: examples and the functionality they provide]
