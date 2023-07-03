# simChef 0.0.3

* When creating simulation R Markdown, we now output full-sized figures by
  default, rather than thumbnails (https://github.com/Yu-Group/simChef/pull/125).
* Renamed various docs-related functions; old names are deprecated but not removed:
  - `create_doc_template()` is now `init_docs()`.
  - `create_rmd()` is now `render_docs()`.
  - `set_rmd_options()` is now `set_doc_options()`.
* `vary_across` can now handle multiple `DGP`/`Method` objects at once
  (https://github.com/Yu-Group/simChef/pull/126).
* `create_docs()` now returns the `.Rmd` file, allowing users to directly edit
  the simulation docs by hand, and allows for more bare-bones docs outputs
  (https://github.com/Yu-Group/simChef/pull/139).
* Added the "Setting up your simulation" vignette with recommendations for
  creating simulation projects that use `simChef`
  (https://github.com/Yu-Group/simChef/pull/152).
* Added a helper function `create_sim()` for initializing a simulation project and
  `run_tests()` for running `testthat` tests of a user's custom simulation
  functions (https://github.com/Yu-Group/simChef/pull/155).
* Made quality of life improvements to the library of `Evaluator`/`Vizualizer`
  helper functions (https://github.com/Yu-Group/simChef/pull/158).
* The default `Visualizer` plotting theme is now `ggplot`, which can now be
  changed globally using the `simChef.plot_theme` option, e.g., to get the
  previous default use `options(simChef.plot_theme = "vthemes")`
  (https://github.com/Yu-Group/simChef/pull/161).
* Bug fixes and CI updates:
  * https://github.com/Yu-Group/simChef/pull/129
  * https://github.com/Yu-Group/simChef/pull/134
