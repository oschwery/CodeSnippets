# CodeSnippets
Various useful bits of code to perform small tasks that might be useful for others as well.

- `AddRandomClade`: Simulate missing diversity based on the [`add.random`](http://blog.phytools.org/2013/01/adding-new-tips-at-random-to-phylogeny.html) code by Liam Revell. Function to randomly add tips to a phylogeny based on different sampling levels per clade. A simplistic approximation of how missing diversity could be distributed, _e.g._ for sensitivity analyses.

- `stateplot_standalone`: Function to plot the inferred/reconstructed rate categories from the MiSSE model, as impemented in teh [`hisse`](https://cran.r-project.org/web/packages/hisse/index.html) package. This allows to visualize the location of the rate categories on the tree, rather than just the inferred rates, which can be useful to spot unexpected behaviour or tips or nodes with highly ambivalent/uncertain category reconstructions.

