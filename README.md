# reg_tree

Weird implementation of a regression tree for identification of convergence clubs (see Postiglione, Benedetti & Lafratta 2010).

**Files**

- `_demo.R` applies this approach to a simulated dataset and to the dataset discussed in and prepared by `data_prep.R` (for illustratory purposes).
- `data_prep.R` contains code to prepare the data (not included) from the Cambridge Econometrics European Regional Database.
- `tree_core.R` holds the main functionality and some helper functions. The main wrapper is `get_nodes()`, whilst `find_split()` documents the process best.
- `tree_aux.R` contains a variety of helper functions.

## Literature

Postiglione, P., Benedetti, R., & Lafratta, G. (2010). A regression tree algorithm for the identification of convergence clubs. Computational Statistics & Data Analysis, 54(11), 2776-2785.
