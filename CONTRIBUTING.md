# Contributing guidelines for FERMENT

Contributions are always welcome! Please read these guidelines before opening an issue or submitting a pull request.

## Code of conduct

This project follows standard open-source community norms. Please visit our [Code of Conduct](CODE_OF_CONDUCT.md) for more information.

## Reporting issues

Open an issue on GitHub with a clear description of the bug or feature request. Include steps to reproduce, expected vs. actual behaviour, and your R/package versions where relevant.

## Privacy and data policy

> **This is the most important rule in this repository. Please read it carefully.**

The data needed to run the app is not included in this repository and is only available internally by request. This is to protect sensitive experimental data and ensure compliance with our data handling policies. If granted access, remember:

> **Never commit real experimental data to this repository!**

This means:
- No files containing actual fermentation measurements.
- No files containing strain names, batch numbers, or any other identifiers linked to real experiments.
- No screenshots, exports, or derivatives of real experimental data.

Your data folder (the one pointed to by `EXCEL_DIR`; see README -> Installation -> step 4) should **never** be committed inside the repository.

This project's data folder is located within a "TU Delft Project Data Storage" ([click here](https://tu-delft-dcc.github.io/docs/data/data_storage/storage_options.html#overview-of-storage-options) for an overview of storage options, and [click here](https://tu-delft-dcc.github.io/docs/data/data_storage/project_drive_request.html#accessing-the-project-data-storage-u-drive) for access tips). Access is subjected to approval by the project leader (Jean-Marc Daran: 
J.G.Daran[at]tudelft.nl) and granted by the project maintainer (Marcel van den Broek: 
Marcel.vandenBroek[at]tudelft.nl).

## Branching rules

This repository uses the following branch structure:

- **`main`** - the stable, production-ready branch. Do not commit directly to `main`.
- **`develop`** - the active development branch. All feature branches are merged here first. This is the branch your pull requests should target.
- **Feature/fix branches** - short-lived branches created off `develop` for individual changes.

The typical flow is:

```
your-feature-branch → develop → main
```

## Submitting changes

1. Make changes in a new branch off `develop` (e.g. `fix-typo`, `add-feature-x`)
2. Make your changes and test them locally
3. Commit with a clear message describing what changed and why
4. Push your branch and open a pull request against `develop`

## Development setup

Clone the repo and restore the environment using `renv`:

```r
renv::restore()
```

Set the `EXCEL_DIR` environment variable to point to a folder containing the data. See the [README](README.md) for details on how to set this up.

Then launch the app locally:

```r
shiny::runApp()
```

You can make changes to the code and relaunch the app to see the results. This allows you to test and iterate quickly on your changes.

## Updating dependencies with `renv`

If your changes require adding or updating an R package:

1. Install the package as normal inside the project:
   ```r
   install.packages("package-name")
   ```
2. Snapshot the updated state of the environment:
   ```r
   renv::snapshot()
   ```
3. Commit the updated `renv.lock` file alongside your code changes. This ensures other contributors and machines can reproduce your exact environment with `renv::restore()`.

Do not commit `renv.lock` changes without also committing the code that requires them, and do not update `renv.lock` without running `renv::snapshot()`. Manual edits to the lockfile will likely break the environment for others.

## Code style

Please follow the existing code style and keep UI and server logic cleanly separated. Comments are appreciated for non-obvious logic.

## Questions?

Feel free to open a discussion/reach out via the issue tracker.
