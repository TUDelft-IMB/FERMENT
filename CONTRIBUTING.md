# Contributing guidelines for FERMENT

Contributions are always welcome! Please read these guidelines before opening an issue or submitting a pull request.

## Code of conduct

This project follows standard open-source community norms. Please visit our [Code of Conduct](CODE_OF_CONDUCT.md) for more information.

## Reporting issues

Open an issue on GitHub with a clear description of the bug or feature request. Include steps to reproduce, expected vs. actual behavior, and your R/package versions where relevant.

## Privacy and data policy

> **This is the most important rule in this repository. Please read it carefully.**

**Never commit real experimental data to this repository!**

This means:
- No files containing actual fermentation measurements.
- No files containing strain names, batch numbers, or any other identifiers linked to real experiments.
- No screenshots, exports, or derivatives of real experimental data.

Your data folder (the one pointed to by `EXCEL_DIR`) should **never** be commited inside the repository.

## Branching rules

This repository uses the following branch structure:

- **`main`** — the stable, production-ready branch. Do not commit directly to `main`.
- **`develop`** — the active development branch. All feature branches are merged here first. This is the branch your pull requests should target.
- **Feature/fix branches** — short-lived branches created off `develop` for individual changes.

The typical flow is:

```
your-feature-branch → develop → main
```

## Submitting changes

1. Make changes in a new branch off `main` (e.g. `fix-typo`, `add-feature-x`)
2. Make your changes and test them locally by running the app (`shiny::runApp()`)
3. Commit with a clear message describing what changed and why
4. Push your branch and open a pull request against `develop`

## Development setup

Clone the repo and restore the environment using `renv`:

```r
renv::restore()
```

Then launch the app locally:

```r
shiny::runApp()
```

## Code style

Please follow the existing code style and keep UI and server logic cleanly separated. Comments are appreciated for non-obvious logic.

## Questions?

Feel free to open a discussion/reach out via the issue tracker.
