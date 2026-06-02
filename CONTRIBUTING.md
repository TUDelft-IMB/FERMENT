# Contributing guidelines for FERMENT

Contributions are always welcome! Here's how to get started:

## Reporting issues

Open an issue on GitHub with a clear description of the bug or feature request. Include steps to reproduce, expected vs. actual behavior, and your R/package versions where relevant.

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
