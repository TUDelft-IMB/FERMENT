# FERMENT

FERMENT is an interactive R Shiny application for the visualization and comparison of fermentation experiment data. It reads structured workbooks produced from fermentation runs and provides four views: an **Overview** tab of all loaded experiments, a **Single Experiment** tab, a **Compare Experiments** tab for overlaying multiple runs, and an **Averages** tab for averaged data across selected experiments.

## Installation

### Prerequisites

You will need [R](https://www.r-project.org/) (≥ 4.2) and [RStudio](https://posit.co/download/rstudio-desktop/) installed on your machine. If you do not have them yet, install R first, then RStudio.

### Steps

1. **Get the repository**

    **Option A: GitHub Desktop (recommended if you are not familiar with the command line)**

    If you are not comfortable with the terminal, use [GitHub Desktop](https://desktop.github.com/). Open it, click **File → Clone Repository → URL**, and paste:
    ```
    https://github.com/TUDelft-IMB/FERMENT.git
    ```
    Choose a local folder where to save it and click **Clone**. GitHub Desktop will download the project for you.
    
    ---

    **Option B: Terminal (git)**

    If you are comfortable with the command line:
    ```bash
    git clone https://github.com/TUDelft-IMB/FERMENT.git
    cd FERMENT
    ```

    ---

    **Option C: Download as ZIP**

    On the [repository page](https://github.com/TUDelft-IMB/FERMENT), click **Code → Download ZIP**, extract the archive, and place the folder somewhere convenient on your machine.

    Note that this method means you will not receive future updates automatically as you would with git. To get updates, you would need to repeat the download process.

    ---

2.  **Open the project in RStudio**

    In RStudio, go to **File → Open Project** and select the `FERMENT.Rproj` file inside the cloned folder. This ensures RStudio sets the working directory correctly and that `renv` activates automatically for this project.
    
    > **Note:** When working in RStudio, open the project via its `.Rproj` file rather than opening individual scripts. This helps ensure that `renv` is activated and that the project's package library is used instead of your global R library.

3.  **Restore the package environment with `renv`**

    This project uses `renv` to lock all R package versions so that the app runs identically on every machine.

    > **Note:** If you do not have `renv` installed yet, run `install.packages("renv")` once in R before proceeding.

    When you open the project for the first time, `renv` will detect the lockfile automatically. In your R console, run:

    ``` r
    renv::restore()
    ```

    This reads `renv.lock` and installs exactly the package versions listed there into the project-local library. You do not need to install packages manually.

    > **First time only:** After `renv::restore()` completes, all required packages are available and you can launch the app normally. You never need to run `renv::restore()` again on the same machine unless the lockfile changes (e.g. after a `git pull` that updates dependencies).

4.  **Set the data directory**

    > **Note:** See the "Privacy and data policy" in [CONTRIBUTING](CONTRIBUTING.md) for details on data folder access.


    The app reads experiment files from a folder set by the environment variable `EXCEL_DIR`. Either modify the path in `global.R` (line 64) or add the path variable to a `.Renviron` file in the project root. The file `Renviron_example.txt` shows an example of how the `.Renviron` file should look like. 
    
    > **Note:** The `.Renviron` file is not tracked by git, so you can safely add your path without worrying about sharing it. It is also a hidden file, so you may need to enable hidden files in your file explorer to see it. On macOS you can view hidden files with `Cmd + Shift + .` in Finder. On Windows, you can enable "Hidden items" in the View tab of File Explorer.

5. **Launch the app**

   In RStudio, open any of `global.R`, `server.R`, or `ui.R` and click the **Run App** 
   button at the top right of the editor pane. RStudio will detect that these files belong 
   to a Shiny app and launch it automatically. This will open the app in a new window or 
   in your default web browser.

   > **Note:** Make sure you have opened the project via `FERMENT.Rproj` (Step 2) before launching. If you open a script file directly without loading the project first, the app may not find its data files or packages correctly.

## Usage

[TODO]

## Contributing

Contributions are always welcome! Please follow the steps described in the [contributing guidelines](./CONTRIBUTING.md).

## License

The repository is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Waiver

Technische Universiteit Delft hereby disclaims all copyright interest in the program “FERMENT” (Shiny app for visualization of fermentation data) written by the Author(s). 

Paulien Herder, Dean of Applied Sciences.

 Copyright (c) 2026 Ewout Knibbe.