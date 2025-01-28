# R Template for Statistical Analysis Projects

## Introduction

You can use this template repository to quickly get up and running with R for your statistical analysis projects.

## Prerequisites (if using Visual Studio Code)

Follow the steps [here](https://code.visualstudio.com/docs/languages/r) to configure Visual Studio Code for R.

## Run the Code

1. After cloning the repository, open the repository root in Visual Studio Code.
2. In Visual Studio Code, open a new R terminal.
3. Run `renv::restore()` to install required packages as specified in the `renv.lock` file.
4. Open an R file in the editor, i.e. `the-office\office-ratings.r`.
5. Run the file.

## Formatting Files

The following code formats R files according to the configuration specified in the `.lintr` file:

```R
styler::style_dir("./")
```
