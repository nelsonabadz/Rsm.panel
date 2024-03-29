
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rsm.panel

<!-- badges: start -->
<!-- badges: end -->

The objective of Rsm.panel is to offer a Shiny panel featuring response
surface methods. This is achieved by allowing users to specify the
dependent and independent variables of the uploaded dataset. The
function further provides various first- and second-order models, both
with and without interaction terms. Additionally, Rsm.panel furnishes
insights into the optimal region of the surface and its corresponding
location.

## Installation

You can install the development version of Rsm.panel like so:

``` r
install.packages("devtools")
devtools::install_github(repo = "nelsonabadz/Rsm.panel",
                         dependencies = TRUE, 
                         upgrade = "always")
```

In the case that you come across this message:

``` r
Downloading GitHub repo nelsonabadz/Rsm.panel@HEAD
These packages have more recent versions available.
It is recommended to update all of them.
Which would you like to update?
```

Please, select the option **All** for the correct installation and functionality of the package.

If you still have some issues with dependencies, try running R and RStudio as administrator.

## Example

``` r
# Execute the function and provide the dataset. That's all!
library(Rsm.panel)
Rsm.panel()
```

![Captura](https://github.com/nelsonabadz/Rsm.panel/assets/44551729/56a2b81b-ad89-44e2-8837-5108a56236e9)

Once you have uploaded the file (an Excel file), you specify the factors and the response variable in the interface. With all that set up, the package will perform the corresponding response surface analysis.
