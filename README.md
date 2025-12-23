<table>
<tr>
<td>
![DUOpop hexsticker](DUOpop.png)
</td>
<td>
<h1>DUOpop package</h1>
This package includes various metrics to measure the utility and privacy of synthetic data. It also provides functions that simplify fetching data from our SQL database. Additionally, the package offers preprocessing functions to prepare data for synthesis and features to streamline and accelerate the synthesis process. 
</td>
</tr>
</table>

Package Installation:

1.  Ensure you are in a 'clean' R environment, with no loaded data objects or packages, to avoid issues with updating existing packages.

    -   Close all RStudio windows until only one is open.

    -   In RStudio, go to the **Tools** menu \> **Global Options** \> uncheck "**restore .RData into workspace at startup**" \> **OK**.

2.  In RStudio, go to the **Session** menu, select **Terminate R**, and then execute the following commands in the R console:

`> install.packages('remotes')`

`> remotes::install_local("I:/Team Data-Science/Synthetische Data/R package/DUOpop_1.0.tar.gz", upgrade = F)`
