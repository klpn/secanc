#secanc
This program can be used to make simple visualizations of Swedish cancer incidence trends using the [Frames](https://github.com/acowley/Frames) and [Chart](https://github.com/timbod7/haskell-chart) Haskell packages, and data accessible in text format via [The National Board of Health and Welfare](http://www.socialstyrelsen.se/statistik/statistikefteramne/cancer). The text data currently covers the period from 1958 (when the Swedish Cancer Registry was funded) to 2011, while the [publicly accesible web interface](http://www.socialstyrelsen.se/statistik/statistikdatabas/cancer) only covers the period from 1970. Note, however, that the interpretation of incidence trends may be problematic trends due to changes in coding practice and diagnostics, and that this may especially affect for the first years after the start of the registry.

In order to use the program (given that the mentioned Haskell packages are installed):

1. Download and unzip the [compressed file with incidence data](http://www.socialstyrelsen.se/SiteCollectionDocuments/cancerstatistik-incidens-riket-2011.zip). The included file [`IncWebR2011head.txt`](IncWebR2011head.txt) consists of the first three lines from the file `IncWebR2011.txt` in this archive and is used to define the structure of the frame (if the whole `IncWebR2011.txt` is used for this, some column types are not correctly recognized).
2. Build the program (with `ghc --make secanc`, using GHC).
3. In order to generate and save a chart of time trends in SVG format, run `./secanc -i icd startyear endyear column filename`. E.g. `./secanc -i 2050 1960 2011 bef2000 aml.svg` will save a plot of incidence trends for acute myeloid leukemia 1960--2011, age standardized to the Swedish population 2000, to `aml.svg`. Supported columns (which currently do not include the columns with raw number of cases rather than incidence) and their descriptions are listed in the `inccols` map in [`secanc.hs`](secanc.hs), and different codes and their descriptions are listed in [`CodeAliases.hs`](CodeAliases.hs) (and there are also English descriptions in the file `Read me Sweden.txt` in the zipfile). It is also possible to plot the cumulative probability of developing cancer for a set of incidence columns defined in `icolgrps` in [`secanc.hs`](secanc.hs) by running `./secanc -c icd startyear endyear grp filename`. E.g.\ `./secanc -c 2050 1960 2011 0-74 amlcum.svg` will save a plot of the cumulative probability of developing acute myeloid leukemia up to age 74 during the period 1960--2011.
