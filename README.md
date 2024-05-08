# Spotlight Report Analysis Code (2022)

This repository contains R code, documentation and data related to a survey of non-white ethnic minority UK residents on their experience of climate change. This code and [underlying data](https://zenodo.org/records/11130201) have been used to generate visualisations for the [Spotlight report](https://zenodo.org/records/11130504).

Because this codebase is written as reproducible research (more on this below) the R code will automatically download the dataset from our zenodo repository. You can also view this and view other details about the underlying data by clicking here: [https://zenodo.org/records/11130201](https://zenodo.org/records/11130201). 

Users who are new to programmatic data science techniques using R, may also want to have a look at my open-access textbook, "Hacking Religion" which provides a walkthrough and explanation of coding in R using this dataset in chapter 2. You can view the code for the textbook [here](https://github.com/kidwellj/hacking_religion_textbook/blob/master/hacking_religion/chapter_2.qmd) and view a [beta version of chapter 2 online here](https://kidwellj.github.io/hacking_religion_textbook/chapter_2.html).



# About the code and data

Code and the paper here are written in R Markdown and for the most part, using the conventions outlined by Kieran Healy [here](https://kieranhealy.org/blog/archives/2014/01/23/plain-text/) which is best viewed in [R Studio](https://www.rstudio.com) though it will be reasonably comprehensible to anyone using a Markdown editor. If I'm not working in RStudio, I'm probably in Sublime text, FYI. Generally, I use [Hadley Wickham's R Style Guide](http://adv-r.had.co.nz/Style.html). 

I'd be extremely happy if someone found errors, or imagined a more efficient means of analysis and reported them as [an issue on this github repository](https://github.com/climate-experiences/spotlight-report/issues).

Paths in this folder are used mostly for R processing. I'm using a "project" oriented workflow, on which you can read more [in a blog by Jenny Bryan here](https://www.tidyverse.org/blog/2017/12/workflow-vs-script/), though for this project the code is relatively simple and we aren't using a large array of libraries, so we haven't bothered with a full development environment. 

Folders have the following significance:

- `data` contains datasets used for analysis. Some of this dataset hasn't been publicly released yet, so will be added at a later date.
- `derived_data` contains files which represent modified forms of files in the above path. This is usually empty and files are generated as you run the code here.
- `figures` contains images and visualisations (graphic files) which are generated by R for the final form of the document. I've included figures here for efficient downloading.
- `cache` isn't included in github but is usually used for working files

The code has been highly commented, in hopes that people with less R experience can understand some of the choices we've made in terms of using libraries for certain functions. There are RStudio format headings as well, indicating the different parts of the code.

One section which is worth highlighting is marked "Reusable Functions". In cases where data processing is repetitive, we have elected to make use of [R Functions](https://r4ds.had.co.nz/functions.html). These should be re-usable for other similar data sets (using LIKERT style questions, etc.) with only light editing, provided the data has been exported from Qualtrics in SPSS `.sav` format. We have made use of the haven() library to utilise `.sav` features and have found this to be superior to exports using CSV or XLSX.


## Why Reproducible Research?

If you're new to github and reproducible research, welcome! It's nice to have you here. Github is ordinarily a place where software developers working on open source software projects deposit their code as they write software collaboratively. However, in recent years a number of scholarly researchers, especially people working on research that involves a digital component have begun to deposit their papers in these same software repositories. The idea is that you can download all of the source-code and data used in an academic study or paper alongside the actual text, run it yourself and ["reproduce" the results](http://kbroman.org/steps2rr/). This can serve as a useful safeguard, a layer of research transparency, and a teaching tool for other persons interested in doing similar work. Particularly when, as is the case in subject areas that are only just starting to get involved in the digital humanities, there is a dearth of work of this nature, it can be helpful to have examples of practice which can be reused, or at least used as an example.

Eschewing proprietary, expensive and unreliable software like Microsoft Word, we're working with free and open source tools and languages from the digital commons: (1) [Markdown](https://en.wikipedia.org/wiki/Markdown) which is intended to be as close as possible to plain text while still allowing for things like boldfaced type, headings and footnotes; and (2) a programming language called [R](https://en.wikipedia.org/wiki/R_(programming_language)) to do all the data analysis. R is an object oriented language that was specifically designed for statistical analysis. It's also fun to tinker with.

To read a bit more on these things and start on your own path towards plain text reproducible research, we recommend:
- Mike Croucher's guide to [github](https://github.com/mikecroucher/Git_Academic_Benefits)
- Karl Broman's guide, "[Initial Steps Toward Reproducible Research](http://kbroman.org/steps2rr/)"
- Kieran Healy's guide, "[The Plain Person’s Guide to Plain Text Social Science](http://kieranhealy.org/files/papers/plain-person-text.pdf)"

The other advantage of putting this paper here is that readers and reviewers can suggest changes and point out errors in the code. To do this, we would ask that you create a github issue by clicking on the green "New issue" button [here](https://github.com/climate-experiences/spotlight-report/issues). More stuff about the project leads Charles Ogunbode and Jeremy Kidwell can be found [here](https://www.charlesogunbode.com/) and [here](http://jeremykidwell.info). Please also note that we have a code of conduct for contributors which [can be found here](https://github.com/climate-experiences/bame_climate_experiences_survey/blob/main/CODE_OF_CONDUCT.md).


# Prerequisites for reproducing this codebase

We've tried to follow best practices in setting up this script for reproducibility, but some setup is required before execution will be successful.

These steps are:

1. Acquire a working installation of R (and RStudio). 
2. Install platform appropriate prerequisites.
3. Clone or download the code from this repository.
4. Set up a proper R/RStudio working environment.
5. If possible, data used in this study is open. Any data which isn't available in another location and doesn't have an embargo will be in the `/data/` directory on this repository.


# Contributing

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.


# License

The content of any research papers and code in this repository are licensed under the [Creative Commons Attribution-ShareAlike 4.0 International Public License](https://creativecommons.org/licenses/by-sa/4.0/legalcode). Underlying datasets designed as part of this research have their own licenses that are specified in their respective repositories.
