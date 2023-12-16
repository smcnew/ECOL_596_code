# ECOL 596W: Practical and Reproducible Data Science for EEB

#### Instructor: Sabrina McNew

##### email: [mcnew\@arizona.edu](mailto:mcnew@arizona.edu){.email}

##### office: BSW 326

#### Course Information

Meetings: Tuesday and Thursdays 9:30 - 10:45

Koeffler 209

Fall 2023

### Resources for ECOL 596W:

Here you will find readings, datasets, and scripts for the class. There are three folders of interest:

1.  /readings: where pdfs of class readings are posted.\
2.  /datasets: where .csv files of datasets that we'll be using in class will be used
3.  /scripts: where scripts for in-class assignments will be placed

How to set up a class project for easiest use:

*Option A)* If you are not yet familiar with git integration (don't worry, you will be by the end of this course!), create a directory on your home computer with the same file structure, i.e. a folder called "Ecol_596_code" and subfolders named datasets, readings, and scripts. This project structure will streamline your analyses going forward.

*Option B)* If you are already familiar with git integration, feel free to fork the repo to your own github account and create a local copy for your own computer.

Update: Now that we're all git experts you should be able to fork and clone this repo to your computer. The easiest way to do this is using the usethis package. You will replace the destdir argument with the path to wherever you want the class repo.

If this command doesn't work, (and for more information) read the chapter on forking and cloning here <https://happygitwithr.com/fork-and-clone>

```         
usethis::create_from_github(
  "https://github.com/smcnew/ECOL_596_code.git",
  destdir = "~/path/to/where/you/want/the/local/repo/",
  fork = TRUE
)
```
