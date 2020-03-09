+++
title = "Latex Exporter"
author = ["Óscar Nájera"]
draft = false
weight = 1004
+++

## Using modern-cv {#using-modern-cv}

[moderncv](https://www.ctan.org/tex-archive/macros/latex/contrib/moderncv) is a standard \\(\LaTeX\\) package that you can find in many of your
latex distributions. For I maintain for personal purposes a fork of it to
better work with my use case at <https://github.com/Titan-C/moderncv.git>
Feel free to use any or even your personal fork for your desired use case.

To configure the export for moderncv you need the addition options in your
org file.

```org
# CV theme - options include: 'casual' (default), 'classic', 'oldstyle' and 'banking'
#+CVSTYLE: banking
# CV color - options include: 'blue' (default), 'orange', 'green', 'red', 'purple', 'grey' and 'black'
#+CVCOLOR: green
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'moderncv "moderncv.tex")
(org-latex-compile "moderncv.tex")
```

<object data="moderncv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="moderncv.org.pdf">to the PDF!</a></p>
</object>


## Using alta-cv {#using-alta-cv}

[AltaCV](https://github.com/liantze/AltaCV) is another project to generate a CV, you will need to install it
yourself. I maintain a fork too at <https://github.com/Titan-C/AltaCV.git>
because I need extra features and I encourage to use this fork on the
`sections` branch.

The style of this CV is more involved and you need some configuration in
your org file to get it to work. First define the margins, the large margin
to the right is to allow for a second column.

```org
#+LATEX_HEADER: \geometry{left=1cm,right=9cm,marginparwidth=6.8cm,marginparsep=1.2cm,top=1.25cm,bottom=1.25cm}
```

Content on the right column has the same structure of a org file, but you
need to enclose it in the `\marginpar{}` command as shown next.

```org
#+latex: \marginpar{
```

```org
* Main Interests
- Free/Libre and Open Source Software (FLOSS)
- Free food
- Free beer

* Programming
- Python
- C/C++
- EmacsLisp
- Bash
- JavaScript
- PHP

* Languages

- *English*  Fluent
- *German*   Fluent
- *Spanish*  Native
- *French*   Intermediate
```

```org
#+latex: }
```

When exporting you can call the following function to get the latex file.

```emacs-lisp
(org-export-to-file 'altacv "altacv.tex")
(org-latex-compile "altacv.tex")
```

<object data="altacv.org.pdf" type="application/pdf" width="100%" height="500px">
<p>Alternative text - include a link <a href="altacv.org.pdf">to the PDF!</a></p>
</object>


## Using AwesomeCV {#using-awesomecv}

[AwesomeCV](https://github.com/posquit0/Awesome-CV) is another LaTeX template for producing nice-looking
CVs. In addition to the regular document attributes, the following are supported:

<div class="ox-hugo-table table table-striped">
<div></div>

| Field            | Description                                               |
|------------------|-----------------------------------------------------------|
| PHOTOSTYLE       | Style of photo to use. Comma-separated values can include |
|                  | circle/rectangle,edge/noedge,left/right.                  |
| CVCOLOR          | Color of highlights.                                      |
| STACKOVERFLOW    | Stack overflow, must be specified as `ID username`        |
| FONTDIR          | Directory where the fonts can be found, defaults          |
|                  | to `fonts/` (as in the standard AwesomeCV)                |
| CVHIGHLIGHTS     | Whether to colorize highlights. Defaults to true          |
| QUOTE            | Optional quote to include at the top of the CV            |
| FIRSTNAME        | First name to be shown in the CV. By default the first    |
|                  | space-separated part of AUTHOR is used.                   |
| LASTNAME         | Last name to be shown in the CV.  By default the second   |
|                  | space-separated part of AUTHOR is used.                   |
| CVFOOTER\_LEFT   | Text to include in the left footer. None by default       |
| CVFOOTER\_MIDDLE | Text to include in the middle footer. None by default.    |
| CVFOOTER\_RIGHT  | Text to include in the right footer. None by default.     |

</div>

AwesomeCV supports a few additional types of environment types in
`CV_ENV`, including `cvemployer`, `cvskills`, `cvhonors` and `cvschool`. Some of
these support additional property fields:

<div class="ox-hugo-table table table-striped">
<div></div>

| Field      | Description                                                          |
|------------|----------------------------------------------------------------------|
| FROM       | Start date of the entry                                              |
| TO         | End date of the entry                                                |
| DATE       | Shortcut to specify both `FROM` and `TO` as the same date.           |
|            | Both `FROM` and `TO` override `DATE`.                                |
| EMPLOYER   | Employer or organization, can also be specified                      |
|            | as `ORGANIZATION`, `SCHOOL`, `EVENT` or `POSITION` (different        |
|            | names make more sense depending on the type of environment)          |
| LABEL      | In `cvsubentry` environments, adds the given text to the left        |
|            | of the date range, can be used to add additional information         |
|            | to the entry.                                                        |
| RIGHT\_IMG | path to an image to include floating to the right of a `cventry`,    |
|            | a `cvsubentry` or `cvschool` entry. Meant to be used to show a logo. |
| PAGEBREAK  | Causes a LaTeX `\clearpage` statement to be inserted in the          |
|            | exported output before the heading.                                  |

</div>

All the supported values of `CV_ENV` are described below.


### `cventries` {#cventries}

Enclose all the subheaders in a `cventries` environment. Subheaders can
be of type `cventry`, `cvschool`, or `cvemployer`.


### `cvhonors` {#cvhonors}

Enclose all the subheaders in a `cvhonors` environment. Subheaders must
be of type `cvhonor`


### `cventry` {#cventry}

Converts to a `\cventry` command. Supports attributes `FROM`, `TO`, `DATE`,
`EMPLOYER`, `LOCATION`, `RIGHT_IMG`.


### `cvsubentry` {#cvsubentry}

Converts to a `\cvsubentry` command. Supports attributes `FROM`, `TO`, `DATE`,
`LABEL` `RIGHT_IMG`.


### `cvemployer` {#cvemployer}

Converts to a `\cventry` with only the title line. Supports attributes
`FROM`, `TO`, `DATE` and `LOCATION`.


### `cvschool` {#cvschool}

Converts to a `\cventry`. The headline should contain the degree
obtained, shown as the main title. Supports attributes `LOCATION`,
`SCHOOL`, `FROM`, `TO`, `DATE` and `RIGHT_IMG`.


### `cvhonor` {#cvhonor}

Converts to a `\cvhonor` command (must be inside a `cvhonors`
headline). Supports attributes `LOCATION`, `EMPLOYER` (in this case `EVENT`
or `POSITION` might be more semantically accurate, and can also be
used), `FROM`, `TO`, `DATE`.


### `cvskills` {#cvskills}

Converts to a `\cvskills` environment. The headline must contain a
[description list](https://orgmode.org/manual/Plain-lists.html), which gets converted into a sequence of `\cvskill`
commands, with the term as the skill title and the description as its
contents.
