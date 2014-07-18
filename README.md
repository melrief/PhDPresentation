# README

This is the source code of the presentation of my Ph.D. defense. It is
written in *Haskell* and uses the [HaTeX](https://github.com/Daniel-Diaz/HaTeX)
library to generate the *LaTeX* code of both the style file and the presentation file.


## Dependencies

[HaTeX](https://github.com/Daniel-Diaz/HaTeX) of course,
[Shake](https://github.com/ndmitchell/shake) that is used as build system (see the *Shakefile*) and a *pdflatex* command in the path. If you
don't have pdflatex or you don't use pdflatex then you have to edit the *Shakefile* to use your compiler.


## Generate the Presentation

To compile the project, execute the *Shakefile* from the project directory:

    runhaskell Shakefile
    
this will create a directory *build/* with three subdirectories:

- **build/dist/** is used for the output of *GHC*
- **build/gen-sources/** contains the produced *LaTeX* code
- **build/output/** contains the output of the *LaTeX* compiler and in particular the presentation called **build/output/main.pdf**


## Generated Files

The important files that the *Shakefile* creates are:

- **build/dist/build/createPaper/createPaper** the executable that when run creates the *LaTeX* source code
- **build/gen-sources/main.tex** and **build/gen-sources/mainStyle.tex** the generated one-line *LaTeX* source code of the presentation and the style. Sadly, the source code generate is in one-line and without spacing, so in the same directory there are also two alternative files called *mainPretty.tex* and *mainStylePretty.tex* that should be easier to read. The reason why we generate both is that *HaTeX* doesn't guarantee that the output of the pretty printing is correct.
- **build/output/main.pdf** the presentation file
                          * 

## Source Code Organization

The source code of the presentation is in the *src/* directory and it is organized as follow:

- **src/Main.hs** contains the *main* function responsible for creating both the style file, with source code in *src/Style.hs*, and the presentation file, whith source code in *src/Presentation.hs*
- **src/Style/Outer.hs** the outer style of the presentation
- **src/Style.hs** the style of the presentation, based on CambridgeUS but with my outer style
- **src/Presentation.hs** the presentation itself
- **src/Commands.hs** a set of commands not available in *HaTeX* that I used to do this presentation
- **src/Utils.hs** some utility functions

In the same directory there is also a *TiKZ* file called *src/responsetimes.tex* that is taken from the thesis and used as it is.
