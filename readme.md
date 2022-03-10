complot readme
==============

Outline
-------
1. Introduction
2. Installation
3. Tutorial
4. complot's architecture
5. Suggestions for reading this code to evaluate it (e.g. if your name is Jacob)

Introduction
------------

complot is a domain-specific language for data visualization that emphasizes *composition* as the primary tool of both plot creation and program structure.
In other words, complot creates a direct link between linguistic forms and graphical components of a visualization, and provides linguistic support for putting those components together in the way that graphical components can be created and then added to a plot.

The goals/purpose of this design is:
1. The compositional structure of complot makes for simple programs that directly connect to a graphical way of thinking about "putting together" a plot.
2. That link between programmatic and graphical composition may help novice programmers get started both creating data visualizations and learning to program with the powerful idea of composition.
3. The compositionality lends itself easily to abstraction and programmatic manipulation, which may be useful for data visualization practitioners.


Installation
------------
Install Racket by getting the installer from https://download.racket-lang.org/ and running it with `sh`.
If you're just trying complot out, you probably want to install Racket in a local directory (e.g. `Downloads/racket`) and accept the installer default to **not** create any links; that way you can uninstall it by simply deleting the directory.
Then enter the complot directory and install complot with the racket package manager:

```
cd complot/
<path/to/racket-directory>/bin/raco pkg install
```

This will download and install complot's dependencies, and once it completes you are ready to go.

If you're not sure where to start, consider checking out the tutorial below, or the examples in `complot/examples/`; you may also want to browse the language reference in `reference.md`.


Tutorial
--------
To get started with complot, just open up DrRacket and create a new file starting with

```
#lang complot
```

(If you followed the installation instructions above and made a local install of racket, you can find the DrRacket executable under the folder you installed Racket in at `<path-to-racket-dir>/bin/drracket`.)

complot is a DSL embedded within Racket, so all Racket is available to complot programs.

We'll use the `A-B-price-data.csv` dataset in the examples (at `complot/examples/A-B-price-data.csv`), and the first thing to do is to load up the data.

```
(define data (read-data "path-to-a-csv.rkt"))
```

Now you can use `describe` see the layout of the data.
Click the green "run" button on the top right and then in the REPL enter:

```
(describe data)
```

Now we know that there are three columns in the data: `day`, `A`, and `B`.
Let's create a visualization.
First, create a canvas on which we can add components of our plot.

```
(define base (plot data))
```

Now we can add components to this base to visualize our data.
Clikc the "run" button again and over in the REPL we can experiment adding different visual elements to the plot.
For example, try this to see a histogram of unique values in the `day` column:

```
(add-to base (histogram #:x "day"))
```

And similarly for `A` and `B`.

It's a bit difficult to tell what the bars mean without axes, so let's add some.

```
(add-to base
        (histogram #:x "day")
		(x-axis)
		(y-axis))
```

That's better!

The `day` column is a sequence of days, so we can plot the `A` prices against those days. And let's add some axes too.

```
(add-to base
        (line #:x "day" #:y "A")
		(x-axis)
		(y-axis))
```

Adding axes all the time is pretty annoying though, so let's go back over to the program area on the left and add a new definition that is our canvas with some axes.

```
(define base+axes
  (add-to base
          (x-axis)
		  (y-axis)))
```

Now if we click "run" we can recreate our plots in the REPL without needing to add axes every time.

```
(add-to base+axes
        (line #:x "day" #:y "A"))
```

complot supports several different ways of visualizing data, called *renderers*, such as `points`, `bars`, and `stacked-histogram`.
Each of these has many options for controlling things like color, see `reference.md` for details, but for example:

```
(add-to base+axes
        (line #:x "day" #:y "A" #:color "purple"))
```


We can also add other visual elements like titles and legends.
For example:

```
(add-to base+axes
        (line #:x "day" #:y "A" #:color "purple")
		(legend))
```

That should be enough to get you started.
Be sure to look at the reference in `reference.md` to see all of the things you can do with complot.
When you're ready to save a plot you've created, just use `save-to-file`, like so:

```
(save-to-file my-final-plot "/path/to/some/place.png")
```



complot's architecture
----------------------
Under the hood, complot implements a separation between the internal representation of a plot and its rendering/appearance.
Specifically, every plot created by `plot` is represented by a data structure, which contains all of the information necessary to render the plot.
That includes plot elements and renderers, all of which are also represented by data structures.
All of these data structures are immutable.
See `structs.rkt` for the definition of the data structures.

Hence, the compositional operator `add-to` simply constructs a copy of its given plot data structure which has an element/render added to it.
This design choice shifts all of the complexity of interpreting the data structures to the part of the system that renders them.

complot data structures are rendered by interpreting the information they contain and converting it into options and arguments for Racket's `plot` library.
The `plot` library offers a complex and rich interface to creating plots, so to a first approximation each piece of the complot data structures simply need to be translated into the corresponding option for `plot`.
Of course, some complot elements don't correspond one-to-one with a single `plot` option, so one simple complot option may translate to many `plot` options and correspondingly a single `plot` option may depend on the combination of several complot elements.
This all works out since the translation only happens at the latest possible time (rendering) when all of that information is available together.
Concretely, this is all done by a rendering function in the complot implementation.

See `complot.rkt` for the entry point to the rendering function, called `render`.
`render` defers the work of converting most of the information in the complot data structures to the functions `renderers->plot:renderer-tree`, `x-axis->plot:axis`, `y-axis->plot:axis`.
These functions are defined in `renderer-conversion.rkt` and `axis-conversion.rkt` respectively.

Finally, `main.rkt` is simply a layer over `complot.rkt` that serves the purpose of implementing support for `#lang complot` and `(require complot)`.
Since Racket's protocol for these forms looks for a file named `main.rkt` in the package directory.


Suggestions for reading this code to evaluate it (e.g. if your name is Jacob)
-----------------------------------------------------------------------------
I suggest first reading the preceding section describing the architecture of complot.
After that, I suggest starting with `complot.rkt`, which is the entry point of the system and implements the high level features of the language.
I have also annotated it extensively to explain racket syntax and features that I use.

Next I suggest checking out `structs.rkt`, to see the layout of the data structures that represent plots, renderers, and elements.

Those two modules should give a good high level view of how complot works.
The only other significant parts of the implementation are in `renderer-conversion.rkt` and `axis-conversion.rkt`, but they mostly implement lower level munging of data and similar things that aren't really critical to understanding how complot works.

