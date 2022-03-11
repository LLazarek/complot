

# The complot language

complot is an embedded domain-specific language (DSL) within the context of Racket.
Embedding within Racket offers flexibility for doing calculations orthogonal or in support of the primary focus of composing data visualizations;
hence, complot intentionally limits its scope of linguistic features to the particular domain of compositional visualization construction.

To use the DSL, either start a program with

    #lang complot

or you can use complot as a library in racket programs using `require`:

    (require complot)

The DSL provides a set of primitives, organized as follows.
First, the `plot` primitive produces a canvas for creating a visualization.
Then, the `add-to` primitive adds *elements* and *renderers* to a plot/canvas to produce a new plot, which can subsequently have more added to it or be rendered.
*Plot elements* are non-data-depicting visual components of a plot, such as axes or annotations.
*Plot renderers* are data-depicting visual components of a plot, such as data points, lines, or bars.
Each renderer specifies what logical axes of the data it depicts.

This is the full list of primitive forms, with each form's specification.

-   `(plot data)` creates a "plot" of `data` with out any visual components; in other words, a blank canvas. See data primitives for obtaining data.

-   `(save-to-file a-plot-or-element-or-renderer path)` saves an image of a the given plot or element or renderer to a file on disk.

-   `(add-to a-plot element-or-renderer ...)` adds the given plot elements or renderers to the given plot. This form is the main compositional tool of the language.

-   Plot elements:
    -   `(x-axis option ...)` / `(y-axis option ...)` creates an x (or y) axis. `options` can be supplied to control how the axis looks, such as the number of ticks or the min and max. They are supplied with keywords of a form like `#:min 0`, which for instance specifies a minimum of 0. Supported keywords are:
        -   `#:label <string>` - the axis label
        -   `#:ticks? <boolean>` - whether the axis should have tick marks
        -   `#:major-ticks-every <number>` - at which interval the axis should have major tick marks
        -   `#:minor-ticks-between-major <number>` - how many minor tick marks should be between major tick marks
        -   `#:ensure-min-tick? <boolean>` - whether there must be a tick mark for the minimum value of the axis
        -   `#:ensure-max-tick? <boolean>`
        -   `#:minimum-ticks <listof number>` - numbers which must have major tick marks, regardless of the interval of ticks
        -   `#:tick-lines? <boolean>` - whether ticks should draw lines across the plot area
        -   `#:layout <symbol>` - the layout of tick marks (e.g. linear, logarithmic)
        -   `#:min <number>` - the minimum value on the axis
        -   `#:max <number>` - the maximum value on the axis
    
    -   `(legend option ...)` creates a legend. Supported keywords:
        -   `#:position <symbol>` - the position of the legend
        -   `#:type <symbol>` - the type of the legend (e.g. a key, or line annotations)
    
    -   `(point-label x y content option ...)` creates a single point with a label. Supported keywords:
        -   `#:anchor <symbol>` - the position of the label relative to the point
        -   *Renderer apperance keywords* (see specification below)
    
    -   `(title text)` creates a title.

-   Renderers:
    -   `(points #:x <string> #:y <string> #:option ...)` renders data as a scatter plot. Supported keywords:
        -   *Renderer apperance keywords* (see specification below)
    
    -   `(line #:x <string> #:y <string> option ...)` renders data as a line plot. Supported keywords:
        -   *Renderer apperance keywords* (see specification below)
    
    -   `(bars #:x <string> #:y <string> option ...)` renders data as a bar chart. Supported keywords:
        -   *Renderer apperance keywords* (see specification below)
    
    -   `(stacked-bars #:x <string> #:y <string> #:group-by <string> option ...)` renders data as a stacked bar chart, where each stack is collected according to `#:x`, the values depicted are from `#:y`, and the bar segments/groups correspond to each distinct value of `#:group-by`. Supported keywords:
        -   `#:colors <listof string or symbol or number>`
        -   `#:alpha <number in [0, 1]>`
        -   `#:invert? <boolean>` - whether to depict the bars horizontally instead of vertically
        -   `#:aggregate <function>` - how to aggregate values within the same group. Default: sum
        -   `#:labels? <boolean>` - whether to label each bar segment with its group value.
    
    -   `(histogram #:x <string> option ...)` renders a frequency histogram of a single logical axis. Supported keywords:
        -   `#:bins <natural>` - how many bins to split the data into
        -   `#:invert? <boolean>` - whether to depict the bars horizontally instead of vertically
        -   *Renderer apperance keywords* (see specification below)
    
    -   `(function <function> #:min <number> #:max <number> option ...)` renders a function. Supported keywords:
        -   `#:name <string>` - the name of the function in the legend (if any)
        -   *Renderer apperance keywords* (see specification below)

-   Data primitives:
    -   `(read-data file-path)` reads data from a file.
    -   `(describe data)` describes the shape of a data set, including the number and names of columns (i.e. logical axes) and number of rows.

*Renderer apperance keywords* are:

-   `#:color <symbol or string or natural>` - color of the renderer
-   `#:alpha <number in [0, 1]>` - transparency, where 0 is fully transparent
-   `#:size <number>` - size of renderer marks (e.g. of points, or thickness of lines and bars)
-   `#:type <symbol>` - the type of renderer (e.g. shape of points, style of line)


# Visualizing plots

Every value constructed by a complot primitive can be depicted visually simply by evaluating it at the REPL.
For instance, a user can create an x-axis by itself and render it to see how it looks before adding it to any plot.
To support this for renderers, a small dummy data set stands in for the data that users would depict when a renderer is added to a plot.

