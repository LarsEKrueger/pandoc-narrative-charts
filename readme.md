# Pandoc filter to generate narrative charts

This module provides a filter for [pandoc](https://www.pandoc.org) to generate
[narrative charts](https://www.xkcd.com/657). The look of these charts can be
customized to a certain extent (e.g. colors, line widths). The story line of
each character is routed automatically with respect to the other characters.

# Example

The chart

![Example Narrative Chart](./doc/example.svg "Example narrative chart")

has been generated from the following markdown document:

~~~
```narcha-event
who: hero
when: beginning
where: home
```

```narcha-event
who: hero
when: ending
where: fortress
```

```narcha-event
who: antagonist
when: kickoff
where: fortress
```

```narcha-event
who: hero
when: kickoff
where: street
```

```narcha-event
who: antagonist
when: kickoff
where: street
```

```narcha-event
who: hero
when: journey
where: road
```

```narcha-event
who: hero
when: climax
where: valley
```

```narcha-event
who: antagonist
when: climax
where: valley
```

```narcha-plot
axisColor: #666
nameHeight: 20
eventGrid: 100
nameLen: 100
left: 150
bottom: 130
```


```narcha-where
id: home
name: Home
key: "1"
```

```narcha-where
id: street
name: A Dirty Street
key: "3"
```

```narcha-where
id: road
name: A Lonely Road
key: "2"
```

```narcha-where
id: fortress
name: Evil Fortress
key: "6"
```

```narcha-where
id: valley
name: The Valley
key: "5"
```

```narcha-who
id: hero
name: Our Hero
color: blue
key: "5"
```

```narcha-who
id: antagonist
name: Villain
color: red
key: "6"
```

```narcha-when
id: beginning
name: Beginning
key: "1"
```

```narcha-when
id: kickoff
name: Kick-off
key: "2"
```

```narcha-when
id: journey
name: The Journey
key: "3"
```

```narcha-when
id: climax
name: Climax
key: "4"
```

```narcha-when
id: ending
name: Fin
key: "5"
```
~~~

Other input languagues should work as long as you can define code blocks with
classes/languages.

# Elements

There are five kinds of elements, some optional, to create a chart:

* Characters (`narcha-who`)
* Times (`narcha-when`)
* Places (`narcha-where`)
* Events (`narcha-event`, i.e. Who was when where?)
* Plots (`narcha-plot`)

The elements are removed from the parsed document. The plot element is replaces
with a raw HTML block, thus it's most likely that only HTML output will work
with this filter.

Elements can be presented in any order.

The syntax of each element is as follows:

## Events: `narcha-event`

~~~
```narcha-event
who: <who-id>
when: <when-id>
where: <where-id>
```
~~~

The three ids `<who-id>`, `<when-id>`, `<where-id>` are arbitrary unicode
strings that identify a character, a time, and a place this event will happen
in the story.

Events are unique. Mentioning the same event multiple times has no effect.

## Characters: `narcha-who`

~~~
```narcha-who
id: <who-id>
name: <name>
color: <color>
key: <key>
```
~~~

This element provides optional descriptions for characters. Again, this has to
be unique. If multiple elements for the same `<who-id>` are found, the last one is used.

The id `<who-id>` is one of the ids mentioned in the events. Descriptions for
non-existing characters, i.e. those without any events, is undefined behaviour.
At the moment, nothing will be rendered.

`<name>` is an arbitrary utf-8 string that will be copied verbatim into the svg
element. Since no escaping is used here, you can abuse it to insert arbitrary
tags into the generated svg. This can be a security risk. You have been warned.

`<color>` is an SVG colorspec, e.g. `red`, `#ff0012`. This is the color of the
story line.

`<key>` is an arbitrary utf-8 string that is used to sort characters.
Number-only keys have to be quoted, e.g.
```
key: "1968"
```

Keep in mind that keys are sorted lexicographically, e.g. in the following
order:
```
key10
key1
key9
```

## Times: `narcha-when`

~~~
```narcha-when
id: <when-id>
name: <name>
key: <key>
```
~~~

These are similar fields as in `narcha-who` with similar meanings and
limitations.

Extranous descriptions are also undefined behaviour and will be mapped to empty
time (columns) in the chart.

## Places: `narcha-where`

~~~
```narcha-where
id: <where-id>
name: <name>
key: <key>
```
~~~

These are similar fields as in `narcha-who` with similar meanings and
limitations.

Extranous descriptions are also undefined behaviour and will be mapped to empty
places (rows) in the chart.

The `<name>` field can be a multi-line YAML string, e.g.

~~~
```narcha-where
id: somewhere
name: |-
  Here
  There
  Everywhere
key: "012"
```
~~~

For each line, a separate line will be produced in the SVG.

In case you do not add `narcha-where` definitions, the `<name>` and `<id>` fields will be set to
the `<id>` field of the event as used in an event. In this case, the keys will
be generated as zero-padded three-digit numbers in order of apperance in the
events. This will generate the cleanest charts.

## Plots: `narcha-plot`

~~~
```narcha-plot
axisColor: <color>      [black]
top: <integer>          [20]
bottom: <integer>       [100]
left: <integer>         [100]
right: <integer>        [50]
arrowGap: <integer>     [10]
eventGrid: <integer>    [50]
nameHeight: <integer>   [10]
nameEdge: <integer>     [1]
nameLen: <integer>      [50]
nameGap: <integer>      [1]
show: <boolean>         [yes]
```
~~~

This will generate a plot from the elements with the parameter above. The
parameters can be given in any order. If a parameter is not present, the value
in the square brackets will be used instead.

`<color>` again denotes a valid SVG color spec.

`<integer>` denotes a positive integer. Most have lower limits around 0. The
meaning of these elements is:

| Parameter  | Meaning                                                                                                |
| :---       | :---                                                                                                   |
| axisColor  | Color of axes and text.                                                                                |
| top        | Number of pixels above the first row.                                                                  |
| bottom     | Number of pixels below the arrow of time. If this is too small, the time descriptions will be cut off. |
| left       | Number of pixels left of the matrix. If this is too small, the place descriptions will be cut off.     |
| right      | Number of pixels right of the tip of the arrow.                                                        |
| arrowGap   | Number of pixels between the last time mark and the arrow head.                                        |
| eventGrid  | Number of pixels between two time marks.                                                               |
| nameHeight | Height of name strings in pixels.                                                                      |
| nameEdge   | Number of pixel, denoting the vertical gap between character name boxes.                               |
| nameLen    | Number of pixels, denoting the width of the character name box. The name will not be clipped.          |
| nameGap    | Number of pixels between the box of the character name and the first event marker.                     |
| show       | Should the plot be displayed. Use this to temporarily disable a plot.                                  |

# Possible improvements

* Use natural sort for keys, e.g.
    ```
    thing10
    thing1
    thing-12
    ```
    would be sorted as
    ```
    thing-12
    thing1
    thing10
    ```
* Better parsing of integer keys, i.e. no more quotes.
* Nicer curves on the first events
* Generate multiple plots from the events, e.g. different levels of detail.
  * tag events
  * filter events by tags in plots
