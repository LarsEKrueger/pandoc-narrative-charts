# Pandoc filter to generate narrative charts

This module provides a filter for [pandoc](https://www.pandoc.org) to generate
[narrative charts](https://www.xkcd.com/657). The look of these charts can be
customized to a certain extent (e.g. colors, line widths). The story line of
each character is routed automatically with respect to the other characters.

# Example

The chart

![Example Narrative Chart](doc/example.svg)

has been generated from the following document:

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

