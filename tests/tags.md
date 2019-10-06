# Simple test page to generate multiple narrative charts using tags

```narcha-event
who: Cat
when: void
where: home
tags: [animal, good]
```
```narcha-event
who: Cat
when: beginning
where: home
tags: [animal, good]
```

```narcha-event
who: Cat
when: journey
where: street
tags: [animal, good]
```

```narcha-event
who: Cat
when: ending
where: fortress
tags: [animal, good]
```

```narcha-event
who: Cat
when: happy-end
where: fortress
tags: [animal, good]
```

```narcha-event
who: hero
when: beginning
where: home
tags: [human, good]
```

```narcha-event
who: hero
when: ending
where: fortress
tags: [human, good]
```

```narcha-event
who: antagonist
when: beginning
where: fortress
tags: [human, bad]
```

```narcha-event
who: hero
when: kickoff
where: street
tags: [human, good]
```

```narcha-event
who: antagonist
when: kickoff
where: street
tags: [human, bad]
```

```narcha-event
who: hero
when: journey
where: road
tags: [human, good]
```

```narcha-event
who: hero
when: climax
where: valley
tags: [human, good]
```

```narcha-event
who: antagonist
when: climax
where: valley
tags: [human, bad]
```

```narcha-event
who: sidekick
when: kickoff
where: valley
tags: [human, good]
```

```narcha-event
who: sidekick
when: journey
where: valley
tags: [human, good]
```

```narcha-event
who: sidekick
when: ending
where: fortress
tags: [human, good]
```

```narcha-event
who: minion
when: kickoff
where: fortress
tags: [human, bad]
```

```narcha-event
who: minion
when: ending
where: road
tags: [human, bad]
```

```narcha-event
who: bird
when: happy-end
where: fortress
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
name: A Lonely Roäd
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
id: Cat
name: A stray cat
color: yellow
key: "00"
```

```narcha-who
id: hero
name: Our Herö
color: blue
key: "10"
```

```narcha-who
id: sidekick
name: Assistant
color: lightblue
key: "11"
```

```narcha-who
id: antagonist
name: Villain
color: red
key: "20"
```

```narcha-who
id: minion
name: Helper
color: pink
key: "21"
```

```narcha-who
id: bird
name: Bird
color: cyan
key: "22"
```

```narcha-when
id: void
name: Big Bang
key: "0"
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
name: Climäx
key: "4"
```

```narcha-when
id: ending
name: Fin
key: "5"
```

```narcha-when
id: happy-end
name: Happy End
key: "6"
```

## Plots

## Animals
```narcha-plot
axisColor: #666
nameHeight: 20
eventGrid: 100
nameLen: 100
left: 150
bottom: 130
tags: [animal]
```

## Humans
```narcha-plot
axisColor: #666
nameHeight: 20
eventGrid: 100
nameLen: 100
left: 150
bottom: 130
tags: [human]
```

## Good
```narcha-plot
axisColor: #666
nameHeight: 20
eventGrid: 100
nameLen: 100
left: 150
bottom: 130
tags: [good]
```

## All
```narcha-plot
axisColor: #666
nameHeight: 20
eventGrid: 100
nameLen: 100
left: 150
bottom: 130
```
