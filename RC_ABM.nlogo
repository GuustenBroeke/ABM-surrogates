globals [
  deltat
  deltax
  R_pre
  R_post
  Delta_R
  E_pre
  E_post
  n_tot
  n_h
  n_m
  n_i
  E_d
  E_d2
  delta_E
  E_start
  E_postharvest
  E_postmove
  E_postmaintenance
  E_end
  resource-grow-old
  resource-dif
]

patches-own [
  resource               ;; Each site owns an amount of resource that is harvested by the turtles
  resource-old           ;; Used to temporarily store the old amount of resource during updating
  resource-observed      ;; Used to temporarily store resource estimate of turtle
]
turtles-own [
  E_a                    ;;Current internal energy
  harvesting?            ;;Is the turtle attempting to harvest this time-step?
  harvested?             ;;Has the turtle attempted to harvest this time-step?
  moved?                 ;;Has the turtle moved this time-step?
  inactive?
  inactive_previous?
  moved_previous?
  harvested_previous?
  w_h                    ;;Internal motivation of the turtle to harvest
  w_m                    ;;Internal motivation of the turtle to move
  neighbour              ;; Used to store identity of neighbouring turtle
  resource-here          ;; Amount of resource the turtle estimates to be present on its site
  neighbourresource      ;; Amount of resource an turtle estimates to be present on a neighbouring site
]

to default-parameters                 ;; Set all parameters to their default value
  set K 2             ;; Carrying capacity of patches
  set r 0.1                 ;; Rate of resource growth
  set D 0.1       ;; Rate of resource diffusion
  set E_b 5                ;; Minimal energy required for turtles to procreate
  set E_m 0.1          ;; Energy required for maintenance each time-step
  set E_move 0.5                 ;; Energy required for moving
  set E_h 0.3              ;; Energy required for harvesting
  set R_max 0.5        ;; Maximum harvest size
  set c 0.9                  ;; Conversion factor between resource and turtle internal energy
  set z 0.2                   ;; Parameter determining the difference between parent and offspring characteristics
  set R_unc 0.1     ;; Uncertainty in resource observations
  set v_d 10                        ;; Parameter determining the probability of dying
  set v_b 10                      ;; Parameter determining the probability of procreation
  set n_0 100      ;; Initial number of turtles
  set R_0 1              ;; Initial amount of resource (proportion of carrying capacity)
  set deltat 1.                       ;; Size of time-step
  set deltax 1.                       ;; Size of patch
end

to go
  set E_start sum [E_a] of turtles
  patches-grow                            ;; Logistic growth of resource
  patches-diffuse                         ;; Diffusion of resource
  patches-recolor                         ;; Patches get a colour indicating the amount of resource
  set R_pre sum [resource] of patches
  set E_pre sum [E_a] of turtles
 ; turtles-reorder
  turtles-observe                         ;; Turtles observe their surroundings
  turtles-harvest?                        ;; Each turtle decides whether to harvest
  set E_postharvest sum [E_a] of turtles
  turtles-move?                           ;; Turtles that haven't harvested decide whether to move, and possibly do so
  set n_tot count turtles
  set n_h count turtles with [harvested? = 1]
  set n_m count turtles with [moved? = 1]
  set n_i count turtles with [inactive? = 1]
  set E_postmove sum [E_a] of turtles
  turtles-maintenance                     ;; Turtles pay their energy maintenance
  set E_postmaintenance sum [E_a] of turtles
  turtles-die?                            ;; Turtles die with a certain probability
  set E_end sum [E_a] of turtles
  turtles-breed?                          ;; Turtles reproduce with a certain probability
  set R_post sum [resource] of patches
  set Delta_R R_pre - R_post
  set E_end sum [E_a] of turtles
 ; ask turtles [setxy random-xcor random-ycor]
  tick                                    ;; The time-step is increased by one
  reset
end

to setup
  clear-all
  ask patches [ set resource R_0 * K]   ;;The initial resource for each patch is a random percentage of the carrying capacity
  patches-recolor
  create-turtles n_0                        ;;Create the turtles, then initialize their variables
  [
    set color red
    set E_a random-float E_b                       ;;The initial energy is random, below energy-birth
    set harvesting? 0                                          ;;The new turtles have not yet harvested
    setxy random-xcor random-ycor                              ;;Random initial location
    set w_h random-float 1                           ;;Random internal motivations, below 1
    set w_m random-float 1
  ]
  set deltax 1
  set deltat 1
  reset-ticks                                                  ;;Start at t=0
end

to patches-diffuse                                     ;; Diffusion of resource between patches
  ask patches [set resource-old resource]
  ask patches [set resource (1 - count neighbors4 * D) * resource-old + D * sum [resource-old] of neighbors4]
end

to patches-grow                                                ;;Each patch grows resource according to the logistic growth equation
  set resource-grow-old sum [resource] of patches
  ask patches [set resource K * resource * exp(r * deltat)/(K + resource * (exp(r * deltat) - 1))]
  set resource-dif K * count patches * resource-grow-old * exp(r * deltat)/(K * count patches + resource-grow-old * (exp(r * deltat) - 1)) - sum [resource] of patches
end

to patches-recolor                                             ;;Patches get a colour indicating the amount of resource
  ask patches [set pcolor scale-color green resource K 0]
end

to turtles-observe                   ;; Turtles estimate the resource on their location and on neighbouring sites
  ask turtles [
    ask neighbors4 [set resource-observed (resource + random-normal 0 R_unc)]
    set neighbour one-of (neighbors4 with-max [resource-observed / (count turtles-here + 1)])
    set neighbourresource [resource-observed] of neighbour
    set resource-here max list 0 (([resource] of patch-here) + random-normal 0 R_unc)
  ]
end

to turtles-harvest?
  ask turtles [
    if resource-here > 0 [                                                     ;;The decision is made for each turtle, so there is a loop over all turtles
      let harvestfunc w_h * (max(list (E_a / E_m - 1) 0)) * (max(list (1 - (resource-here)/(count turtles-here)/(R_max))))*(neighbourresource /(resource-here)*(count turtles-here)/([count turtles] of neighbour + 1))
      ;show harvestfunc
      let Pharvest 0
      if harvestfunc < 0 [set Pharvest 1]
      if harvestfunc >= 0 [set Pharvest exp(- harvestfunc)]
     ; show Pharvest
      ;let Pharvest exp(- w_h * (max(list (E_a / E_m - 1) 0)) * (max(list (1 - (resource-here)/(count turtles-here)/(R_max))))*(neighbourresource /(resource-here)*(count turtles-here)/([count turtles] of neighbour + 1)))
      if random-float 1 < Pharvest [set harvesting? 1]                ;;For each turtle, it is stored whether it will attempt to harvest
     ; show Pharvest
    ]
  ]
  turtles-harvest                         ;; Harvesting turtles check for competitors and act based on the result
end

to turtles-harvest                                                            ;;This function lets the turtles harvest
  ask turtles with [harvesting? = 1][                                   ;;All the turtles on the patch harvest
    let harvest min(list (c * [resource] of patch-here / count turtles-here with [harvesting? = 1]) (c * R_max))

    set E_a E_a + harvest - E_h ;;Each turtle harvests as much as possible, but if there is not enough for all turtles, everything is equally split
    ask patch-here [set resource resource - harvest / c]
    set harvesting? 0                                          ;;The turtles is no longer harvesting
    set harvested? 1                                           ;;This stores that the turtles has harvested
  ]
end

to turtles-move?                                        ;; Decision of turtles to move
  ask turtles with [harvested? = 0] [
    let Pmove 0                                        ;;Compute probability of moving
    if (-1 * w_m * ((count turtles-on neighbour + 1)/(count turtles-here)*(resource-here)* c /(c * neighbourresource - E_move))) > 0 [
      set Pmove 1
    ]
    if (-1 * w_m * ((count turtles-on neighbour + 1)/(count turtles-here)*(resource-here)* c /(c * neighbourresource - E_move))) <= 0 [
      set Pmove exp(- min list 10 w_m * ((count turtles-on neighbour + 1)/(count turtles-here)*(resource-here)* c /(c * neighbourresource - E_move)))  ;;Compute probability of moving
   ]
    ifelse random-float 1 < Pmove [
      move
    ]
    [
      set inactive? 1
    ]
  ]
end

to move                                          ;; Turtle movement
  move-to neighbour
  set E_a E_a - E_move
  set moved? 1
end

to turtles-maintenance
  ask turtles [set E_a E_a - E_m]  ;;Each turtle pays a constant maintenance for each time-step
end

to turtles-die?                                   ;;This function determines for each turtle whether it dies in the current time-step
  ask turtles [
    let Pdie exp(- v_d * E_a)    ;;The probability of dying, which depends on internal energy
    if random-float 1 < Pdie [
      set E_d E_d + E_a
      die                                 ;;The turtle dies, depending on the probability
    ]
  ]
end

to turtles-breed?              ;; Procreation
  ask turtles [
    let Pbirth 1 - exp(- v_b * (E_a - E_b))   ;; Computation of birth probability
    if Pbirth > random-float 1 [
      turtles-breed
    ]
  ]
end

to turtles-breed
  hatch 1 [                         ;;The turtle is copied (the hatch command gives the new turtle all the characteristics of the old one)
    set E_a E_a / 2           ;;The energy is divided equally over both turtles
    if adaptation? = true [
      set w_h w_h + (random-float z) - (z / 2)  ;;The internal motivations of the new turtle get some small deviation compared to the old turtle. The size of the deviation is random-uniform. The maximum deviation is a parameter, which for now is the same for all internal motivations
      if w_h < 0 [set w_h 0]                                    ;;The internal motivations need to be above 0.
      set w_m w_m + (random-float z) - (z / 2)        ;;The internal motivations of the new turtle get some small deviation compared to the old turtle. The size of the deviation is random-uniform. The maximum deviation is a parameter, which for now is the same for all internal motivations
      if w_m < 0 [set w_m 0]
      ;setxy random-pxcor random-pycor
    ]
    if adaptation? = false [
      set w_h random-float 1                           ;;Random internal motivations, below 1
      set w_m random-float 1
    ]
  ]
  set E_a E_a / 2
  if adaptation? = true [
    set w_h w_h + (random-float z) - (z / 2)  ;;The internal motivations of the old turtle also get a deviation in the same way.
    if w_h < 0 [set w_h 0]
    set w_m w_m + (random-float z) - (z / 2)
    if w_m < 0 [set w_m 0]
  ]
  if adaptation? = false [
    set w_h random-float 1                           ;;Random internal motivations, below 1
    set w_m random-float 1
  ]

end

to reset                 ;;Some turtle variables which store the actions for the current time-step are reset before the start of the next time-step
  ask turtles [
    set harvested_previous? 0
    set moved_previous? 0
    set inactive_previous? 0
    set R_pre sum [resource] of patches
    if harvested? = 1 [set harvested_previous? 1]
    if moved? = 1 [set moved_previous? 1]
    if inactive? = 1 [set inactive_previous? 1]
    set inactive? 0
    set harvested? 0
    set harvesting? 0
    set moved? 0
  ]
  set E_d2 E_d
  set E_d 0
end

to turtles-reorder
  let ntot sum [E_a] of turtles / 3.6
  let Etot sum [E_a] of turtles
  show Etot
  ask turtles [die]
  create-turtles round ntot                        ;;Create the turtles, then initialize their variables
  [
    set color red
    set E_a Etot / round ntot                                ;;The initial energy is random, below energy-birth
    setxy random-xcor random-ycor                  ;;Random initial location
    set w_h random-float 1                         ;;Random internal motivations, below 1
    set w_m random-float 1
  ]
  show sum [E_a] of turtles
end
@#$#@#$#@
GRAPHICS-WINDOW
452
103
821
473
-1
-1
10.94
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
9
364
181
397
K
K
0
10
4.0
0.01
1
NIL
HORIZONTAL

SLIDER
10
134
181
167
n_0
n_0
0
100
144.0
1
1
NIL
HORIZONTAL

BUTTON
15
48
92
81
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
95
47
187
80
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
15
13
92
46
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
188
165
360
198
r
r
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
9
199
181
232
D
D
0
1
0.01
0.01
1
NIL
HORIZONTAL

PLOT
1020
172
1220
322
resource
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [resource] of patches"

SLIDER
9
298
181
331
E_m
E_m
0
1
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
188
198
360
231
R_max
R_max
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
10
166
181
199
c
c
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
187
297
359
330
v_d
v_d
0
10
2.0
0.1
1
NIL
HORIZONTAL

PLOT
820
24
1020
174
w_m
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot mean [w_m] of turtles]"

PLOT
1020
24
1220
174
w_h
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot mean [w_h] of turtles]"

PLOT
820
172
1020
322
nagents
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles"

SLIDER
9
331
181
364
E_move
E_move
0
1
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
187
329
359
362
z
z
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
187
231
360
264
R_unc
R_unc
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
9
265
181
298
E_h
E_h
0
1
0.27
0.01
1
NIL
HORIZONTAL

SLIDER
187
264
359
297
v_b
v_b
0
20
2.0
1
1
NIL
HORIZONTAL

SLIDER
9
232
181
265
E_b
E_b
0
10
5.0
0.1
1
NIL
HORIZONTAL

SLIDER
188
132
360
165
R_0
R_0
0
1
0.69
0.1
1
NIL
HORIZONTAL

BUTTON
190
47
329
80
NIL
default-parameters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
820
322
1020
472
inactive -> inactive
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [inactive_previous? = 1] [plot count turtles with [inactive? = 1 and inactive_previous? = 1] / count turtles with [inactive_previous? = 1]]"

PLOT
1020
322
1220
472
inactive -> move
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [inactive_previous? = 1] [plot count turtles with [moved? = 1 and inactive_previous? = 1] / count turtles with [inactive_previous? = 1]]"

PLOT
1220
322
1420
472
inactive -> harvest
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [inactive_previous? = 1] [plot count turtles with [harvested? = 1 and inactive_previous? = 1] / count turtles with [inactive_previous? = 1]]"

PLOT
820
472
1020
622
move -> inactive
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [moved_previous? = 1] [plot count turtles with [inactive? = 1 and moved_previous? = 1] / count turtles with [moved_previous? = 1]]"

PLOT
1020
472
1220
622
move -> move
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [moved_previous? = 1] [plot count turtles with [moved? = 1 and moved_previous? = 1] / count turtles with [moved_previous? = 1]]"

PLOT
1220
472
1420
622
move -> harvest
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [moved_previous? = 1] [plot count turtles with [harvested? = 1 and moved_previous? = 1] / count turtles with [moved_previous? = 1]] "

PLOT
1220
622
1420
772
harvest -> harvest
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [harvested_previous? = 1] [plot count turtles with [harvested? = 1 and harvested_previous? = 1] / count turtles with [harvested_previous? = 1]]"

PLOT
1020
622
1220
772
harvest -> move
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [harvested_previous? = 1] [plot count turtles with [moved? = 1 and harvested_previous? = 1] / count turtles with [harvested_previous? = 1]]"

PLOT
820
622
1020
772
harvest -> inactive
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [harvested_previous? = 1] [plot count turtles with [inactive? = 1 and harvested_previous? = 1] / count turtles with [harvested_previous? = 1]]"

MONITOR
1222
20
1905
65
NIL
(count turtles with [harvested? = 1] + count turtles with [moved? = 1] + count turtles with [inactive? = 1])/ count turtles
17
1
11

PLOT
1418
172
1618
322
Mean energy
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot mean [E_a] of turtles]"

PLOT
1619
321
1819
471
harvested?
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [harvested? = 1]"

PLOT
1619
471
1819
621
moved?
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [moved? = 1] "

PLOT
1619
620
1819
770
inactive?
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [inactive? = 1]"

PLOT
420
472
620
622
phi
NIL
NIL
0.0
10.0
0.0
1.0E-5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if E_pre > 0 [plot Delta_R / (E_pre * R_pre)]"

PLOT
222
472
422
622
resource
NIL
NIL
0.0
4.0
0.0
10.0
true
false
"set-histogram-num-bars 10\nset-plot-pen-mode 1" "\n"
PENS
"default" 1.0 0 -16777216 true "" "histogram [resource] of patches"

PLOT
620
472
820
622
Total Harvest
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot delta_R"

PLOT
1419
322
1619
472
Proportion Inactive
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot count turtles with [inactive? = 1] / count turtles]"

PLOT
1420
472
1620
622
Proportion Moved
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot count turtles with [moved? = 1] / count turtles]"

PLOT
1420
622
1620
772
Proportion Harvested
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot count turtles with [harvested? = 1] / count turtles]"

PLOT
620
622
820
772
Mean harvest
NIL
NIL
0.0
10.0
0.0
0.5
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles with [harvested? = 1] [plot delta_R / count turtles with [harvested? = 1]]"

PLOT
1220
172
1420
322
Energy
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if any? turtles [plot sum [E_a] of turtles]"

SWITCH
63
446
184
479
adaptation?
adaptation?
0
1
-1000

PLOT
1618
173
1818
323
E_d
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot E_d"

PLOT
421
621
621
771
resource-dif
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot resource-dif"

PLOT
221
621
421
771
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot K * resource-grow-old * exp(r * deltat)/(K + resource-grow-old * (exp(r * deltat) - 1))"

@#$#@#$#@
## WHAT IS IT?

The model simulates agents competing for a common resource, which grows on and diffuses between patches. The agents convert the resource to internal energy, which is used to pay energy maintenance, as well as to perform the basic actions of moving and harvesting. Agents die when they cannot pay their energy maintenance, and generate offspring when their internal energy is high. Offspring inherit their parent's characteristics, with a small random deviation. This inheritance can lead to adaptation due to natural selection. 

## HOW IT WORKS

Each time-step, agents can either harvest from their present location, move to a neighbouring location, or remain inactive. Harvesting and moving cost energy, and reamining inactive allows the agent to preserve energy. Agents thus make trade-offs between performing actions and preserving energy. 

## HOW TO USE IT

The 'default parameters' is used to set the model to the standard parameter settings. For a detailed description of the effects of each parameter we refer to the full model description, available on openABM, or in the reference below.  

## THINGS TO NOTICE

On long simulation times, a process of natural selection can change the distributions of the agent parameters w_move and w_harvest. The direction of this adaptation depends on the values of the model parameters. 

## CREDITS AND REFERENCES

Guus ten Broeke, George van Voorn and Arend Ligtenbergb(2016). Which Sensitivity Analysis Method Should I Use for My Agent-Based Model? Journal of Artificial Societies and Social Simulation 19 (1) 5 <http://jasss.soc.surrey.ac.uk/19/1/5.html>
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="OFATK_ABM" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="4000"/>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>count turtles</metric>
    <metric>mean [E_a] of turtles</metric>
    <metric>sum [resource] of patches</metric>
    <metric>count turtles with [harvested? = 1]</metric>
    <metric>count turtles with [moved? = 1]</metric>
    <metric>count turtles with [inactive? = 1 ]</metric>
    <enumeratedValueSet variable="K">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
      <value value="40"/>
      <value value="50"/>
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
      <value value="90"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="nominal" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>count turtles</metric>
    <metric>mean [E_a] of turtles</metric>
    <metric>sum [resource] of patches</metric>
    <metric>n_tot</metric>
    <metric>n_h</metric>
    <metric>n_m</metric>
    <metric>n_i</metric>
    <metric>E_d</metric>
    <metric>E_d2</metric>
    <metric>resource-dif</metric>
  </experiment>
  <experiment name="nominal_notransient" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="2000"/>
    <exitCondition>count turtles = 0</exitCondition>
    <metric>count turtles</metric>
    <metric>mean [E_a] of turtles</metric>
    <metric>sum [resource] of patches</metric>
    <metric>n_tot</metric>
    <metric>n_h</metric>
    <metric>n_m</metric>
    <metric>n_i</metric>
    <metric>E_d</metric>
    <metric>E_d2</metric>
    <metric>resource-dif</metric>
    <enumeratedValueSet variable="n_0">
      <value value="144"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R_0">
      <value value="0.69"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
