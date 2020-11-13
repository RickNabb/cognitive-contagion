extensions [py]

globals [
  ;; Messaging
  selected-turtle
  mag-g
  cur-message-id
  messages-over-time
  citizen-priors
  citizen-malleables

  ;; SIR
  Nk
  kronecker_g
]

citizens-own [
  ;; Messaging
  brain
  messages-heard
  messages-believed
]

medias-own [
  media-attrs
  messages-sent
  idee
]

breed [ medias media ]
breed [ citizens citizen ]

undirected-link-breed [ social-friends social-friend ]
directed-link-breed [ subscribers subscriber ]

;;;;;;;;;;;;;;;;;
;; SETUP PROCS
;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set-default-shape turtles "circle"
  set-default-shape medias "box"
  py:setup "python"
  py:run "import sys"
  py:run "import os"
  py:run "import kronecker as kron"
  py:run "from data import *"
  py:run "from messaging import *"
  py:run "import mag as MAG"

  set Nk array_shape seed ^ k
  set citizen-priors []
  set citizen-malleables [ "Attributes.A" ]

  ask patches [
    set pcolor white
  ]

  ifelse not load-graph? [
    create-agents

    set kronecker_g kronecker_pow seed k

    set mag-g mag
    connect_mag
    connect_media
  ] [
    read-graph
  ]

  set messages-over-time load-messages-over-time messages-data-path

  let max_turtle max-one-of turtles [ count social-friend-neighbors ]
  repeat 120 [ layout-spring turtles social-friends 0.3 10 1 ]
  ask turtles with [ count social-friend-neighbors = 0 ] [ setxy random-xcor random-ycor ]

  layout

  reset-ticks
end

to create-agents
  create-citizenz
  create-media
end

to create-citizen-dist [ id ]
  let prior-vals (map sample-attr-dist citizen-priors)
  let malleable-vals (map sample-attr-dist citizen-malleables)
  create-citizen id prior-vals malleable-vals
end

to create-citizen [ id prior-vals malleable-vals ]
  let b create-agent-brain id citizen-priors citizen-malleables prior-vals malleable-vals
  create-citizens 1 [
    set brain b
    set messages-heard []
    set messages-believed []
    set size 1
    setxy random-xcor random-ycor
  ]
end

to create-citizenz
  let id 0
  repeat Nk [
    create-citizen-dist id
    set id id + 1
  ]
end

to create-media
  ; Disbelief
  create-medias 1 [
    set media-attrs [ ["A" -3] ]
    set cur-message-id 0
    set messages-sent []
    setxy -4 0
    set color red
    set idee "DIS"
  ]

;  ; Uncertainty
;  create-medias 1 [
;    set media-attrs [ ["A" 0] ]
;    set cur-message-id 0
;    set messages-sent []
;    setxy -3 0
;    set color violet
;    set idee "UNC"
;  ]

  ; Belief
  create-medias 1 [
    set media-attrs [ ["A" 3] ]
    set cur-message-id 0
    set messages-sent []
    setxy -4 1
    set color blue
    set idee "BEL"
  ]
end

;;;;;;;;;;;;;;;;;
;; SIMULATION PROCS
;;;;;;;;;;;;;;;;;

to go
  ifelse ticks <= tick-end [
    step
  ] [ stop ]
end

to step
  ;; Have media companies create messages

  let messages (dict-value messages-over-time (word ticks))
  foreach messages [ media-messages ->
    let media-idee item 0 media-messages
    foreach (item 1 media-messages) [ m ->
      ask medias with [ idee = media-idee] [
        let a (dict-value m "A")
        repeat message-repeats [
          send-media-message-to-subscribers self (list (list "A" a))
        ]
      ]
    ]
  ]

  layout

  tick
end

to update-agents
  ask citizens [
    update-citizen
  ]
end

to update-citizen
  if show-citizen-political? [ give-self-ip-color ]
end

to send-media-message-to-subscribers [ m message ]
  ask m [
    let mid cur-message-id
    set messages-sent (lput (list mid message) messages-sent)
    ask my-subscribers [
      ask other-end [
        receive-message self m message mid
      ]
    ]
    set cur-message-id (cur-message-id + 1)
  ]
end

to receive-message [ cit sender message message-id ]
  ask cit [
    if not (heard-message? self ticks message-id) [
      hear-message self message-id message

      if spread-type = "distance" [
        let dist-to-brain 1000
        if dist-fn = "l2" [ set dist-to-brain dist-to-agent-brain brain message ]
        if dist-fn = "l2 weighted" [ set dist-to-brain weighted-dist-to-agent-brain brain message ]

        ;; Whether or not to believe the message
        if dist-to-brain <= (dict-value brain "beta") [
          set brain (believe-message-py brain message)
          believe-message self message-id message
        ]
        update-citizen

        ;; Whether or not to share with neighbors
        if dist-to-brain <= (dict-value brain "alpha") [
          ask social-friend-neighbors [
            receive-message self cit message message-id
          ]
        ]
      ]

      if spread-type = "simple" [
        let roll random-float 1
        if roll <= simple-spread-chance [
;          show(word "believing " message-id)
          set brain (believe-message-py brain message)
          believe-message self message-id message
          ask social-friend-neighbors [
            receive-message self cit message message-id
          ]
        ]
      ]

      ;; For this one, have an agent believe the media wholesale, or otherwise keep track of how
      ;; many friends tell you a message. If a requisite ratio of them do, then believe it
      if spread-type = "complex" [
        ifelse is-media? sender [
          set brain (believe-message-py brain message)
          believe-message self message-id message
          ask social-friend-neighbors [
            receive-message self cit message message-id
          ]
        ] [
          let unique-heard-from 0
          let messages-at-tick (dict-value messages-heard ticks)
        ]
      ]
    ]
  ]
end

to believe-message [ cit message-id message ]
  ask cit [
    let i (index-of-dict-entry messages-believed ticks)
    ifelse i != -1 [
      let messages-at-tick (item i messages-believed)
      let message-ids (item 1 messages-at-tick)
      set message-ids (lput message-id message-ids)
      set messages-at-tick (replace-item 1 messages-at-tick message-ids)
      set messages-believed (replace-item i messages-believed messages-at-tick)
    ] [
      set messages-believed (lput (list ticks (list message-id)) messages-believed)
    ]
  ]
end

to-report heard-message? [ cit tix message-id ]
  let heard? false
  ask cit [
    let messages-at-tick (dict-value messages-heard tix)
    if messages-at-tick != -1 [
      set heard? (member? message-id messages-at-tick)
    ]
  ]
  report heard?
end

to hear-message [ cit message-id message ]
  ask cit [
    let i (index-of-dict-entry messages-heard ticks)
    ifelse i != -1 [
      let messages-at-tick (item i messages-heard)
      let message-ids (item 1 messages-at-tick)
      set message-ids (lput message-id message-ids)
      set messages-at-tick (replace-item 1 messages-at-tick message-ids)
      set messages-heard (replace-item i messages-heard messages-at-tick)
    ] [
      set messages-heard (lput (list ticks (list message-id)) messages-heard )
    ]
  ]
end

to test
  py:setup "python"
  py:run "from messaging import *"
end

;;;;;;;;;;;;;
; I/O PROCEDURES
;;;;;;;;;;;;;

to save-graph
  let cit-ip ([(list self (dict-value brain "P") (dict-value brain "I") (dict-value brain "ID"))] of citizens)
  let cit-social [[self] of both-ends] of social-friends
  let media-ip ([(list self (dict-value media-attrs "P") (dict-value media-attrs "I"))] of medias)
  let media-sub [[self] of both-ends] of subscribers
  py:run (word "save_graph('" save-graph-path "','" cit-ip "','" cit-social "','" media-ip "','" media-sub "')")
end

to read-graph
  let graph py:runresult(word "read_graph('" load-graph-path "')")
  let citizenz item 0 graph
  let citizens-conns item 1 graph
  let mediaz item 2 graph
  let media-subs item 3 graph

  ;; id, p, i
  foreach citizenz [ c ->
    let id item 0 c
    let p item 1 c
    let i item 2 c
    create-citizen id i p
  ]

  ;; Fudging this for the time-being since we're creating the same media every time
  create-media
  foreach citizens-conns [ c ->
    let c1 read-from-string (item 0 c)
    let c2 read-from-string (item 1 c)
    ask citizen c1 [ create-social-friend-with citizen c2 ]
  ]

  ;; Fudging media connections too since epsilon may not want to change between runs
  connect_media
end

;;;;;;;;;;;;;
; DISPLAY PROCEDURES
;;;;;;;;;;;;;

to make-link-transparent
  ifelse is-list? color [
    ifelse length color = 4 [
      set color (replace-item 3 color 0)
    ] [
      set color (lput 0 color)
    ]
  ] [
    set color extract-rgb color
    set color (lput 0 color)
  ]
end

to make-link-visible
  ifelse is-list? color [
    ifelse length color = 4 [
      set color (replace-item 3 color 255)
    ] [
      set color (lput 255 color)
    ]
  ] [
    set color extract-rgb color
    set color (lput 255 color)
  ]
end

to give-agent-ip-color [ agent ]
  ask agent [
    give-self-ip-color
  ]
end

to give-self-ip-color
  let a (dict-value brain "A")

  ;; Attribute A color
  if a = -3 [ set color (extract-rgb 12) ]
  if a = -2 [ set color (extract-rgb 14) ]
  if a = -1 [ set color (extract-rgb 16) ]
  if a = 0 [ set color (extract-rgb violet) ]
  if a = 1 [ set color (extract-rgb 106) ]
  if a = 2 [ set color (extract-rgb 104) ]
  if a = 3 [ set color (extract-rgb 102) ]
end

to give-link-ip-color [ l ]
  ask l [
    give-self-link-ip-color
  ]
end

to give-self-link-ip-color
  set color (extract-rgb gray)
  if [shape] of end1 = [shape] of end2 [
    set color (extract-rgb green)
  ]
  if [color] of end1 = [color] of end2 [
    set color [color] of end1
  ]
end

to layout
  update-agents

  ifelse show-media-connections? [ ask subscribers [ make-link-visible ] ] [ ask subscribers [ make-link-transparent ] ]
  ifelse show-social-friends? [
    ask social-friends [
      make-link-visible
      give-self-link-ip-color
    ]
  ] [ ask social-friends [ make-link-transparent ] ]
end

;;;;;;;;;;;;;;;;;
;; KRONECKER PROCS
;;;;;;;;;;;;;;;;;

;;; Take the Kronecker power of a given graph represented by matrix
;;; g to the power k.
;;;
;;; param g: Must be a string representation of a python array
;;; param k: An integer to raise the graph to the Kronecker power of
to-report kronecker_pow [g pow]
  report py:runresult(
    word "kron.kronecker_pow(kron.np.array(" g "), " pow ")"
  )
end

to connect_kronecker
  let u 0
  let v 0
  foreach kronecker_g [ row ->
     set v 0
     foreach row [ el ->
      let rand random-float 1
      if (el > rand) and (u != v) [
        ;show(word "Linking turtle " u " with " v)
;        ask citizen u [ create-friend-with citizen v ]
;        ask citizen u [ create-seeing-with citizen v ]
      ]
      set v v + 1
    ]
    set u u + 1
  ]
end

;;;;;;;;;;;;;;;
; MAG PROCS
;;;;;;;;;;;;;;;

;;; Run a political MAG function in the python script.
to-report mag
  report py:runresult(
    word "MAG.attr_mag(" Nk "," (list-as-py-array citizen-malleables false) ")"
  )
end

to connect_mag
  let u 0
  let v 0
  foreach mag-g [ row ->
     set v 0
     foreach row [ el ->
      let rand random-float 1
      if (el > rand) and (u != v) [
        ;show(word "Linking turtle b/c el:" el " and rand " rand)
        ask turtle u [ create-social-friend-with turtle v ]
      ]
      set v v + 1
    ]
    set u u + 1
  ]
end

to connect_media
  let u 0
  ask medias [
    let m self
    ask citizens [
      let t self
      if dist-to-agent-brain brain ([media-attrs] of m) <= epsilon [
        create-subscriber-from m
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;
; PROCS TO MATCH
; PY MESSAGING FILE
;;;;;;;;;;;;;;;

to-report load-messages-over-time [ path ]
  report py:runresult(
    word "read_message_over_time_data('" path "')"
  )
end

to-report sample-attr-dist [ attr ]
  report py:runresult(
    word "random_dist_sample(" attr ")"
  )
end

to-report sample-attr-dist-given [ attr given ]
  ;show(word "random_dist_sample(" attr "," (tuple-list-as-py-dict given false false) ")")
  ;; Now it's putting quotes around the Attribute.I which should not be there... have to reconcile somehow
  report py:runresult(
    word "random_dist_sample(" attr "," (tuple-list-as-py-dict given false false) ")"
  )
end

to-report create-agent-brain [ id prior-attrs malleable-attrs prior-vals malleable-vals ]
  report py:runresult(
    word "create_agent_brain(" id "," (list-as-py-array prior-attrs false) "," (list-as-py-array malleable-attrs false) "," (list-as-py-array prior-vals false) ", " (list-as-py-array malleable-vals false) ",'" brain-type "'," beta ", " threshold "," alpha ")"
  )
end

to-report random-message [ attrs ]
  report py:runresult(
    word "random_message(" (list-as-py-array attrs false) ")"
  )
end

to-report receive-message-py [ agent-brain message ]
  ;show(agent-brain-as-py-dict agent-brain)
  ;show(list-as-py-dict message false false)
  report py:runresult(
    word "receive_message(" (agent-brain-as-py-dict agent-brain) ", " (list-as-py-dict message true false) ", " spread-type ")"
  )
end

to-report believe-message-py [ agent-brain message ]
;  show(agent-brain-as-py-dict agent-brain)
  ;show(list-as-py-dict message false false)
  report py:runresult(
    word "believe_message(" (agent-brain-as-py-dict agent-brain) ", " (list-as-py-dict message true false) ", '" spread-type "','" brain-type "')"
  )
end

to-report message-dist [ m1 m2 ]
  report py:runresult(
    word "message_distance(" (list-as-py-array m1 false) "," (list-as-py-array m2 false) ")"
  )
end

to-report weighted-message-dist [ m1 m2 m1-weights m2-weights ]
  report py:runresult(
    word "weighted_message_distance(" (list-as-py-array m1 false) "," (list-as-py-array m2 false) "," (list-as-py-array m1-weights false) "," (list-as-py-array m2-weights false) ")"
  )
end

to-report dist-to-agent-brain [ agent-brain message ]
  report py:runresult(
    word "dist_to_agent_brain(" (agent-brain-as-py-dict agent-brain) "," (list-as-py-dict message true false) ")"
  )
end

to-report weighted-dist-to-agent-brain [ agent-brain message ]
  report py:runresult(
    word "weighted_dist_to_agent_brain(" (agent-brain-as-py-dict agent-brain) "," (list-as-py-dict message true false) ",attribute_weights)"
  )
end

to-report baseline-infected-size
  report py:runresult("baseline_infected_size")
end

to-report infectiousness-duration
  report py:runresult("infectiousness_duration")
end

;;;;;;;;;;;;;;;
; HELPER PROCS
;;;;;;;;;;;;;;;

to-report array_shape [g]
  report py:runresult(
    word "kron.np.array(" g ").shape[0]"
  )
end

to-report name-of-attribute-val [ attr val ]
  report py:runresult(
    word "Attributes." attr ".value(" val ").name"
  )
end

;[["ID" 49] ["beta" 1.5] ["tokens" [["I" [["0" 0] ["1" 0] ["2" 0] ["3" 0] ["4" 0]]] ["P" [["0" 0] ["1" 0] ["2" 0]]]]] ["update_threshold" 5] ["I" 2] ["P" 0]]
to-report agent-brain-as-py-dict [ b ]
  if brain-type = "discrete" [
    ;; Do the tokens thing
    let tokens item 1 (dict-entry b "tokens")
    let subtokens [ ]
    foreach tokens [ token ->
      set subtokens (lput (multi-list-as-tuple-list token false false) subtokens)
    ]
    let token-ml (list "tokens" (list-as-py-dict subtokens true false))

    ;; Replace the tokens entry in the original list
    set b (replace-dict-item b "tokens" token-ml)
  ]

  ;; Convert to a py-dict
  report list-as-py-dict-rec b true false
end

to-report agent-brain-token-list [ agent attr ]
  let b [brain] of agent
  let tokens (dict-value b "tokens")
  let token-list []
  foreach tokens [ token ->
    if (item 0 token) = attr [
      set token-list map [ el -> item 1 el ] (item 1 token)
;      foreach (item 1 token) [ bucket ->
;        set token-list (lput (item 1 bucket) token-list)
;      ]
    ]
  ]
  report token-list
end

to-report histogrammable-brain-token-list [ agent attr ]
  let l agent-brain-token-list agent attr
  let i 0
  let hist []
  foreach l [ t ->
    repeat t [
      set hist (lput i hist)
    ]
    set i i + 1
  ]
  report hist
end

to-report replace-dict-item [ l key value ]
  let key-i 0
  let i 0
  foreach l [ el ->
    if (item 0 el) = key [
      set key-i i
    ]
    set i i + 1
  ]
  report (replace-item key-i l value)
end

to-report dict-value [ dict key ]
  foreach dict [ list-attr ->
    if item 0 list-attr = key[
      report item 1 list-attr
    ]
  ]
  report -1
end

to-report dict-entry [ dict key ]
  foreach dict [ list-attr ->
    if item 0 list-attr = key [
      report list-attr
    ]
  ]
  report -1
end

to-report index-of-dict-entry [ dict key ]
  let i 0
  foreach dict [ list-attr ->
    if item 0 list-attr = key [
      report i
    ]
    set i i + 1
  ]
  report -1
end

to-report list-as-py-array [ l val-quotes? ]
  let py-array "["
  let i 1
  foreach l [ el ->
    if val-quotes? [ set el (word "'" el "'") ]

    ifelse i = length l
    [ set py-array (word "" py-array el) ]
    [ set py-array (word "" py-array el ",") ]

    set i i + 1
  ]
  report (word py-array "]")
end

to-report list-item-as-dict-item [ el key-quotes? val-quotes? ]
  if key-quotes? and val-quotes? [ report (word "'" (item 0 el) "': '" (item 1 el) "'") ]
  if key-quotes? [ report (word "'" (item 0 el) "': " (item 1 el)) ]
  if val-quotes? [ report (word (item 0 el) ": '" (item 1 el) "'") ]
  report (word (item 0 el) ": " (item 1 el))
end

to-report multi-list-as-py-dict [ ml key-quotes? val-quotes? ]
  let attr (item 0 ml)
  let l (item 1 ml)
  let py-dict (word  "{'" attr "': { ")
  let i 1
  foreach l [ el ->
    ;show(tuple-list-as-py-dict el)
    ifelse i = length l
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) " }") ]
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) ", ") ]

    set i i + 1
    ;show(py-dict)
  ]
  report py-dict
end

to-report multi-list-as-tuple-list [ ml key-quotes? val-quotes? ]
  let attr (item 0 ml)
  let l (item 1 ml)
  let py-dict "{ "
  let i 1
  foreach l [ el ->
    ;show(tuple-list-as-py-dict el)
    ifelse i = length l
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) " }") ]
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) ", ") ]

    set i i + 1
    ;show(py-dict)
  ]
  report list attr py-dict
end

to-report list-as-py-dict [ l key-quotes? val-quotes? ]
  let py-dict "{ "
  let i 1
  foreach l [ el ->
    ;show(tuple-list-as-py-dict el)
    ifelse i = length l
    ;[ set py-dict (word py-dict (tuple-list-as-py-dict el) " }") ]
    ;[ set py-dict (word py-dict (tuple-list-as-py-dict el) ", ") ]
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) " }") ]
    [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) ", ") ]

    set i i + 1
    ;show(py-dict)
  ]
  report py-dict
end

; [ [ 'key1' val ] [ 'key2' [ 'key3' val2 ] ] [ 'key4' [ val3 val4 val5 ] ] ]
to-report list-as-py-dict-rec [ l key-quotes? val-quotes? ]
  let py-dict "{ "
  let i 1
  foreach l [ el ->
    ifelse length el = 2 and is-list? item 1 el and length item 1 el > 0 [
      ifelse is-list? (item 0 (item 1 el)) [
        let temp-item list (item 0 el) (list-as-py-dict-rec (item 1 el) key-quotes? val-quotes?)
        ifelse i = length l
        [ set py-dict (word py-dict (list-item-as-dict-item temp-item key-quotes? val-quotes?) " }") ]
        [ set py-dict (word py-dict (list-item-as-dict-item temp-item key-quotes? val-quotes?) ", ") ]
      ] [
        let temp-item list (item 0 el) (list-as-py-array item 1 el true)
        ifelse i = length l
        [ set py-dict (word py-dict (list-item-as-dict-item temp-item key-quotes? false) " }") ]
        [ set py-dict (word py-dict (list-item-as-dict-item temp-item key-quotes? false) ", ") ]
      ]
    ] [
      ifelse i = length l
      [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) " }") ]
      [ set py-dict (word py-dict (list-item-as-dict-item el key-quotes? val-quotes?) ", ") ]
    ]

    set i i + 1
    ;show(py-dict)
  ]
  report py-dict
end

to-report tuple-list-as-py-dict [ l key-quotes? val-quotes? ]
  if (length l = 2) [
    report (word "{"(list-item-as-dict-item l key-quotes? val-quotes?) "}")
  ]
  report -1
end

to-report squeeze [ val lower upper ]
  report (max (list (min list val upper) lower))
end

to-report date-time-safe
  let datetime date-and-time
  let safedatetime ""
  let i 0
  repeat length datetime [
    let char item i datetime
    if char != " " and char != ":" and char != "." [
      set safedatetime (word safedatetime char)
    ]
    set i i + 1
  ]
  report safedatetime
end
@#$#@#$#@
GRAPHICS-WINDOW
1276
20
1905
650
-1
-1
18.82
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

BUTTON
20
55
83
88
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

TEXTBOX
1918
25
2068
109
KEY:\nCircle = Very Liberal\nStar = Liberal\nTriangle = Moderate\nPentagon = Conservative\nSquare = Very Conservative
11
0.0
1

TEXTBOX
1922
135
2072
153
BLUE = Democrat
11
105.0
1

TEXTBOX
1921
150
2071
168
PURPLE = Independent
11
114.0
1

TEXTBOX
1921
168
2071
186
RED = Republican
11
15.0
1

BUTTON
172
56
235
89
NIL
test
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
20
98
83
131
Step
step
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
849
562
1246
755
A Histogram
A Value
Number of Agents
-4.0
4.0
0.0
50.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -4 4\n\nhistogram [dict-value brain \"A\"] of citizens"

MONITOR
847
763
905
808
-3
count citizens with [dict-value brain \"A\" = -3]
1
1
11

MONITOR
904
763
961
808
-2
count citizens with [dict-value brain \"A\" = -2]
1
1
11

MONITOR
967
763
1033
808
-1
count citizens with [dict-value brain \"A\" = -1]
1
1
11

MONITOR
1038
763
1088
808
0
count citizens with [dict-value brain \"A\" = 0]
1
1
11

MONITOR
1095
763
1145
808
1
count citizens with [dict-value brain \"A\" = 1]
1
1
11

BUTTON
94
55
157
88
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SWITCH
20
486
153
519
uniform-beta?
uniform-beta?
0
1
-1000

SWITCH
573
486
732
519
uniform-threshold?
uniform-threshold?
0
1
-1000

SLIDER
20
446
192
479
beta
beta
0
5
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
573
446
703
479
threshold
threshold
0
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
388
446
560
479
epsilon
epsilon
0
10
0.8
0.1
1
NIL
HORIZONTAL

SWITCH
830
52
1027
85
show-media-connections?
show-media-connections?
0
1
-1000

BUTTON
94
98
160
131
NIL
layout
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
205
446
377
479
alpha
alpha
0
5
1.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
583
52
733
70
Number of citizens
11
0.0
1

TEXTBOX
20
430
170
448
Threshold to believe messages
11
0.0
1

TEXTBOX
206
426
356
444
Threshold to share messages
11
0.0
1

TEXTBOX
390
426
556
454
Threshold to subscribe to media
11
0.0
1

TEXTBOX
20
399
170
417
Citizen Parameters
14
0.0
1

TEXTBOX
583
18
733
36
Simulation Parameters
14
0.0
1

TEXTBOX
575
423
740
451
Tokens needed to change belief
11
0.0
1

TEXTBOX
23
17
173
35
Simulation Controls
14
0.0
1

TEXTBOX
852
505
1002
523
Simulation State Plots
14
0.0
1

SLIDER
582
80
754
113
k
k
0
10
5.0
1
1
NIL
HORIZONTAL

SWITCH
1033
52
1207
85
show-citizen-political?
show-citizen-political?
0
1
-1000

SWITCH
832
93
999
126
show-social-friends?
show-social-friends?
1
1
-1000

TEXTBOX
850
532
1000
550
Cognitive State
11
0.0
1

PLOT
1933
258
2202
493
Social Friend Degree of Nodes
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
"default" 1.0 1 -16777216 true "" "set-plot-x-range 0 (max [count social-friend-neighbors] of citizens) + 1\nhistogram [count social-friend-neighbors] of citizens"

TEXTBOX
1926
215
2114
238
Aggregate Charts
13
0.0
1

TEXTBOX
26
619
214
642
Macro Parameters
10
0.0
1

CHOOSER
410
765
552
810
spread-type
spread-type
"simple" "complex" "distance"
0

TEXTBOX
833
29
1021
52
Display
11
0.0
1

SWITCH
28
652
147
685
load-graph?
load-graph?
1
1
-1000

INPUTBOX
20
709
235
769
load-graph-path
./citizen-graph-k4.csv
1
0
String

INPUTBOX
24
187
239
247
save-graph-path
./citizen-graph-k4.csv
1
0
String

BUTTON
174
100
271
134
Save Graph
save-graph
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
266
715
405
760
dist-fn
dist-fn
"l2" "l2 weighted"
0

SLIDER
23
543
197
576
simple-spread-chance
simple-spread-chance
0
1
0.04
0.01
1
NIL
HORIZONTAL

SLIDER
208
543
382
576
complex-spread-ratio
complex-spread-ratio
0
1
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
410
715
549
760
brain-type
brain-type
"discrete" "continuous"
1

SLIDER
24
140
198
173
tick-end
tick-end
30
150
99.0
1
1
NIL
HORIZONTAL

INPUTBOX
251
189
467
250
sim-output-dir
D:\\school\\grad-school\\Tufts\\research\\covid-misinfo\\simulation-data\\11-4-20\\cognitive/gradual
1
0
String

INPUTBOX
25
285
473
360
messages-data-path
D:/school/grad-school/Tufts/research/cognitive-contagion/messaging-data/all-one.json
1
0
String

TEXTBOX
24
263
239
289
Message Parameters
11
0.0
1

SLIDER
25
363
198
396
message-repeats
message-repeats
0
10
1.0
1
1
NIL
HORIZONTAL

INPUTBOX
582
125
807
215
seed
[[0.6,0.26,0.24],\n  [0.40,0.2,0.4],\n  [0.21,0.24,0.65]]
1
0
String

PLOT
847
175
1221
463
Percent of Agents with Beliefs
Steps
% of Agents
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"3" 1.0 0 -15390905 true "" "plot (count citizens with [ dict-value brain \"A\" = 3 ]) / (count citizens)"
"2" 1.0 0 -14070903 true "" "plot (count citizens with [ dict-value brain \"A\" = 2 ]) / (count citizens)"
"1" 1.0 0 -10649926 true "" "plot (count citizens with [ dict-value brain \"A\" = 1 ]) / (count citizens)"
"0" 1.0 0 -10141563 true "" "plot (count citizens with [ dict-value brain \"A\" = 0 ]) / (count citizens)"
"-1" 1.0 0 -2139308 true "" "plot (count citizens with [ dict-value brain \"A\" = -1 ]) / (count citizens)"
"-2" 1.0 0 -5298144 true "" "plot (count citizens with [ dict-value brain \"A\" = -2 ]) / (count citizens)"
"-3" 1.0 0 -10873583 true "" "plot (count citizens with [ dict-value brain \"A\" = -3 ]) / (count citizens)"

MONITOR
1150
763
1208
808
2
count citizens with [dict-value brain \"A\" = 2]
17
1
11

MONITOR
1212
763
1270
808
3
count citizens with [dict-value brain \"A\" = 3]
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="default-exp" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>let rand random 10000
export-world (word sim-output-dir "/" rand "_world.csv")
export-plot "percent_tp_gte_1" (word sim-output-dir "/" rand "_percent_tp.csv")</final>
    <metric>count citizens</metric>
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
