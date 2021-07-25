;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COGNITIVE CONTAGION MODEL
;; Author: Nick Rabb (nicholas.rabb@tufts.edu)
;;
;; This simulation space contains code to generate several different contagion models
;; across different network topologies and propagation types. It implements the simple,
;; complex, and cognitive contagion models; several contagion functions; and various
;; parameterizations of graph topologies to test against.
;;
;; It also has implementations of different message set capabilities, read from external files,
;; to run consistent message simulations. Moreover, graphs can be exported to text files that can
;; be read in to ensure consistency across simulations.
;;
;; This simulation frequently interfaces with external Python scripts, so there are a handful of
;; helper functions in the project to assist with data conversion & parsing. There are also functions
;; useful for post-simulation data analysis, as well as BehaviorSpace pipelines to run experiments.
;;
;; TODO:
;; - Move agent memory to numpy data structure for speed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [py]

globals [
  ;; Messaging
  selected-turtle
  mag-g
  cur-message-id
  messages-over-time
  citizen-priors
  citizen-malleables

  ;; For experiments
  contagion-dir
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

  ;; Python imports and setup
  setup-py

  ;; Set the priors and malleables for each citizen
  set citizen-priors []
  set citizen-malleables [ "Attributes.A" ]

  ask patches [
    set pcolor white
  ]

  ifelse not load-graph? [
    create-agents
    connect-agents
    connect-media
  ] [
    read-graph
  ]

  ;; Load message data sets to be used by the influencer agents
  set messages-over-time load-messages-over-time (word messages-data-path "/" message-file ".json")

  ;; Layout turtles
  let max_turtle max-one-of turtles [ count social-friend-neighbors ]
  if graph-type = "erdos-renyi" [
    ask turtles with [ count social-friend-neighbors = 0 ] [ setxy random-xcor random-ycor ]
    repeat 120 [ layout-spring turtles social-friends 0.3 10 2 ]
  ]
  if graph-type = "mag" [
    repeat 120 [ layout-spring turtles social-friends 0.6 10 10 ]
  ]
  if graph-type = "watts-strogatz" [
    layout-circle sort citizens 12
    repeat 2 [ layout-spring citizens social-friends 0.3 10 1 ]
  ]
  if graph-type = "barabasi-albert" [
    layout-radial citizens social-friends max_turtle
    layout-spring citizens social-friends 0.3 10 1
  ]

  layout

  reset-ticks
end

;; Set up all the relevant Python scripts
to setup-py
  py:setup "python"
  py:run "import sys"
  py:run "import os"
  py:run "import kronecker as kron"
  py:run "from data import *"
  py:run (word "NUM_BELIEF_BUCKETS = " belief-resolution)
  py:run "from messaging import *"
  py:run "import mag as MAG"
  py:run "from nlogo_graphs import *"
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
;    set size 0.5
    set size 1
    setxy random-xcor random-ycor
  ]
end

to create-citizenz
  let id 0
  repeat N [
    create-citizen-dist id
    set id id + 1
  ]
end

to create-media
  if media-agents? [
      ; Disbelief
;      create-medias 1 [
;        set media-attrs [ ["A" 0] ]
;        set cur-message-id 0
;        set messages-sent []
;        setxy -4 0
;        set color red
;        set idee "DIS"
;      ]

    ;  ; Uncertainty
    ;  create-medias 1 [
    ;    set media-attrs [ ["A" 3] ]
    ;    set cur-message-id 0
    ;    set messages-sent []
    ;    setxy -3 0
    ;    set color violet
    ;    set idee "UNC"
    ;  ]

    ; Belief
    create-medias 1 [
      set media-attrs [ ["A" 6] ]
      set cur-message-id 0
      set messages-sent []
      setxy -4 1
      set color blue
      set idee "BEL"
    ]
  ]
end

;; Connect the agents in the simulation based on the graph type selected.
to connect-agents
  let G -1
  if graph-type = "erdos-renyi" [
    set G er-graph N erdos-renyi-p
  ]
  if graph-type = "watts-strogatz" [
    set G ws-graph N watts-strogatz-k watts-strogatz-p
  ]
  if graph-type = "barabasi-albert" [
    set G ba-graph N ba-m
  ]
  if graph-type = "mag" [
    set G mag N (list-as-py-array (sort citizen-malleables) false) mag-style
;    show [dict-value brain "A"] of citizens
    let i 0

    ;; Here, we have to reset the agent beliefs to be what they were in the MAG algorithm, otherwise
    ;; the edge connections don't make sense.
    repeat length (dict-value G "L") [
      let cit-attrs (item i (dict-value G "L"))
      let j 0

      ;; We update beliefs in the sorted order of malleables then priors because that's how L is
      ;; generated in the MAG algorithm.
      foreach (sort citizen-malleables) [ attr ->
        ask citizens with [(dict-value brain "ID") = i] [
          set brain update-agent-belief brain attr (item j cit-attrs)
        ]
        set j j + 1
      ]
      foreach (sort citizen-priors) [ attr ->
        ask citizens with [(dict-value brain "ID") = i] [
          set brain update-agent-belief brain attr (item j cit-attrs)
        ]
        set j j + 1
      ]
      set i i + 1
    ]
;    show [sort (list (dict-value brain "ID") (dict-value brain "A"))] of citizens
  ]

  let edges (dict-value G "edges")
  foreach edges [ ed ->
    let end-1 (item 0 ed)
    let end-2 (item 1 ed)
    let cit1 citizen end-1
    let cit2 citizen end-2
;    show (word "Linking " cit1 "(" (dict-value [brain] of cit1 "A") ") and " cit2 "(" (dict-value [brain] of cit2 "A") ")")
    ask citizen end-1 [ create-social-friend-with citizen end-2 ]
  ]
end

to connect-media
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

to setup-belief-plot

end

;;;;;;;;;;;;;;;;;
;; BEHAVIORSPACE SIMULATION
;; PROCS
;;;;;;;;;;;;;;;;;

;; For runs of the cognitive contagion simulations, set function parameters according to the type of
;; function being used.
;; NOTE: These are only used with BehaviorSpace simulations!!
to set-cognitive-contagion-params
  if cognitive-fn = "linear-gullible" [
    set cognitive-scalar? true
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-scalar 0
    set cognitive-translate 1
  ]
  if cognitive-fn = "linear-mid" [
    set cognitive-scalar? true
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-scalar 1
    set cognitive-translate 1
  ]
  if cognitive-fn = "linear-stubborn" [
    set cognitive-scalar? true
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-translate 10
    set cognitive-scalar 20
  ]
  if cognitive-fn = "threshold-gullible" [
    set cognitive-scalar? false
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-translate 6
  ]
  if cognitive-fn = "threshold-mid" [
    set cognitive-scalar? false
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-translate 3
  ]
  if cognitive-fn = "threshold-stubborn" [
    set cognitive-scalar? false
    set cognitive-exponent? false
    set cognitive-translate? true
    set cognitive-translate 1
  ]
  ;; Threshold t
  let t 1
  if cognitive-fn = "sigmoid-gullible" [
    set t 6
    set cognitive-scalar? false
    set cognitive-exponent? true
    set cognitive-translate? true
    set cognitive-exponent 1
    set cognitive-translate t + 1
  ]
  if cognitive-fn = "sigmoid-mid" [
    set t 2
    set cognitive-scalar? false
    set cognitive-exponent? true
    set cognitive-translate? true
    set cognitive-exponent 2
    set cognitive-translate t + 1
  ]
  if cognitive-fn = "sigmoid-stubborn" [
    set t 1
    set cognitive-scalar? false
    set cognitive-exponent? true
    set cognitive-translate? true
    set cognitive-exponent 4
    set cognitive-translate t + 1
  ]
end

;; Calculate the distances between the brain of cit and its neighbors.
;;
;; @param cit - The citizen to calculate the distance for.
;; @reports A list of distances between agent brains of cit and its neighbors.
to-report neighbor-distance [ cit ]
  let cit-distances []
  ask cit [
    let ego-brain brain
    ask social-friend-neighbors [
      set cit-distances lput (dist-between-agent-brains brain ego-brain) cit-distances
    ]
  ]
  report cit-distances
end

;; Calculate the average of all average citizen distances. This can be
;; used as a makeshift measure of homophily in the graph.
;;
;; @reports The average citizen distance across the entire graph.
to-report avg-citizen-distance
  let distance-n []
  ask citizens [
    let cit-distances neighbor-distance self

    ;; Some nodes may be disconnected
    if length cit-distances > 0 [
      set distance-n lput (mean cit-distances) distance-n
    ]
  ]
  report list (mean distance-n) (variance distance-n)
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
  if media-agents? [
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
  ]
  if contagion-on? [
    ;; In the case where we do not have influencer agents, simply do a contagion from the agent perspective
    ask citizens [
      let c self
      ask social-friend-neighbors [
        receive-message self c (agent-brain-malleable-values c) 0
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

;; Update any display properties of agents
to update-citizen
  if show-citizen-political? [ give-self-ip-color ]
end

;; Initiate the sending of a message from influencer agent m to its subscribers.
;;
;; @param m - The influencer agent to send the message.
;; @param message - The message to send.
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

;; Have a citizen agent receive a message: hear it, either believe it or not, and subsequently either
;; share it or not.
;;
;; @param cit - The citizen agent who is receiving the message.
;; @param sender - The originator of the message (CURRENTLY NOT USED -- SHOULD BE REMOVED)
;; @param message - The message itself.
;; @param message-id - The unique ID of the message (used so the citizen agent does not duplicate shares)
to receive-message [ cit sender message message-id ]
  ask cit [
    if not (heard-message? self ticks message-id) [
      hear-message self message-id message

      if spread-type = "cognitive" [
        let p 0
        let scalar 1
        let expon 1
        let trans 0
        let dist (dist-to-agent-brain brain message)

        if cognitive-scalar? [ set scalar cognitive-scalar ]
        if cognitive-exponent? [ set expon cognitive-exponent ]
        if cognitive-translate? [ set trans cognitive-translate ]

        ;; Good values for linear:
        if member? "linear" cognitive-fn [ set p 1 / (trans + (scalar * dist) ^ expon) ]

        ;; Good values for sigmoid: expon = -4, trans = -5 (works like old threshold function)
        if member? "sigmoid" cognitive-fn [ set p (1 / (1 + (exp (expon * (dist - trans))))) ]
;        show (word "dist: " dist)
;        show (word self ": " (dict-value brain "A") " " message " (p=" p ")")

        if member? "threshold" cognitive-fn [
          ifelse dist <= trans [ set p 1 ] [ set p 0 ]
        ]

        ;; Whether or not to believe the message
        let roll random-float 1
        if roll <= p [
;          show (word "believed with p" p " and roll " roll)
          set brain (believe-message-py brain message)
          believe-message self message-id message

          ask social-friend-neighbors [
            receive-message self cit message message-id
          ]
        ]
        update-citizen
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

      if spread-type = "complex" [
        let believing-neighbors 0
        ask social-friend-neighbors [
          let believes true
          foreach message [ m ->
            let attr (item 0 m)
            let val (item 1 m)
            set believes (believes and (dict-value brain attr = val))
          ]
          if believes [
            set believing-neighbors believing-neighbors + 1
          ]
        ]
;        show (word "Citizen " cit "has ratio " (believing-neighbors / length sort social-friend-neighbors))
        if (believing-neighbors / length sort social-friend-neighbors) >= complex-spread-ratio [
;          show (word "Citizen " cit " believing with ratio " (believing-neighbors / length sort social-friend-neighbors))
          set brain (believe-message-py brain message)
          believe-message self message-id message
          ;; Unsure if this sharing behavior is correct...
          ask social-friend-neighbors [
            receive-message self cit message message-id
          ]
        ]
      ]
    ]
  ]
end

;; Have a citizen agent believe a message and update its cognitive model. This also records
;; that the agent believed message message-id at the current tick.
;;
;; @param cit - The citizen to believe the message.
;; @param message-id - The id of the message.
;; @param message - The message itself.
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

;; Return whether or not a citizen agent has already heard a given message id
;; at tick tix.
;;
;; @param cit - The citizen to check.
;; @param tix - The tick number to check against.
;; @param message-id - The message ID to check for.
;; @reports a boolean whether or not the citizen has already heard that message at tick tix.
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

;; Have a citizen agent hear a message and record that they've done so (so they don't
;; interact with the same message again).
;;
;; @param cit - The citizen to hear the message.
;; @param message-id - The ID of the message to record with the current tick.
;; @param message - The message itself.
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

;; Save the current state of the graph to a file (whose path is specified in the simulation interface).
;; This will save all the agents with their state variables, the social connections between citizen agents,
;; and the influencer agents state and subscribers.
to save-graph
  ;; TODO: Find some way to get the prior & malleable attributes into a list rather than hardcoding
  let cit-ip ([(list self (dict-value brain "A") (dict-value brain "ID"))] of citizens)
  let cit-social [[self] of both-ends] of social-friends
  let media-ip ([(list self (dict-value media-attrs "A"))] of medias)
  let media-sub [[self] of both-ends] of subscribers
  py:run (word "save_graph('" save-graph-path "','" cit-ip "','" cit-social "','" media-ip "','" media-sub "')")
end

;; Read a graph back in from a data file (specified by the load-graph-path variable in the interface) and
;; construct the model appropriately.
to read-graph
  let graph py:runresult(word "read_graph('" load-graph-path "')")
  let citizenz item 0 graph
  let citizens-conns item 1 graph
  let mediaz item 2 graph
  let media-subs item 3 graph

  ;; id, a
  foreach citizenz [ c ->
    let id item 0 c
    let a item 1 c
    create-citizen id [] (list a)
  ]

  ;; Fudging this for the time-being since we're creating the same media every time
  create-media
  foreach citizens-conns [ c ->
    let c1 read-from-string (item 0 c)
    let c2 read-from-string (item 1 c)
    ask citizen c1 [ create-social-friend-with citizen c2 ]
  ]

  ;; Fudging media connections too since epsilon may not want to change between runs
  connect-media
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
;  show (round (255 / belief-resolution) * a)
  let bel-color []
  set bel-color lput (255 - (round ((255 / (belief-resolution - 1)) * a))) bel-color
  set bel-color lput 0 bel-color
  set bel-color lput (round ((255 / (belief-resolution - 1)) * a)) bel-color
  set color bel-color

  ;; Attribute A color
;  if a = 0 [ set color (extract-rgb 12) ] ; dark red
;  if a = 1 [ set color (extract-rgb 14) ] ; red
;  if a = 2 [ set color (extract-rgb 16) ] ; light red
;  if a = 3 [ set color (extract-rgb 115) ] ; violet
;  if a = 4 [ set color (extract-rgb 106) ] ; light blue
;  if a = 5 [ set color (extract-rgb 104) ] ; blue
;  if a = 6 [ set color (extract-rgb 102) ] ; dark blue
end

to give-link-ip-color [ l ]
  ask l [
    give-self-link-ip-color
  ]
end

to give-self-link-ip-color
  let c1 [color] of end1
  let c2 [color] of end2
  let opacity 100
  ifelse c1 = c2 [
    ifelse length c1 = 4 [
      set color (replace-item 3 c1 opacity)
    ] [
      set color (lput opacity c1)
    ]
  ] [
    set color lput opacity (extract-rgb gray)
  ]
end

;; Lay out the simulation display based on the properties set in the simulation interface.
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

;;;;;;;;;;;;;;;
; PROCS TO MATCH
; PY MESSAGING FILE
;;;;;;;;;;;;;;;

;; NOTE: For procedures that simply report back what comes from a python function, please refer
;; to the python function itself for function details.

to-report load-messages-over-time [ path ]
  report py:runresult(
    word "read_message_over_time_data('" path "')"
  )
end

to-report sample-attr-dist [ attr ]
  report py:runresult(
    word "random_dist_sample(" attr "," belief-resolution ")"
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
    word "create_agent_brain(" id "," (list-as-py-array prior-attrs false) "," (list-as-py-array malleable-attrs false) "," (list-as-py-array prior-vals false) ", " (list-as-py-array malleable-vals false) ",'" brain-type "',1, " threshold ",1)"
  )
end

;; Change a belief in the agent brain structure.
;; @param agent-brain - The [brain] variable of the citizen agent type.
;; @param attr - The attribute to change.
;; @param value - The new value to update it to.
to-report update-agent-belief [ agent-brain attr value ]
  report py:runresult(
    (word "update_agent_belief(" (agent-brain-as-py-dict agent-brain) "," attr "," value ")")
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
;  show (word "believe_message(" (agent-brain-as-py-dict agent-brain) ", " (list-as-py-dict message true false) ", '" spread-type "','" brain-type "')")
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

;; Get the distance between two agents' brains.
;; @param agent1-brain - The first agent's brain.
;; @param agent2-brain - The second agent's brain.
to-report dist-between-agent-brains [ agent1-brain agent2-brain ]
  report py:runresult(
    word "dist_between_agent_brains(" (agent-brain-as-py-dict agent1-brain) "," (agent-brain-as-py-dict agent2-brain) ")"
  )
end

to-report weighted-dist-to-agent-brain [ agent-brain message ]
  report py:runresult(
    word "weighted_dist_to_agent_brain(" (agent-brain-as-py-dict agent-brain) "," (list-as-py-dict message true false) "," cognitive-scalar ")"
  )
end

to-report baseline-infected-size
  report py:runresult("baseline_infected_size")
end

to-report infectiousness-duration
  report py:runresult("infectiousness_duration")
end

;; Create an Erdos-Renyi graph with the NetworkX package in python
;; @param en - The number of nodes for the graph (since N is a global variable)
;; @param p - The probability of two random nodes connecting.
;; @reports A dictionary [ ['nodes' nodes] ['edges' edges] ] where nodes is a list
;; of single values, and edges is a list of two-element lists (indicating nodes).
to-report er-graph [en p]
  report py:runresult((word "ER_graph(" en "," p ")"))
end

;; Create a Watts-Strogatz graph with the NetworkX package in python
;; @param en - The number of nodes for the graph (since N is a global variable)
;; @param k - The number of neighbors to initially connect to.
;; @param p - The probability of an edge rewiring.
;; @reports A dictionary [ ['nodes' nodes] ['edges' edges] ] where nodes is a list
;; of single values, and edges is a list of two-element lists (indicating nodes).
to-report ws-graph [en k p]
  report py:runresult((word "WS_graph(" en "," k "," p ")"))
end

;; Create a Barabasi-Albert graph with the NetworkX package in python
;; @param en - The number of nodes for the graph (since N is a global variable)
;; @param m - The number of edges to connect with when a node is added.
;; @reports A dictionary [ ['nodes' nodes] ['edges' edges] ] where nodes is a list
;; of single values, and edges is a list of two-element lists (indicating nodes).
to-report ba-graph [en m]
  report py:runresult((word "BA_graph(" en "," m ")"))
end

;; Run a MAG function in the python script.
;; @param en - The number of nodes for the graph (since N is a global variable)
;; @param attrs - A list of attributes to construct the graph from - these will designate
;; attribute affinity matrices defined in the data.py file to use for edge probabilities.
;; @param style - A connection style to use if no more specific setup is designated.
;; @param belief-resolution - A parameterized setting to denote how finely to break up belief scales.
to-report mag [ en attrs style ]
  report py:runresult(
    (word "MAG_graph(" en "," attrs ",'" style "'," belief-resolution ")")
  )
end

;; Connect a MAG graph based on values in the global mag-g variable (those values
;; are probabilities that two nodes in a graph will connect).
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

;; Create a graph of agents and links with the help of the NetworkX python package. This procedure
;; is not parameterized, but rather takes the citizens and social edges in the existing network.
;;
;; @reports A NetworkX graph object (which ends up being a NetLogo dictionary of nodes and edges).
to-report nx-graph
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  report py:runresult(
    (word "nlogo_graph_to_nx(" citizen-arr "," edge-arr ")")
  )
end

;; Calculate paths between influencers and target agents given a message and a threshold. This will return paths
;; that only contain agents of threshold distance from the message.
;;
;; @param influencer - The influencer agent (w_0 of the path)
;; @param target - The target agent (w_k of the path)
;; @param message - The message being compared to each agent in the paths
;; @param t - The threshold of distance between agent brain and message to use for path counts
;; @reports A list of paths only containing agents within threshold distance of the message.
to-report influencer-distance-paths [ influencer target message t ]
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let subscribers-arr list-as-py-array (sort [subscribers] of influencer) true
  let message-dict list-as-py-dict message true false
;  report (word "influencer_paths_within_distance(" citizen-arr "," edge-arr "," subscribers-arr ",'" target "'," message-dict "," t ")")
  report py:runresult(
    (word "influencer_paths_within_distance(" citizen-arr "," edge-arr "," subscribers-arr ",'" target "'," message-dict "," t ")")
  )
end

;;;;;;;;;;;;;;;
; HELPER PROCS
;;;;;;;;;;;;;;;

to-report array_shape [g]
  report py:runresult(
    word "kron.np.array(" g ").shape[0]"
  )
end

;; Get the value of an attribute from the Attributes enumeration in Python.
;;
;; @param attr - The variable name of the attribute.
;; @param val - The value of the attribute to fetch.
;; @reports The integer value associated with the enumeration.
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

to-report agent-brain-malleable-values [ agent ]
  let b [brain] of agent
  let malleables (dict-value b "malleable")
  report filter [ bel -> member? (item 0 bel) malleables ] [brain] of agent
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

;; Limits a value between a min and a max.
;; @param val - The value.
;; @param lower - The lower bound.
;; @param upper - The upper bound.
;; @reports The value squeezed between the two bounds.
to-report squeeze [ val lower upper ]
  report (max (list (min list val upper) lower))
end

;; @reports a date/time string that is safe for file names
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

;;;;;;;;;;;;;;;;;;
;; PROCS TO HELP
;; WITH PYTHON CONVERSION
;;;;;;;;;;;;;;;;;;

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
@#$#@#$#@
GRAPHICS-WINDOW
1162
13
1791
643
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
21
55
84
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

BUTTON
172
56
279
90
Reset Python
setup-py
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
21
98
84
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
728
392
1125
585
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
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -1 (belief-resolution + 1)\n\nhistogram [dict-value brain \"A\"] of citizens"

MONITOR
725
593
783
638
0
count citizens with [dict-value brain \"A\" = 0]
1
1
11

MONITOR
783
593
840
638
1
count citizens with [dict-value brain \"A\" = 1]
1
1
11

MONITOR
845
593
911
638
2
count citizens with [dict-value brain \"A\" = 2]
1
1
11

MONITOR
917
593
975
638
3
count citizens with [dict-value brain \"A\" = 3]
1
1
11

MONITOR
973
593
1031
638
4
count citizens with [dict-value brain \"A\" = 4]
1
1
11

BUTTON
95
55
158
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

SLIDER
536
742
666
775
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
177
278
349
311
epsilon
epsilon
0
10
0.0
0.1
1
NIL
HORIZONTAL

SWITCH
298
58
495
91
show-media-connections?
show-media-connections?
0
1
-1000

BUTTON
95
98
161
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

TEXTBOX
27
345
177
363
Number of citizens
11
0.0
1

TEXTBOX
177
258
343
286
Threshold to subscribe to media
11
0.0
1

TEXTBOX
25
144
175
162
Simulation Parameters
14
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
730
334
880
352
Simulation State Plots
14
0.0
1

SLIDER
253
369
425
402
N
N
0
1000
500.0
10
1
NIL
HORIZONTAL

SWITCH
502
58
676
91
show-citizen-political?
show-citizen-political?
0
1
-1000

SWITCH
300
99
467
132
show-social-friends?
show-social-friends?
0
1
-1000

TEXTBOX
728
362
878
380
Cognitive State
11
0.0
1

PLOT
723
689
992
924
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
719
658
907
681
Aggregate Charts
13
0.0
1

CHOOSER
330
793
472
838
spread-type
spread-type
"simple" "complex" "cognitive"
2

TEXTBOX
302
35
490
58
Display
11
0.0
1

SWITCH
27
370
146
403
load-graph?
load-graph?
1
1
-1000

INPUTBOX
25
409
240
469
load-graph-path
./exp1-graph.csv
1
0
String

INPUTBOX
27
475
242
535
save-graph-path
./exp1-graph.csv
1
0
String

BUTTON
173
97
270
131
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
18
792
171
837
cognitive-fn
cognitive-fn
"linear-gullible" "linear-stubborn" "linear-mid" "threshold-gullible" "threshold-mid" "threshold-stubborn" "sigmoid-gullible" "sigmoid-stubborn" "sigmoid-mid"
7

SLIDER
164
739
338
772
simple-spread-chance
simple-spread-chance
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
349
739
523
772
complex-spread-ratio
complex-spread-ratio
0
1
0.35
0.01
1
NIL
HORIZONTAL

CHOOSER
183
793
322
838
brain-type
brain-type
"discrete" "continuous"
0

SLIDER
25
175
199
208
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
209
177
526
239
sim-output-dir
D:/school/grad-school/Tufts/research/cognitive-contagion/simulation-data/
1
0
String

INPUTBOX
23
580
471
655
messages-data-path
D:/school/grad-school/Tufts/research/cognitive-contagion/messaging-data
1
0
String

TEXTBOX
23
559
238
585
Message Parameters
11
0.0
1

SLIDER
23
659
196
692
message-repeats
message-repeats
0
10
1.0
1
1
NIL
HORIZONTAL

PLOT
732
24
1106
312
percent-agent-beliefs
Steps
% of Agents
0.0
10.0
0.0
1.0
true
false
"let i 0\nrepeat belief-resolution [\n  let pen-name (word i)\n  create-temporary-plot-pen pen-name\n  set-current-plot-pen pen-name\n  \n  let bel-color []\n  set bel-color lput (255 - (round ((255 / (belief-resolution - 1)) * i))) bel-color\n  set bel-color lput 0 bel-color\n  set bel-color lput (round ((255 / (belief-resolution - 1)) * i)) bel-color\n\n  set-plot-pen-color bel-color\n\n  set i i + 1\n]" "let i 0\nrepeat belief-resolution [\n  let pen-name (word i)\n  set-current-plot-pen pen-name\n  \n  plot (count citizens with [ dict-value brain \"A\" = i ]) / (count citizens)\n\n  set i i + 1\n]"
PENS

MONITOR
1028
593
1086
638
5
count citizens with [dict-value brain \"A\" = 5]
17
1
11

MONITOR
1090
593
1148
638
6
count citizens with [dict-value brain \"A\" = 6]
17
1
11

CHOOSER
478
585
617
630
message-file
message-file
"default" "50-50" "gradual"
0

SWITCH
27
275
161
308
media-agents?
media-agents?
1
1
-1000

SLIDER
20
882
193
915
cognitive-exponent
cognitive-exponent
-10
10
4.0
1
1
NIL
HORIZONTAL

SLIDER
20
842
193
875
cognitive-scalar
cognitive-scalar
-20
20
20.0
1
1
NIL
HORIZONTAL

SWITCH
200
842
345
875
cognitive-scalar?
cognitive-scalar?
1
1
-1000

SWITCH
202
884
367
917
cognitive-exponent?
cognitive-exponent?
0
1
-1000

SLIDER
19
927
192
960
cognitive-translate
cognitive-translate
-10
10
2.0
1
1
NIL
HORIZONTAL

SWITCH
202
927
365
960
cognitive-translate?
cognitive-translate?
0
1
-1000

TEXTBOX
22
715
210
738
Contagion Parameters
11
0.0
1

CHOOSER
255
410
394
455
graph-type
graph-type
"erdos-renyi" "watts-strogatz" "barabasi-albert" "mag" "facebook"
1

SLIDER
437
369
560
402
erdos-renyi-p
erdos-renyi-p
0
1
0.05
0.01
1
NIL
HORIZONTAL

TEXTBOX
27
250
215
273
Influencer Parameters
11
0.0
1

TEXTBOX
28
324
216
347
Graph Parameters
11
0.0
1

SLIDER
564
369
698
402
watts-strogatz-p
watts-strogatz-p
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
565
409
692
442
watts-strogatz-k
watts-strogatz-k
0
N - 1
5.0
1
1
NIL
HORIZONTAL

SLIDER
435
483
560
516
ba-m
ba-m
0
20
3.0
1
1
NIL
HORIZONTAL

TEXTBOX
437
460
625
483
Barabasi-Albert (ba)
11
0.0
1

CHOOSER
564
472
703
517
mag-style
mag-style
"default" "homophilic" "heterophilic"
1

SWITCH
23
738
156
771
contagion-on?
contagion-on?
0
1
-1000

SLIDER
497
99
670
133
belief-resolution
belief-resolution
0
100
7.0
1
1
NIL
HORIZONTAL

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
  <experiment name="simple-complex-experiment" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe))
set contagion-dir (word run-dir "/" spread-type "/" message-file)
py:run (word "if not os.path.isdir('" run-dir "'): os.mkdir('" run-dir "')")
py:run (word "if not os.path.isdir('" run-dir "/" spread-type "'): os.mkdir('" run-dir "/" spread-type "')")
py:run (word "if not os.path.isdir('" contagion-dir "'): os.mkdir('" contagion-dir "')")</setup>
    <go>go</go>
    <final>let rand random 10000
export-world (word contagion-dir "/" rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" rand "_percent-agent-beliefs.csv")</final>
    <metric>count citizens</metric>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-file">
      <value value="&quot;default&quot;"/>
      <value value="&quot;50-50&quot;"/>
      <value value="&quot;gradual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="cognitive-exp" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup
set-cognitive-contagion-params
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe))
set contagion-dir (word run-dir "/" spread-type "/" message-file "/" cognitive-fn)
py:run (word "if not os.path.isdir('" run-dir "'): os.mkdir('" run-dir "')")
py:run (word "if not os.path.isdir('" run-dir "/" spread-type "'): os.mkdir('" run-dir "/" spread-type "')")
py:run (word "if not os.path.isdir('" run-dir "/" spread-type "/" message-file "'): os.mkdir('" run-dir "/" spread-type "/" message-file "')")
py:run (word "if not os.path.isdir('" contagion-dir "'): os.mkdir('" contagion-dir "')")</setup>
    <go>go</go>
    <final>let rand random 10000
export-world (word contagion-dir "/" rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" rand "_percent-agent-beliefs.csv")</final>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-gullible&quot;"/>
      <value value="&quot;sigmoid-mid&quot;"/>
      <value value="&quot;sigmoid-stubborn&quot;"/>
      <value value="&quot;linear-mid&quot;"/>
      <value value="&quot;linear-gullible&quot;"/>
      <value value="&quot;linear-stubborn&quot;"/>
      <value value="&quot;threshold-mid&quot;"/>
      <value value="&quot;threshold-gullible&quot;"/>
      <value value="&quot;threshold-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-file">
      <value value="&quot;default&quot;"/>
      <value value="&quot;50-50&quot;"/>
      <value value="&quot;gradual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="graph-exp" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup
set-cognitive-contagion-params
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe))
set contagion-dir (word run-dir "/" brain-type "/" spread-type "/" message-file "/" cognitive-fn "/" graph-type)
py:run (word "if not os.path.isdir('" run-dir "'): os.mkdir('" run-dir "')")
py:run (word "if not os.path.isdir('" run-dir "/" brain-type "'): os.mkdir('" run-dir "/" brain-type "')")
py:run (word "if not os.path.isdir('" run-dir "/" brain-type  "/" spread-type "'): os.mkdir('" run-dir "/" brain-type "/" spread-type "')")
py:run (word "if not os.path.isdir('" run-dir "/" brain-type  "/" spread-type "/" message-file "'): os.mkdir('" run-dir "/" brain-type "/" spread-type "/" message-file "')")
py:run (word "if not os.path.isdir('" run-dir "/" brain-type  "/" spread-type "/" message-file "/" cognitive-fn "'): os.mkdir('" run-dir "/" brain-type "/" spread-type "/" message-file "/" cognitive-fn "')")
py:run (word "if not os.path.isdir('" contagion-dir "'): os.mkdir('" contagion-dir "')")</setup>
    <go>go</go>
    <final>let rand random 10000
export-world (word contagion-dir "/" rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" rand "_percent-agent-beliefs.csv")</final>
    <metric>count citizens</metric>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
      <value value="&quot;continuous&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
      <value value="&quot;complex&quot;"/>
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-file">
      <value value="&quot;default&quot;"/>
      <value value="&quot;50-50&quot;"/>
      <value value="&quot;gradual&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
      <value value="&quot;watts-strogatz&quot;"/>
      <value value="&quot;barabasi-albert&quot;"/>
      <value value="&quot;mag&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="influence-likelihood-paths" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>show "wow"</go>
    <timeLimit steps="1"/>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 0]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 1]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 2]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 3]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 4]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 5]) [["A" 6]] 2)</metric>
    <metric>length (influencer-distance-paths (media 500) (one-of citizens with [dict-value brain "A" = 6]) [["A" 6]] 2)</metric>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
      <value value="&quot;barabasi-albert&quot;"/>
      <value value="&quot;watts-strogatz&quot;"/>
      <value value="&quot;mag&quot;"/>
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
