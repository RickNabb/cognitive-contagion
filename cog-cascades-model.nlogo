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
  beliefs-over-time
  citizen-priors
  citizen-malleables
  topics

  ;; For experiments
  contagion-dir
  behavior-rand
]

citizens-own [
  ;; Messaging
  brain
  messages-heard
  messages-believed
  ;; Trust
  agent-messages-memory
  ;; Groups
  groups
]

medias-own [
  idee
  brain
  messages-heard
  messages-believed
  messages-sent
]

breed [ medias media ]
breed [ citizens citizen ]

directed-link-breed [ social-friends social-friend ]
directed-link-breed [ subscribers subscriber ]

social-friends-own [ weight ]
subscribers-own [ weight ]

;;;;;;;;;;;;;;;;;
;; SETUP PROCS
;;;;;;;;;;;;;;;;;

to setup-test
  setup-py
  let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe) "-conditions-to-polarize-cognitive")
  let graphs-path (word run-dir "/graphs")
  let graph-file (word graphs-path "/" cognitive-translate "-" institution-tactic "-" media-ecosystem-dist "-" citizen-init-dist "-" epsilon "-" graph-type "-" ba-m "-" repetition ".csv")
  ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
    set load-graph? true
    set load-graph-path graph-file
    setup
  ] [
    set load-graph? false
    set save-graph-path graph-file
    py:run (word "create_nested_dirs('" graphs-path "')")
    setup
    save-graph
  ]
  set contagion-dir (word run-dir "/" cognitive-translate "/" institution-tactic "/" media-ecosystem-dist "/" citizen-init-dist "/" epsilon "/" graph-type "/" ba-m)
  py:run (word "create_nested_dirs('" contagion-dir "')")
end

to setup
  clear-all
  set-default-shape turtles "circle"
  set-default-shape medias "box"

  ;; Python imports and setup
  setup-py

  ;; Set the priors and malleables for each citizen
  set citizen-priors []
  set citizen-malleables [ "A" ]

  set topics [ ["Q1" ["A"]] ]

  set beliefs-over-time []

  ask patches [
    set pcolor white
  ]

  ifelse not load-graph? [
    create-agents
    connect-agents
    connect-media
  ] [
    read-graph
    if citizen-citizen-trust? [
      initialize-citizen-citizen-memory
    ]
    if citizen-media-trust? [
      initialize-citizen-media-memory
    ]
  ]

  ;; Load message data sets to be used by influencer agents
  if media-agents? and institution-tactic = "predetermined" [
    set messages-over-time load-messages-over-time messages-data-file
  ]

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
  if graph-type = "barabasi-albert" or graph-type = "kronecker" or graph-type = "ba-homophilic" [
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
;  py:run "import kronecker as kron"
;  py:run "import data_analysis"
  py:run "from utils import *"
  py:run "from messaging import *"
;  py:run "import mag as MAG"
  py:run "from nlogo_graphs import *"
  py:run "from nlogo_io import *"
  py:run "from belief_initialization import *"
end

to create-agents
  create-citizenz
  create-media
end

to create-citizenz
  let id 0
  let en 0
  ifelse graph-type != "kronecker" [
    set en N
  ] [
    set en array_shape kronecker-seed ^ kronecker-k
  ]

  if citizen-init-type = "whole-population" [
    ;; Generate list of agent initial beliefs for the specified distribution in the interface
    let prior-vals (initial-belief-dist citizens citizen-init-dist en citizen-priors)
    let malleable-vals (initial-belief-dist citizens citizen-init-dist en citizen-malleables)

    ;; Create them all at once to speed up the process
    create-citizens en [
      set id read-from-string (substring (word "" self) 9 (position ")" (word "" self)))
      set brain create-agent-brain id citizen-priors citizen-malleables (item id prior-vals) (item id malleable-vals)
      set messages-heard []
      set messages-believed []
      set agent-messages-memory []
      set groups []

      set size 0.5
      setxy random-xcor random-ycor
      set id id + 1
    ]
  ]
  if citizen-init-type = "per-group" [
    let beliefs citizen-priors
    let per-group-mapping py:runresult(word "read_json_file('" citizen-init-per-group-file "')")
    let base-id 0
    let total-n 0
    foreach per-group-mapping [ group ->
      let group-name item 0 group
      let group-properties item 1 group

      set en dict-value group-properties "n"
      set total-n total-n + en
      let init-per-belief dict-value group-properties "beliefs"

      ; A map of [ bel: list-of-belief-vals ] for size group-en
      let dists-per-belief []

      foreach init-per-belief [ belief-init ->
        let belief-name item 0 belief-init
        let init-type item 1 belief-init

        let dist-type item 0 init-type

        let belief-list []
        if dist-type = "uniform" [
          set belief-list uniform-dist (belief-resolution) en
        ]
        if dist-type = "normal" [
          set belief-list normal-dist (belief-resolution - 1) (item 1 init-type) (item 2 init-type) en
        ]
        if dist-type = "matched_normal" [
          set belief-list matched-normal-dist (belief-resolution - 1) (item 1 init-type) (item 2 init-type) en (item 3 init-type) (item 4 init-type) (item 5 init-type)
        ]
        if dist-type = "polarized" [
          let l normal-dist (belief-resolution - 1) 1 0.5 (ceiling (en / 2))
          let r normal-dist (belief-resolution - 1) (belief-resolution - 2) 0.5 (floor (en / 2))
          foreach r [ el -> set l (lput el l) ]
          set belief-list l
        ]
        set dists-per-belief (lput (list group-name belief-list) dists-per-belief)
      ]

      let per-cit-beliefs (distribute-beliefs-per-citizen dists-per-belief)
      create-citizens en [
        set id read-from-string (substring (word "" self) 9 (position ")" (word "" self)))
        set brain create-agent-brain id citizen-priors citizen-malleables [] (item (id - base-id) per-cit-beliefs)
        set messages-heard []
        set messages-believed []
        set agent-messages-memory []
        set groups (list group-name)

        set size 0.5
        setxy random-xcor random-ycor
      ]
      set base-id (base-id + en)
    ]
    set N total-n
  ]
end

to initialize-citizen-citizen-memory
  let all-beliefs (set-merge-lists (list citizen-priors citizen-malleables))
  ask citizens [
    foreach (sort out-social-friend-neighbors) [ neighbor ->
      add-agent-memory self neighbor
      initialize-memory-from-init-belief self neighbor
    ]
  ]
end

to initialize-citizen-media-memory
  let all-beliefs (set-merge-lists (list citizen-priors citizen-malleables))
  ask citizens [
    foreach (sort subscriber-neighbors) [ neighbor ->
      add-agent-memory self neighbor
      initialize-memory-from-init-belief self neighbor
    ]
  ]
end

to initialize-memory-from-init-belief [ cit oth ]
  let all-beliefs (set-merge-lists (list citizen-priors citizen-malleables))
  ask cit [
    repeat cit-memory-len [
      ; TODO: Should this just be a message encoding all belief values rather than all belief
      ; values worth of single messages?
      foreach all-beliefs [ bel ->
        let message (list (list bel (dict-value ([brain] of oth) bel)))
        add-message-to-memory self oth message
      ]
    ]
  ]
end

;; Generate an initial belief distribution for the whole network of en agents, drawn
;; from a distribution of dist-type for each belief in beliefs.
;; @param dist-type - A string denoting what type of distribution to draw from.
;; @param en - The number of agents.
;; @param beliefs - How many belief propositions to draw for.
;; NOTE: This assumes that each proposition in beliefs is drawn from the same
;; kind of distribution.
to-report initial-belief-dist [ agent-set dist-type en beliefs ]
  if agent-set = citizens [
    if dist-type = "uniform" [
      report uniform-dist-multiple (belief-resolution) en (length beliefs)
    ]
    if dist-type = "normal" [
      report normal-dist-multiple (belief-resolution - 1) cit-init-normal-mean cit-init-normal-std en (length beliefs)
    ]
    if dist-type = "polarized" [
      let l normal-dist-multiple (belief-resolution - 1) 1 0.5 (ceiling (en / 2)) (length beliefs)
      let r normal-dist-multiple (belief-resolution - 1) (belief-resolution - 2) 0.5 (floor (en / 2)) (length beliefs)
      foreach r [ el -> set l (lput el l) ]
      report l
    ]
  ]
  if agent-set = medias [
    if dist-type = "uniform" [
      report uniform-dist-multiple (belief-resolution) media-ecosystem-n (length beliefs)
    ]
    if dist-type = "normal" [
      report normal-dist-multiple (belief-resolution - 1) media-dist-normal-mean media-dist-normal-std media-ecosystem-n (length beliefs)
    ]
    if dist-type = "polarized" [
      let l normal-dist-multiple (belief-resolution - 1) 1 1 (round (media-ecosystem-n / 2)) (length beliefs)
      let r normal-dist-multiple (belief-resolution - 1) (belief-resolution - 2) 1 (round (media-ecosystem-n / 2)) (length beliefs)
      foreach r [ el -> set l (lput el l) ]
      report l
    ]
  ]
end

to create-media
  if media-agents? [
    if media-ecosystem = "distribution" [
      ;; Generate list of agent initial beliefs for the specified distribution in the interface
      let prior-vals (initial-belief-dist medias media-ecosystem-dist media-ecosystem-n citizen-priors)
      let malleable-vals (initial-belief-dist medias media-ecosystem-dist media-ecosystem-n citizen-malleables)

      let id 0
      create-medias media-ecosystem-n [
        set brain create-agent-brain (N + id) citizen-priors citizen-malleables (item id prior-vals) (item id malleable-vals)
        set messages-heard []
        set messages-believed []
        set messages-sent []

        setxy random-xcor random-ycor
        set color green
        set id id + 1
      ]
    ]
    if media-ecosystem = "predetermined" [
      let ecosystem load-media-ecosystem media-ecosystem-file
      foreach ecosystem [ m ->
        create-medias 1 [
          set idee (dict-value m "id")
          set brain (dict-value m "brain")
          set messages-heard []
          set messages-believed []
          set messages-sent []

          setxy random-xcor random-ycor
          set color green
        ]
      ]
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
  if graph-type = "ba-homophilic" [
    set G ba-graph-homophilic N ba-m
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
  if graph-type = "kronecker" [
    set G kronecker kronecker-seed kronecker-k
  ]

  ; Create links
  let edges (dict-value G "edges")
  foreach edges [ ed ->
    let end-1 (item 0 ed)
    let end-2 (item 1 ed)
    let cit1 citizen end-1
    let cit2 citizen end-2
;    show (word "Linking " cit1 "(" (dict-value [brain] of cit1 "A") ") and " cit2 "(" (dict-value [brain] of cit2 "A") ")")
    ask citizen end-1 [ create-social-friend-to citizen end-2 [ set weight citizen-citizen-influence ] ]
  ]

  ; Remove isolates
  ask citizens with [ empty? sort social-friend-neighbors ] [ show "removed isolate" ]
end

to connect-media
  ifelse media-connection-type = "per-group" [
    let media-group-mapping py:runresult(word "read_json_file('" media-connection-file "')")
    foreach media-group-mapping [ media-entry ->
      let media-id item 0 media-entry
      let groupz item 1 media-entry

      let med item 0 sort medias with [ idee = media-id ]

      foreach groupz [ group ->
        let group-cits citizens with [ member? group groups ]
        ask group-cits [
          create-subscriber-from med [ set weight media-citizen-influence ]
        ]
      ]
    ]
  ][
    if citizen-citizen-trust? [
      initialize-citizen-citizen-memory
    ]
    ifelse citizen-media-trust? [
      connect-media-initial-zeta
      initialize-citizen-media-memory
    ] [
      connect-media-epsilon
    ]
  ]
end

to connect-media-epsilon
  ask medias [
    let m self
    ask citizens [
      let dist dist-between-agent-brains brain ([brain] of m)
      if dist <= epsilon [
        create-subscriber-from m [ set weight media-citizen-influence ]
      ]
    ]
  ]
end

to connect-media-initial-zeta
  let zeta-conns cit-media-initial-connections-by-zeta
  let j (length sort citizens)
  foreach zeta-conns [ sub-list ->
    let m (media j)
    let i 0
    foreach sub-list [ sub ->
      let c citizen i
      if sub = 1 [ ask m [ create-subscriber-to c ] ]
      set i i + 1
    ]
    set j j + 1
  ]
end

to connect-media-zeta
  let zeta-conns cit-media-connections-by-zeta

  ask medias [ ask subscribers [ die ] ]

  let i 0
  foreach zeta-conns [ sub-list ->
    let cit (citizen i)
    let j (length sort citizens)
    foreach sub-list [ sub ->
      let m (media j)
      if sub = 1 [ ask cit [ create-subscriber-from m ] ]
      set j j + 1
    ]
    set i i + 1
  ]
end

to connect-citizens-zeta
  let zeta-conns cit-cit-connections-by-zeta

  ask citizens [ ask social-friends [ die ] ]

  let i 0
  foreach zeta-conns [ soc-friends ->
    let cit1 (citizen i)
    let j 0
    foreach soc-friends [ friend ->
      let cit2 (citizen j)
      if friend = 1 [ ask cit1 [
        create-social-friend-from cit2
        create-social-friend-to cit2
      ] ]
      set j j + 1
    ]
    set i i + 1
  ]
end

to connect-all-media
  ask medias [
    let m self
    ask citizens [
      create-subscriber-from m
    ]
  ]
end

to prune-media-connections
  let all-beliefs (set-merge-lists (list citizen-priors citizen-malleables))
  let message-with-all-topics (map [ bel -> (list bel 1) ] all-beliefs)
  ask medias [
    let m self
    ask citizens [
      update-trust-connection self m message-with-all-topics
    ]
  ]
end

to setup-belief-plot

end

;;;;;;;;;;;;;;;;;
;; BEHAVIORSPACE SIMULATION
;; PROCS
;;;;;;;;;;;;;;;;;

;; Calculate the distances between the brain of cit and its neighbors.
;;
;; @param cit - The citizen to calculate the distance for.
;; @reports A list of distances between agent brains of cit and its neighbors.
to-report neighbor-distance [ cit ]
  let cit-distances []
  ask cit [
    let ego-brain brain
    ask out-social-friend-neighbors [
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
  set beliefs-over-time (lput (list ticks (map [ cit -> agent-brain-beliefs-as-dict cit ] (sort citizens))) beliefs-over-time)
  if media-agents? [
;    show "Sending messages from institutions"
    institutions-send-messages
    if citizen-media-trust? and matrix-trust-conn? [
;      show "Reconnecting by zeta"
      connect-media-zeta
    ]
    if citizen-citizen-trust? and matrix-trust-conn? [
;      show "Reconnecting by zeta"
      connect-citizens-zeta
    ]
  ]
  if contagion-on? [
    ;; In the case where we do not have influencer agents, simply do a contagion from the agent perspective
    ask citizens with [ not is-agent-brain-empty? self ] [
      let c self
      ask out-link-neighbors [
       ; show (word "Citizen " self " receiving message " (agent-brain-malleable-values c) " from citizen " c)
;        receive-message -1 self c (agent-brain-malleable-values c) 0
        spread-message -1 self c (agent-brain-malleable-values c) 0
      ]
    ]
  ]

  layout

  tick
end

to institutions-send-messages
  ;; Have media companies create from files
  if institution-tactic = "predetermined" [
    institutions-predetermined-tactic
  ]
  if institution-tactic = "broadcast-brain" [
    institutions-broadcast-brain
  ]
  if institution-tactic = "appeal-mean" [
    institutions-appeal-mean
  ]
  if institution-tactic = "appeal-median" [
    institutions-appeal-median
  ]
  if institution-tactic = "appeal-mode" [
    institutions-appeal-mode
  ]
end

to institutions-predetermined-tactic
  let messages (dict-value messages-over-time (word ticks))
  if messages != -1 [
    foreach messages [ media-messages ->
      let media-idee item 0 media-messages
      foreach (item 1 media-messages) [ m ->
        ;; TODO: Fix this so it's not hardcoded to just be "A"
        ask medias with [ idee = media-idee ] [
          let a (dict-value m "A")
          repeat message-repeats [
;            show (word "Media " media-idee " sent " m)
            send-media-message-to-subscribers self (list (list "A" a))
          ]
        ]
      ]
    ]
  ]
end

to institutions-broadcast-brain
  ask medias [
    let message-val dict-value brain "A"
    repeat message-repeats [
      send-media-message-to-subscribers self (list (list "A" message-val))
    ]
  ]
end

to institutions-appeal-mean
  ask medias [
    let beliefs [ dict-value brain "A" ] of subscriber-neighbors
    if not empty? beliefs [
      let message-val (round (mean beliefs))
      repeat message-repeats [
        send-media-message-to-subscribers self (list (list "A" message-val))
      ]
    ]
  ]
end

to institutions-appeal-median
  ask medias [
    let beliefs [ dict-value brain "A" ] of subscriber-neighbors
    if not empty? beliefs [
      let message-val (round (median beliefs))
      repeat message-repeats [
        send-media-message-to-subscribers self (list (list "A" message-val))
      ]
    ]
  ]
end

to institutions-appeal-mode
  ask medias [
    let beliefs [ dict-value brain "A" ] of subscriber-neighbors
    if not empty? beliefs [
      let message-val (round (item 0 (modes beliefs)))
      repeat message-repeats [
        send-media-message-to-subscribers self (list (list "A" message-val))
      ]
    ]
  ]
end

to update-agents
  ask citizens [
    update-citizen
  ]
  ask medias [
    update-media
  ]
end

;; Update any display properties of agents
to update-citizen
  if show-citizen-political? [ give-self-ip-color ]
end

;; Update any display properties of media
to update-media
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

    ifelse matrix-spread? [
      let spread-res spread-from-media m message 0.01
;      show "Got spread-res"
      let heard (dict-value spread-res "heard")
      let believed (dict-value spread-res "believed")
      let heard-from (dict-value spread-res "heard_from")
      ; TODO: This does not seem to be working
;      show (word "sending message from media " m)
;      show (word "heard: " heard)
;      show (word "heard from: " heard-from)

      let i 0
;      show "Starting update"
      repeat length heard [
        let cit (citizen i)
        if (item i heard) = 1 [
          hear-message cit mid message
          if citizen-media-trust? [
            add-agent-memory cit m
            add-message-to-memory cit m message
            if not matrix-trust-conn? [
              update-trust-connection cit m message
            ]
          ]
          let cits-heard-from []
          ifelse cits-hear-from-path? [
            ; This is the old, more computationally expensive version
            set cits-heard-from (map [ c -> read-from-string c ] (dict-value (spread-path heard-from i) "nodes"))

            ; Limit this to just the immediate prior sender -- a chain of length 2 for
            ; computational efficiency
;            let path (spread-path heard-from i)
;            let heard-immediate (dict-value heard-from (word i))
;            foreach heard-immediate [ c ->
;              set cits-heard-from (lput c cits-heard-from)
;              let heard-before (dict-value (dict-value path "edges") (word c))
;              set cits-heard-from (lput (read-from-string heard-before) cits-heard-from)
;            ]
          ] [
            set cits-heard-from (dict-value heard-from (word i))
          ]
          foreach cits-heard-from [ c ->
            let cit-sender (citizen c)
;            show heard-from
;            show (word "citizen " cit " heard from " cit-sender)
;            show (word "the whole path was " (spread-path heard-from i))
            if (cit-sender != cit) and citizen-citizen-trust? [
              add-agent-memory cit cit-sender
              add-message-to-memory cit cit-sender message
              if not matrix-trust-conn? [
                update-trust-connection cit cit-sender message
              ]
            ]
          ]
        ]
        if (item i believed) = 1 [ believe-message cit mid message ]

        ask cit [ update-citizen ]

        set i i + 1
      ]
;      show "Finished update"
    ] [
      ask subscriber-neighbors [
;        if not (heard-message? self ticks mid) [
;          hear-message self mid message
;          if (believe-message? self message) [
;            believe-message self mid message
;            spread-message m self m message mid
;          ]
;        ]
        receive-message m self [] message mid
      ]
    ]
    set cur-message-id (cur-message-id + 1)
  ]
end

to-report cognitive-contagion-p-no-cit [ braine message ]
  let p 0
  let scalar 1
  let expon 1
  let trans 0
  let dist (dist-to-agent-brain braine message)

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
  report p
end

to-report cognitive-contagion-p [ cit message ]
  let p 0
  ask cit [
    set p cognitive-contagion-p-no-cit brain message
  ]
  report p
end

to-report believe-message? [ cit message ]
  if spread-type = "cognitive" [
    let p cognitive-contagion-p self message
    ;; Whether or not to believe the message
    let roll random-float 1
    if roll <= p [
      report true
    ]
  ]
  if spread-type = "simple" [
    let roll random-float 1
    if roll <= simple-spread-chance [
      report true
    ]
  ]
  if spread-type = "complex" [
    let believing-neighbors 0
    ask out-social-friend-neighbors [
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
    if (believing-neighbors / length sort out-social-friend-neighbors) >= complex-spread-ratio [
      report true
    ]
  ]
  report false
end

to spread-message [ source cit sender message message-id ]
  ; Add message to memory for trust
  if citizen-media-trust? [
    add-agent-memory self source
    add-message-to-memory self source message
  ]
  if citizen-citizen-trust? [
    add-agent-memory self sender
    add-message-to-memory self sender message
  ]
  let next-believers one-spread-iteration cit message
  foreach next-believers [ cit-id ->
    let next-cit (citizen cit-id)
    if not (heard-message? next-cit ticks message-id) [
      hear-message next-cit message-id message
      believe-message next-cit message-id message
      ask next-cit [ update-citizen ]

    ; Update connections
;      if not matrix-trust-conn? [
;      if citizen-media-trust? [ update-trust-connection self source message ]
;      if citizen-citizen-trust? [ update-trust-connection self sender message ]
;      ]
      spread-message source next-cit cit message message-id
    ]
  ]
end

;; Have a citizen agent receive a message: hear it, either believe it or not, and subsequently either
;; share it or not.
;;
;; @param source - The original institutional agent who sent the message
;; @param cit - The citizen agent who is receiving the message.
;; @param senders - The chain of those who have shared this message
;; @param message - The message itself.
;; @param message-id - The unique ID of the message (used so the citizen agent does not duplicate shares)
to receive-message [ source cit senders message message-id ]
  ask cit [
    if not (heard-message? self ticks message-id) [
      hear-message self message-id message
;      show (word "message " message-id " heard from source " source " from sender " sender)

      if citizen-media-trust? [
        add-agent-memory self source
        add-message-to-memory self source message
      ]
      if citizen-citizen-trust? [
        foreach senders [ sender ->
          add-agent-memory self sender
          add-message-to-memory self sender message
        ]
      ]

      if spread-type = "cognitive" [

        let p cognitive-contagion-p self message
        ;; Whether or not to believe the message
        let roll random-float 1
        if roll <= p [
;          show (word "believed with p" p " and roll " roll)
          set brain (believe-message-py brain message)
          believe-message self message-id message

          ask out-social-friend-neighbors [
            let new-senders []
            ifelse cits-hear-from-path? [
              set new-senders (lput cit senders)
            ] [
              set new-senders (list cit)
            ]
            receive-message source self new-senders message message-id
          ]
        ]
        update-citizen
        if not matrix-trust-conn? [
          if citizen-media-trust? [ update-trust-connection self source message ]
          if citizen-citizen-trust? [
            foreach senders [ sender ->
              update-trust-connection self sender message
            ]
          ]
        ]
      ]

      if spread-type = "simple" [
        let roll random-float 1
        if roll <= simple-spread-chance [
          ;show(word "believing " message-id)
          ;show (believe-message-py brain message)
          set brain (believe-message-py brain message)
          believe-message self message-id message
          ask out-social-friend-neighbors [
            receive-message source self cit message message-id
          ]
        ]
      ]

      if spread-type = "complex" [
        let believing-neighbors 0
        ask out-social-friend-neighbors [
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
        ; NOTE: This is a choice made with the non-bidirectional graph to count *all* the social neighbors
        ; while determining whether or not to spread complex contagion -- the interpretation is that an ego
        ; agent would also be gauging support from people who spread to them, who "they follow"
        if (believing-neighbors / length sort social-friend-neighbors) >= complex-spread-ratio [
;          show (word "Citizen " cit " believing with ratio " (believing-neighbors / length sort social-friend-neighbors))
          set brain (believe-message-py brain message)
          believe-message self message-id message
          ;; Unsure if this sharing behavior is correct...
          ask out-social-friend-neighbors [
            receive-message source self cit message message-id
          ]
        ]
      ]
    ]
  ]
end

;; Have a citizen commit a media message to memory
;;
;; @param cit - The citizen
;; @param source - The original media source of the message
;; @param message - The content of the message
to add-message-to-memory [ cit source message ]
  ask cit [
    let memory-by-belief dict-value agent-messages-memory source
    foreach message [ bel-val ->
      let belief-key (item 0 bel-val)
      let belief dict-value message belief-key
      let memory dict-value memory-by-belief belief-key

      set memory (lput belief memory)
      if length memory > cit-memory-len [
        set memory but-first memory
      ]
      set memory-by-belief (replace-dict-item memory-by-belief belief-key memory)
      set agent-messages-memory (replace-dict-item agent-messages-memory source memory-by-belief)
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
;    st brain (believe-message-py brain message)
    foreach message [ entry ->
      let bel-key item 0 entry
      let bel-val item 1 entry
      ask cit [
        set brain (replace-dict-item brain bel-key bel-val)
      ]
    ]
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

;; Connect or disconnect from another agent based off of
;; trust in that citizen and the global threshold set by zeta;
;; trust conditioned by topics that the message touched on.
;;
;; @param cit - The citizen to update links.
;; @param oth - The other agent (citizen or media) to check trust against (the one who
;; just sent a message to the citizen).
;; @param message - The message a citizen just received.
to update-trust-connection [ cit oth message ]
  let message-topics topics-in-message message

  ;; TODO: This is an assumption that connections do not differ based on topic,
  ;; but rather that distrust in one topic may make or break a connection even
  ;; given trust in another topic
  let trust-by-topic map [ topic -> citizen-trust-in-other cit oth topic ] message-topics
  let trust mean trust-by-topic
;  show trust-by-topic
  ask cit [
    if is-media? oth [
      if trust < zeta-media and in-link-neighbor? oth [
        ask in-subscriber-from oth [ die ]
      ]
      if trust >= zeta-media and not in-link-neighbor? oth [
        create-subscriber-from oth
      ]
    ]
    if is-citizen? oth [
      if trust < zeta-cit and social-friend-neighbor? oth [
        ask social-friend-with oth [ die ]
      ]
      if trust >= zeta-cit and not in-link-neighbor? oth [
        create-social-friend-to oth
        create-social-friend-from oth
      ]
    ]
  ]
end

;; Add a memory slot for a given agent's messages for citizen cit
;;
;; @param cit - The citizen to add a memory slot to
;; @param agente - The agent to add memory tracking for
to add-agent-memory [ cit agente ]
  ask cit [
    let all-beliefs (set-merge-lists (list citizen-priors citizen-malleables))
    let entry (dict-value agent-messages-memory agente)
    if entry = -1 [
      if is-citizen? agente [
;        show (word "adding new agent memory for " agente)
;        show (word "prior memory " agent-messages-memory)
;        show (word "neighborhood " sort out-social-friend-neighbors)
      ]
      set agent-messages-memory (lput (list agente (map [ b -> list b [] ] all-beliefs)) agent-messages-memory)
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
  let cit-ip []
  let cit-social []
  foreach sort citizens [ cit ->
    ; For now, only use item 0 of 'groups,' but modify in the future to support multiple groups
    set cit-ip (lput (list cit (dict-value ([brain] of cit) "A") (dict-value ([brain] of cit) "ID") (item 0 ([groups] of cit))) cit-ip)
    foreach sort [ out-social-friend-neighbors ] of cit [ nb ->
      set cit-social (lput (list cit nb) cit-social)
    ]
  ]
  let media-ip []
  let media-sub []
  foreach sort medias [ m ->
    set media-ip (lput (list m ([idee] of m) (dict-value ([brain] of m) "A")) media-ip)
    foreach sort [ subscriber-neighbors ] of m [ sub ->
      set media-sub (lput (list m sub) media-sub)
    ]
  ]
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
  ;; TODO: Change this from being hard-coded for one belief "A" to being general
  create-citizens (length citizenz) [
    let i read-from-string (substring (word self) 9 ((length (word self)) - 1))
    let c (item i citizenz)
    let id item 0 c
    let a read-from-string (item 1 c)
    set groups (list (item 2 c))

    set brain create-agent-brain id citizen-priors citizen-malleables [] (list a)
    set messages-heard []
    set messages-believed []
    set agent-messages-memory []

    set size 0.5
    setxy random-xcor random-ycor
    set i i + 1
  ]

  create-medias (length mediaz) [
    let i (read-from-string (substring (word self) 7 ((length (word self)) - 1))) - N
    let m (item i mediaz)
    let id item 0 m
    set idee (item 1 m)
    let a read-from-string (item 2 m)
    set brain create-agent-brain (id) citizen-priors citizen-malleables [] (list a)
    set messages-heard []
    set messages-believed []
    set messages-sent []

    setxy random-xcor random-ycor
    set color green
    set i i + 1
  ]

  foreach citizens-conns [ c ->
    let c1 read-from-string (item 0 c)
    let c2 read-from-string (item 1 c)
    ask citizen c1 [ create-social-friend-to citizen c2 [ set weight citizen-citizen-influence ] ]
  ]

  foreach media-subs [ sub ->
    let c read-from-string (item 0 sub)
    let m read-from-string (item 1 sub)
    ask media m [ create-subscriber-to citizen c [ set weight media-citizen-influence ] ]
  ]
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
  ifelse a = -1 [
    set color (extract-rgb gray)
  ] [
    let bel-color []
    set bel-color lput (squeeze (255 - (floor ((255 / (belief-resolution - 1)) * a))) 0 255) bel-color
    set bel-color lput 0 bel-color
    set bel-color lput (squeeze (floor ((255 / (belief-resolution - 1)) * a)) 0 255) bel-color
    set color bel-color
  ]
;  show (round (255 / belief-resolution) * a)


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

to-report distribute-beliefs-per-citizen [ distributions-per-belief ]
  report py:runresult(
    word "distribute_beliefs_per_citizen(" (list-as-py-dict-rec distributions-per-belief true false) "," (list-as-py-array citizen-malleables true) ")"
  )
end

;; NOTE: For procedures that simply report back what comes from a python function, please refer
;; to the python function itself for function details.

;; Return a series of samples drawn from a uniform distribution from [0, maxx]
;; where each of en samples has k entries.
;; @param maxx - The maximum to draw from.
;; @param en - The number of k entry samples to draw.
;; @param k - The number of entries per n sample.
to-report uniform-dist-multiple [ maxx en k ]
  report py:runresult(
    word "uniform_dist_multiple(" maxx "," en "," k ")"
  )
end

;; Return a series of samples drawn from a uniform distribution from [0, maxx]
;; where each of en samples has 1 entry.
;; @param maxx - The maximum to draw from.
;; @param en - The number of samples to draw.
to-report uniform-dist [ maxx en ]
  report py:runresult(
    word "uniform_dist(" maxx "," en ")"
  )
end

;; Return a series of samples drawn from a normal distribution from [0, maxx]
;; with mean mu, std sigma; where each of en samples has k entries.
;; @param maxx - The maximum to draw from.
;; @param mu - The mean of the distribution.
;; @param sigma - The std deviation of the distribution.
;; @param en - The number of k entry samples to draw.
;; @param k - The number of entries per n sample.
to-report normal-dist-multiple [ maxx mu sigma en k ]
  report py:runresult(
    word "normal_dist_multiple(" maxx "," mu "," sigma "," en "," k ")"
  )
end

;; Return a series of samples drawn from a normal distribution from [0, maxx]
;; with mean mu, std sigma; where each of en samples has k entries.
;; @param maxx - The maximum to draw from.
;; @param mu - The mean of the distribution.
;; @param sigma - The std deviation of the distribution.
;; @param en - The number of samples to draw.
to-report normal-dist [ maxx mu sigma en ]
  report py:runresult(
    word "normal_dist(" maxx "," mu "," sigma "," en ")"
  )
end

;; Return a series of samples drawn from a normal distribution from [0, maxx]
;; with mean mu, std sigma; where each of en samples has k entries. This
;; distribution is potentially re-rolled until the sample taken from it
;; has target-count values contained in target-vals within a threshold thresh.
;; This has a higher chance of guaranteeing less variant distributions, even with
;; higher sigmas.
;; @param maxx - The maximum to draw from.
;; @param mu - The mean of the distribution.
;; @param sigma - The std deviation of the distribution.
;; @param en - The number of samples to draw.
;; @param target-vals - A list of values to check counts for against target-count
;; @param target-count - The number of values in the dist that should match values
;; in target-vals
;; @param thresh - A threshold within which the distribution count and target-count
;; can be.
to-report matched-normal-dist [ maxx mu sigma en target-vals target-count thresh ]
  report py:runresult(
    word "matched_normal_dist(" maxx "," mu "," sigma "," en "," (list-as-py-array target-vals false) "," target-count "," thresh ")"
  )
end

to-report load-messages-over-time [ filename ]
;  if not file-exists? (word path "/" belief-resolution "/" media-ecosystem-file) [
;    error "Messaging directory does not exist for current resolution and ecosystem type"
;  ]
  report py:runresult(
    word "read_message_over_time_data('" filename "')"
  )
end

to-report load-media-ecosystem [ filename ]
;  if not file-exists? (word path "/" belief-resolution) [
;    error "Media ecosystem directory does not exist for current resolution"
;  ]
  report py:runresult(
    word "read_media_ecosystem_data('" filename "')"
  )
end

to-report create-agent-brain [ id prior-attrs malleable-attrs prior-vals malleable-vals ]
  report py:runresult(
    word "create_agent_brain(" id "," (list-as-py-array prior-attrs true) "," (list-as-py-array malleable-attrs true) "," (list-as-py-array prior-vals false) ", " (list-as-py-array malleable-vals false) ",'" brain-type "',1,1)"
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

;; Return citizen "cit" trust level in agent "oth" on topic "topic"
;; calculated by trust-fn. This fetches the memory matrix from the
;; citizen, conditioned by topic, of messages sent by agent oth,
;; and then feeds them through the trust function to get a result.
;;
;; @param cit - The citizen to get trust for.
;; @param oth - The other agent their trust is in.
;; @param topic - The topic to fetch memory for (containing belief propositions).
to-report citizen-trust-in-other [ cit oth topic ]
  let topic-beliefs (dict-value topics topic)
  let memory-by-belief [ dict-value agent-messages-memory oth ] of cit
  let b [ brain ] of cit
  let py-function ""
  if citizen-trust-fn = "average-bel" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "agent_trust_in_other_belief_func(" (list-as-py-dict-rec memory-by-belief true false) "," (agent-brain-as-py-dict b) "," (list-as-py-array topic-beliefs true) ", " py-function ")")

  report py:runresult(
    command
  )
end

to-report cit-media-initial-connections-by-zeta
  let cit-beliefs list-as-py-array (map [ cit -> list-as-py-dict-rec (agent-brain-beliefs-as-dict cit) true false ] (sort citizens)) false
  let media-beliefs list-as-py-array (map [ med -> list-as-py-dict-rec (agent-brain-beliefs-as-dict med) true false ] (sort medias)) false
  let py-function ""

;  show cit-beliefs
;  show media-beliefs

  if citizen-trust-fn = "average-bel" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "citizen_media_initial_connections_by_zeta(" cit-beliefs "," media-beliefs "," zeta-media "," cit-memory-len "," (list-as-py-dict-rec topics true true) "," py-function ")")

  report py:runresult(
    command
  )
end

to-report cit-media-connections-by-zeta
  let cit-beliefs list-as-py-array (map [ cit -> list-as-py-dict-rec (agent-brain-beliefs-as-dict cit) true false ] (sort citizens)) false
  let cit-memories list-as-py-array (map [ cit -> list-as-py-dict-rec ([agent-messages-memory] of cit) true false ] (sort citizens)) false
  let py-function ""

;  show cit-beliefs
;  show cit-memories

  if citizen-trust-fn = "average-bel" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "citizen_media_connections_by_zeta(" cit-beliefs "," cit-memories "," zeta-media "," cit-memory-len "," (length sort medias) "," (list-as-py-dict-rec topics true true) "," py-function ")")

  report py:runresult(
    command
  )
end

to-report cit-cit-connections-by-zeta
  let cit-beliefs list-as-py-array (map [ cit -> list-as-py-dict-rec (agent-brain-beliefs-as-dict cit) true false ] (sort citizens)) false
  let cit-memories list-as-py-array (map [ cit -> list-as-py-dict-rec ([agent-messages-memory] of cit) true false ] (sort citizens)) false
  let py-function ""

;  show cit-beliefs
;  show cit-memories

  if citizen-trust-fn = "average-bel" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "citizen_citizen_connections_by_zeta(" cit-beliefs "," cit-memories "," zeta-cit "," (list-as-py-dict-rec topics true true) "," py-function ")")

  report py:runresult(
    command
  )
end

;; Perform one iteration of spread from a given citizen with a given message.
;; This returns an array of neighbors who would believe the message with the
;; currently set spread-type function and parameters.
;;
;; @param cit - The citizen sharing a message
;; @param message - The message
to-report one-spread-iteration [ cit message ]
  let citizen-arr list-as-py-array (map [ c -> agent-brain-as-py-dict [brain] of citizen c ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let py-function ""
  if spread-type = "cognitive" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "one_spread_iteration(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," ([dict-value brain "ID"] of cit) "," (list-as-py-dict message true false) "," py-function ")")
  report py:runresult(
    command
  )
end

to-report spread-from-media [ med message limit ]
  let citizen-arr list-as-py-array (map [ c -> agent-brain-as-py-dict [brain] of citizen c ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let nlogo-graph-py (word "nlogo_graph_to_nx(" citizen-arr "," edge-arr ")")
  let cits map [ sub -> [ dict-value brain "ID" ] of sub ] (sort [ subscriber-neighbors ] of med)
  let py-function ""
  if spread-type = "cognitive" [
    set py-function (word "curr_sigmoid_p(" cognitive-exponent "," cognitive-translate ")")
  ]
  let command (word "spread_from(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," (list-as-py-array cits false) "," (list-as-py-dict message true false) "," py-function "," limit ")")
  report py:runresult(
    command
  )
end

to-report spread-path [ heard-from end-citizen ]
  let command (word "reconstruct_spread_path(" (list-as-py-dict-rec heard-from true false) ",'" end-citizen "')")
  report py:runresult(
    command
  )
end

to-report believe-message-py [ agent-brain message ]
;  show(agent-brain-as-py-dict agent-brain)
  ;show(list-as-py-dict message false false)
;  show message
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

to-report topics-in-message [ message ]
  report py:runresult(
    word "topics_in_message(" (list-as-py-dict-rec topics true true) "," (list-as-py-dict message true false) ")"
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

;; Create a Barabasi-Albert graph with the NetworkX package in python
;; @param en - The number of nodes for the graph (since N is a global variable)
;; @param m - The number of edges to connect with when a node is added.
;; @reports A dictionary [ ['nodes' nodes] ['edges' edges] ] where nodes is a list
;; of single values, and edges is a list of two-element lists (indicating nodes).
to-report ba-graph-homophilic [en m]
  let brains map agent-brain-beliefs-as-list (sort citizens)
  let groupz map [cit -> (list-as-py-array ([groups] of cit) true) ] (sort citizens)
;  let all-groups set-merge-lists map [ cit -> [groups] of cit ] (sort citizens)
  let belief-homophilee 0
  let group-homophilee 0
  if belief-homophily? [ set belief-homophilee belief-homophily ]
  if group-homophily? [ set group-homophilee group-homophily ]
  report py:runresult((word "BA_graph_homophilic(" en "," m "," belief-resolution "," belief-homophilee "," group-homophilee "," (list-as-py-array brains false) "," (list-as-py-array groupz false) ")"))
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

to-report kronecker [ seed k ]
  report py:runresult(
    (word "kronecker_graph_bidirected(np.array(" seed ")," k ")")
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
        ask turtle u [ create-social-friend-to turtle v [ set weight citizen-citizen-influence ] ]
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

;; Report the amount of homophily in the graph by measuring the average neighbor
;; distance across the graph by belief.
to-report graph-homophily-belief
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let homophily py:runresult(
    (word "graph_homophily(nlogo_graph_to_nx(" citizen-arr "," edge-arr "))")
  )
  report homophily
end

;; Report the amount of homophily in the graph by measuring the average neighbor
;; distance across the graph by group membership.
to-report graph-homophily-group
  let citizen-arr list-as-py-array (map [ cit -> list-as-py-dict (list (list "groups" groups-embedding ([groups] of (citizen cit))) (list "ID" ([dict-value brain "ID"] of (citizen cit)))) true false ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let homophily py:runresult(
    (word "graph_homophily(nlogo_graph_to_nx(" citizen-arr "," edge-arr "))")
  )
  report homophily
end

to-report groups-embedding [ groupz ]
  let embedding py:runresult(
    (word "groups_to_embedding(" list-as-py-array groupz true ")")
  )
  report embedding
end

;; Report the amount of homophily for a single citizen by measuring the average neighbor
;; distance for it.
;; @param cit -- The citizen id to get homophily for.
to-report citizen-homophily [ cit-id ]
  let citizen-arr list-as-py-array (map [ c -> agent-brain-as-py-dict [brain] of citizen c ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  report py:runresult(
    (word "node_homophily(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," cit-id ")")
  )
end

to-report graph-fragmentation
  let cit-beliefs list-as-py-array (map [ cit -> list-as-py-dict-rec (agent-brain-beliefs-as-dict cit) true false ] (sort citizens)) false
  let media-beliefs list-as-py-array (map [ med -> list-as-py-dict-rec (agent-brain-beliefs-as-dict med) true false ] (sort medias)) false
  let subscriberz list-as-py-array (map [ med -> list-as-py-array ([ map [ s -> [ dict-value brain "ID"] of s ] sort subscriber-neighbors ] of med) false ] (sort medias)) false
  let py-function ""

  let command (word "graph_fragmentation(" cit-beliefs "," media-beliefs "," subscriberz ")")

  report py:runresult(
    command
  )
end

;; Report the amount of polarization in the graph.
;; @param attr - The attribute to measure polarization for.
to-report graph-polarization [ attr ]
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  report py:runresult(
    (word "nlogo_graph_polarization(" citizen-arr "," edge-arr ",'" attr "'," (belief-resolution - 1) ")")
  )
end

;; Report the amount of disagreement in the graph.
;; @param attr - The attribute to measure disagreement for.
to-report graph-disagreement [ attr ]
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  report py:runresult(
    (word "nlogo_graph_disagreement(" citizen-arr "," edge-arr ",'" attr "'," (belief-resolution - 1) ")")
  )
end

to-report agent-power [ agent attr ]
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let media-arr []
  let subscriber-arr []

  if is-media? agent [
    set media-arr list-as-py-array (map [ med -> agent-brain-as-py-dict [brain] of med ] (sort medias)) false
    set subscriber-arr list-as-py-array (sort subscribers) true
  ]

  if spread-type = "cognitive" [
    let scalar 1
    let ex 1
    let trans 0
    if cognitive-scalar? [ set scalar cognitive-scalar ]
    if cognitive-exponent? [ set ex cognitive-exponent ]
    if cognitive-translate? [ set trans cognitive-translate ]
    ifelse is-media? agent [
      report py:runresult((word "first_degree_power_cognitive(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr ")," ([dict-value brain "ID"] of agent) ",'" attr "','" cognitive-fn "'," scalar "," ex "," trans ")"))
    ] [
      report py:runresult((word "first_degree_power_cognitive(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," ([dict-value brain "ID"] of agent) ",'" attr "','" cognitive-fn "'," scalar "," ex "," trans ")"))
    ]
  ]
  if spread-type = "complex" [
    ifelse is-media? agent [
      report py:runresult((word "first_degree_power_complex(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr ")," ([dict-value brain "ID"] of agent) ",'" attr "'," complex-spread-ratio ")"))
    ] [
      report py:runresult((word "first_degree_power_complex(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," ([dict-value brain "ID"] of agent) ",'" attr "'," complex-spread-ratio ")"))
    ]
  ]
  if spread-type = "simple" [
    ifelse is-media? agent [
      report py:runresult((word "first_degree_power_simple(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr ")," ([dict-value brain "ID"] of agent) "," simple-spread-chance ")"))
    ] [
      report py:runresult((word "first_degree_power_simple(nlogo_graph_to_nx(" citizen-arr "," edge-arr ")," ([dict-value brain "ID"] of agent) "," simple-spread-chance ")"))
    ]
  ]
end

to-report agents-power [ attr ]
  let citizen-arr list-as-py-array (map [ cit -> agent-brain-as-py-dict [brain] of citizen cit ] (range N)) false
  let edge-arr list-as-py-array (sort social-friends) true
  let media-arr list-as-py-array (map [ med -> agent-brain-as-py-dict [brain] of med ] (sort medias)) false
  let subscriber-arr list-as-py-array (sort subscribers) true

  if spread-type = "cognitive" [
    let scalar 1
    let ex 1
    let trans 0
    if cognitive-scalar? [ set scalar cognitive-scalar ]
    if cognitive-exponent? [ set ex cognitive-exponent ]
    if cognitive-translate? [ set trans cognitive-translate ]
    report py:runresult((word "first_degree_power_dist_cognitive(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr "),'" attr "','" cognitive-fn "'," scalar "," ex "," trans ")"))
  ]
  if spread-type = "complex" [
    report py:runresult((word "first_degree_power_dist_complex(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr "),'" attr "'," complex-spread-ratio ")"))
  ]
  if spread-type = "simple" [
    report py:runresult((word "first_degree_power_dist_simple(nlogo_graph_to_nx_with_media(" citizen-arr "," edge-arr "," media-arr "," subscriber-arr ")," simple-spread-chance ")"))
  ]
end

;; Report the chi-squared test values between two distributions.
;; @param dist1 - The first distribution.
;; @param dist2 - The second distribution.
to-report chi-sq [ dist1 dist2 ]
  let dist []
  let i 0
  repeat length dist1 [
    ;; TODO: Maybe remove these plus ones?? They're just there b/c chi-sq needs at least 1 sample
    let entry (list ((item i dist1) + 1) ((item i dist2) + 1))
    set dist lput (list-as-py-array entry false) dist
    set i i + 1
  ]
  report py:runresult((word "chi2_contingency(" (list-as-py-array dist false) ")"))
end

;; Write out the message-related data that the simulation has stored: agent beliefs at each time step,
;; messages that agents heard, messages that agents believed, messages that institutions sent.
to output-message-data [ path uniqueid ]
  let bel-over-time-py list-as-py-array (map [ tick-entry -> (word "{" (item 0 tick-entry) ": " (list-as-py-array (map [ bels -> list-as-py-dict-rec bels true false ] (item 1 tick-entry)) false) "}") ] beliefs-over-time) false
  let messages-heard-py list-as-py-array (map [ cit -> [ list-as-py-array (map [ tick-entry -> (word "{" (item 0 tick-entry) ": " (list-as-py-array (item 1 tick-entry) false) "}") ] messages-heard) false ] of cit ] (sort citizens)) false
  let messages-believed-py list-as-py-array (map [ cit -> [ list-as-py-array (map [ tick-entry -> (word "{" (item 0 tick-entry) ": " (list-as-py-array (item 1 tick-entry) false) "}") ] messages-believed) false ] of cit ] (sort citizens)) false
  ; This is the old version without being keyed on media
;  let messages-sent-py list-as-py-array (map [ med -> [ list-as-py-dict-rec (list idee messages-sent) true false ] of med ] (sort medias)) false
  let messages-sent-py list-as-py-dict (map [ med -> [ (list idee list-as-py-dict-rec (map [ message -> message ] messages-sent) true false)] of med ] (sort medias)) true false
  py:run (word "write_message_data('" path "'," "'" uniqueid "'," bel-over-time-py "," messages-heard-py "," messages-believed-py "," messages-sent-py ")")
end

to convert-world-data-to-message-data [ path ]
  let path-ids py:runresult((word "get_all_world_files('" path "')"))
  foreach path-ids [ path-id ->
    let pth item 0 path-id
    let ids item 1 path-id
    foreach ids [ id ->
      show (word "Generating message data for " pth ": " id)
      import-world (word pth "/" id "_world.csv")
      output-message-data pth read-from-string(id)
    ]
  ]
end

;;;;;;;;;;;;;;;
; HELPER PROCS
;;;;;;;;;;;;;;;

;; Perform a set union of two lists
;;
;; @param lists - The lists to union
to-report set-merge-lists [ lists ]
  let merged []
  foreach lists [ l ->
    foreach l [ element -> set merged (lput element merged) ]
  ]
  set merged remove-duplicates merged
  report merged
end

;; Report the distribution (count) of beliefs for attribute attr over a given agent set.
;; @param agentset - The agentset to count over.
;; @param attr - The string attribute to search for in agent brains.
to-report agent-brain-distribution [ agentset attr ]
  let brain-attrs [ dict-value brain attr ] of agentset
  let i 0
  let dist []
  repeat belief-resolution [
    let num-attr length filter [ el -> el = i ] brain-attrs
    set dist lput num-attr dist
    set i i + 1
  ]
  report dist
end

to-report is-agent-brain-empty? [ agent ]
  report empty? agent-brain-malleable-values agent
end

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

to-report agent-brain-as-py-dict [ b ]
  ;; Convert to a py-dict
  report list-as-py-dict-rec b true false
end

to-report agent-brain-malleable-values [ agent ]
  let b [brain] of agent
  let malleables (dict-value b "malleable")
  report filter [ bel -> member? (item 0 bel) malleables ] [brain] of agent
end

to-report agent-brain-prior-values [ agent ]
  let b [brain] of agent
  let priors (dict-value b "prior")
  report filter [ bel -> member? (item 0 bel) priors ] [brain] of agent
end

;; Get the beliefs of an agent as a list, not a dictionary including their name
;; @param agent - The turtle to get beliefs for
;; @reports A list of lists containing only the integer belief values for each belief in order
to-report agent-brain-beliefs-as-list [ agent ]
  let b [brain] of agent
  let priors (dict-value b "prior")
  let malleables (dict-value b "malleable")
  let bels filter [ bel -> member? (item 0 bel) priors or member? (item 0 bel) malleables ] [brain] of agent
  report map [ bel -> item 1 bel ] bels
end

;; Get the beliefs of an agent as a list, not a dictionary including their name
;; @param agent - The turtle to get beliefs for
;; @reports A list of lists containing only the integer belief values for each belief in order
to-report agent-brain-beliefs-as-dict [ agent ]
  let b [brain] of agent
  let priors (dict-value b "prior")
  let malleables (dict-value b "malleable")
  report filter [ bel -> member? (item 0 bel) priors or member? (item 0 bel) malleables ] [brain] of agent
end

to-report media-sent-messages-reduced [ med ]
  report [map [ m -> (item 0 (item 1 m)) ] messages-sent] of med
end

to-report media-messages-for-bel [ med bel ]
  report filter [ bel-val -> (item 0 bel-val) = bel ] media-sent-messages-reduced med
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

to-report last-n-of [ liste en ]
  report sublist liste (max (list (length liste - en) 0)) (length liste)
end

to-report belief-prob-list
  let l []
  let i 0
  repeat belief-resolution [
    set l lput (cognitive-contagion-p-no-cit [["A" 0]] (list (list "A" i))) l
    set i i + 1
  ]
  report l
end

;;;;;;;;;;;;;;;;;;
;; PROCS TO HELP
;; WITH PYTHON CONVERSION
;;;;;;;;;;;;;;;;;;

;; Replace an element in a NetLogo dictionary (list of lists) with a new value
;;
;; @param l - The list (dictionary) replace an item in
;; @param key - The key of the entry to replace
;; @param value - The new value to insert at entry @param key
to-report replace-dict-item [ l key value ]
  let key-i 0
  let i 0
  foreach l [ el ->
    if (item 0 el) = key [
      set key-i i
    ]
    set i i + 1
  ]
  report (replace-item key-i l (list key value))
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
  if length l = 0 [ report "{}" ]
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
880
35
1187
343
-1
-1
9.061
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
1
1
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
1449
394
1847
544
cit-a-histogram
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
1447
553
1505
598
0
count citizens with [dict-value brain \"A\" = 0]
1
1
11

MONITOR
1504
553
1561
598
1
count citizens with [dict-value brain \"A\" = 1]
1
1
11

MONITOR
1567
553
1633
598
2
count citizens with [dict-value brain \"A\" = 2]
1
1
11

MONITOR
1639
553
1697
598
3
count citizens with [dict-value brain \"A\" = 3]
1
1
11

MONITOR
1694
553
1752
598
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
832
788
962
821
threshold
threshold
0
20
20.0
1
1
NIL
HORIZONTAL

SLIDER
505
400
615
433
epsilon
epsilon
0
belief-resolution
1.0
1
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
1
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
680
305
830
323
Number of citizens
11
0.0
1

TEXTBOX
335
362
516
390
Connection Parameters
12
0.0
1

TEXTBOX
25
144
193
163
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
1452
334
1602
352
Simulation State Plots
14
0.0
1

SLIDER
678
327
850
360
N
N
0
1000
300.0
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
495
132
show-social-friends?
show-social-friends?
0
1
-1000

TEXTBOX
1449
364
1599
382
Cognitive State
11
0.0
1

PLOT
1047
902
1417
1052
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

CHOOSER
684
737
826
782
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
32
829
151
862
load-graph?
load-graph?
1
1
-1000

INPUTBOX
29
869
244
929
load-graph-path
D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/complex-contagion-sweep-BA-belief-homophily/graphs/15-0.75-0.75-6.csv
1
0
String

INPUTBOX
32
935
247
995
save-graph-path
D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/cognitive-contagion-sweep-BA-belief-homophily/graphs/15-2-5-0.75-5.csv
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

SLIDER
830
699
1004
732
simple-spread-chance
simple-spread-chance
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
830
737
1004
770
complex-spread-ratio
complex-spread-ratio
0
1
0.75
0.01
1
NIL
HORIZONTAL

CHOOSER
685
788
824
833
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
1000
63.0
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
D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/
1
0
String

PLOT
1754
35
2046
258
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
1749
553
1807
598
5
count citizens with [dict-value brain \"A\" = 5]
17
1
11

MONITOR
1812
553
1870
598
6
count citizens with [dict-value brain \"A\" = 6]
17
1
11

SWITCH
27
311
161
344
media-agents?
media-agents?
0
1
-1000

SLIDER
687
949
860
982
cognitive-exponent
cognitive-exponent
-10
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
687
909
860
942
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
867
909
1012
942
cognitive-scalar?
cognitive-scalar?
1
1
-1000

SWITCH
870
949
1024
982
cognitive-exponent?
cognitive-exponent?
0
1
-1000

SLIDER
687
994
860
1027
cognitive-translate
cognitive-translate
-10
20
0.0
1
1
NIL
HORIZONTAL

SWITCH
870
994
1025
1027
cognitive-translate?
cognitive-translate?
0
1
-1000

TEXTBOX
685
668
873
691
Contagion Parameters
14
0.0
1

CHOOSER
254
824
393
869
graph-type
graph-type
"erdos-renyi" "watts-strogatz" "barabasi-albert" "ba-homophilic" "mag" "facebook" "kronecker"
2

SLIDER
257
895
380
928
erdos-renyi-p
erdos-renyi-p
0
1
0.5
0.01
1
NIL
HORIZONTAL

TEXTBOX
27
280
255
299
Institutional Agent Parameters
14
0.0
1

TEXTBOX
33
780
221
803
Graph Parameters
14
0.0
1

SLIDER
409
847
543
880
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
409
887
536
920
watts-strogatz-k
watts-strogatz-k
0
N - 1
15.0
1
1
NIL
HORIZONTAL

SLIDER
254
962
379
995
ba-m
ba-m
0
20
15.0
1
1
NIL
HORIZONTAL

TEXTBOX
257
939
372
962
Barabasi-Albert (ba)
11
0.0
1

CHOOSER
252
1029
391
1074
mag-style
mag-style
"default" "homophilic" "heterophilic"
0

SWITCH
685
697
818
730
contagion-on?
contagion-on?
1
1
-1000

SLIDER
504
100
677
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

SLIDER
30
1113
187
1146
citizen-citizen-influence
citizen-citizen-influence
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
33
1032
186
1065
citizen-media-influence
citizen-media-influence
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
33
1072
188
1105
media-citizen-influence
media-citizen-influence
0
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
34
1008
184
1026
Link weight settings
11
0.0
1

INPUTBOX
409
946
552
1031
kronecker-seed
[[0.6,0.16,0.24],\n  [0.40,0.2,0.4],\n  [0.21,0.14,0.65]]
1
1
String

SLIDER
409
1037
501
1070
kronecker-k
kronecker-k
0
10
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
413
927
563
945
Kronecker
11
0.0
1

INPUTBOX
25
484
323
544
messages-data-file
./gallup-media-messages.json
1
0
String

SLIDER
208
413
321
446
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
1453
864
1826
1019
polarization
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
"0" 1.0 0 -16777216 true "" ";plot graph-polarization \"A\""

TEXTBOX
258
875
408
893
Erdos-Renyi (random)
11
0.0
1

TEXTBOX
409
824
559
842
Watts-Strogatz (small world)
11
0.0
1

TEXTBOX
255
1008
405
1026
Multiplicate Attribute Graph
11
0.0
1

TEXTBOX
32
807
182
825
Saving/loading
11
0.0
1

TEXTBOX
679
278
829
296
Citizen Parameters
14
0.0
1

CHOOSER
856
414
994
459
citizen-init-dist
citizen-init-dist
"uniform" "normal" "polarized"
2

TEXTBOX
857
389
1007
412
Initial belief dist whole pop
12
0.0
1

SLIDER
1017
415
1145
448
cit-init-normal-mean
cit-init-normal-mean
0
belief-resolution
3.0
1
1
NIL
HORIZONTAL

SLIDER
1017
454
1147
487
cit-init-normal-std
cit-init-normal-std
0
belief-resolution / 2
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
30
362
180
380
Messaging tactics
12
0.0
1

CHOOSER
27
412
205
457
institution-tactic
institution-tactic
"predetermined" "broadcast-brain" "appeal-mean" "appeal-mode" "appeal-median" "max-reach-no-chain"
0

TEXTBOX
28
465
277
493
If using predetermined tactics from a file
11
0.0
1

CHOOSER
27
592
166
637
media-ecosystem
media-ecosystem
"predetermined" "distribution"
0

INPUTBOX
29
675
288
735
media-ecosystem-file
./gallup-media-ecosystem.json
1
0
String

CHOOSER
183
593
328
638
media-ecosystem-dist
media-ecosystem-dist
"uniform" "normal" "polarized"
2

SLIDER
334
669
484
702
media-dist-normal-mean
media-dist-normal-mean
0
belief-resolution
3.0
1
1
NIL
HORIZONTAL

SLIDER
334
709
486
742
media-dist-normal-std
media-dist-normal-std
0
belief-resolution
1.0
1
1
NIL
HORIZONTAL

SLIDER
332
593
474
626
media-ecosystem-n
media-ecosystem-n
0
100
15.0
1
1
NIL
HORIZONTAL

PLOT
1449
609
1851
759
media-a-histogram
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
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -1 (belief-resolution + 1)\nlet messages-by-media filter [m -> not empty? m ] (map [ m -> (last-n-of (map [ bel-val -> (item 1 bel-val) ] (media-messages-for-bel m \"A\")) cit-memory-len) ] (sort medias))\nlet media-bel-by-messages (map [ m -> round mean m ] messages-by-media)\nhistogram media-bel-by-messages\n;histogram [dict-value brain \"A\"] of medias"

MONITOR
1447
767
1505
812
0
count medias with [dict-value brain \"A\" = 0]
0
1
11

MONITOR
1510
768
1568
813
1
count medias with [dict-value brain \"A\" = 1]
0
1
11

MONITOR
1570
768
1628
813
2
count medias with [dict-value brain \"A\" = 2]
0
1
11

MONITOR
1632
768
1690
813
3
count medias with [dict-value brain \"A\" = 3]
0
1
11

MONITOR
1694
769
1752
814
4
count medias with [dict-value brain \"A\" = 4]
0
1
11

MONITOR
1755
769
1813
814
5
count medias with [dict-value brain \"A\" = 5]
0
1
11

MONITOR
1815
769
1873
814
6
count medias with [dict-value brain \"A\" = 6]
0
1
11

SWITCH
253
1085
391
1118
belief-homophily?
belief-homophily?
0
1
-1000

SLIDER
25
215
198
248
repetition
repetition
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
684
582
801
615
cit-memory-len
cit-memory-len
0
20
1.0
1
1
NIL
HORIZONTAL

SLIDER
853
590
993
623
zeta-cit
zeta-cit
0
1
0.5
0.01
1
NIL
HORIZONTAL

CHOOSER
853
542
992
587
citizen-trust-fn
citizen-trust-fn
"average-bel"
0

SWITCH
683
542
847
575
citizen-citizen-trust?
citizen-citizen-trust?
1
1
-1000

SWITCH
449
503
601
536
citizen-media-trust?
citizen-media-trust?
1
1
-1000

PLOT
1454
1024
1831
1186
homophily
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
"default" 1.0 0 -16777216 true "" ";plot 1 / (1 + (item 0 graph-homophily))"

SLIDER
334
503
447
536
zeta-media
zeta-media
0
1
0.5
0.01
1
NIL
HORIZONTAL

TEXTBOX
28
389
97
412
Tactic
11
0.0
1

TEXTBOX
209
377
334
405
How many messages\nto send per tick
11
0.0
1

TEXTBOX
334
468
522
496
Trust threshold for\nconnection
11
0.0
1

TEXTBOX
29
563
217
586
Media initialization
12
0.0
1

TEXTBOX
185
570
373
593
Media distribution parameters
11
0.0
1

TEXTBOX
336
645
455
668
If normal distribution
11
0.0
1

TEXTBOX
30
653
290
681
If using predetermined ecosystem from a file
11
0.0
1

TEXTBOX
27
263
676
291
========================================================================
11
0.0
1

TEXTBOX
664
268
679
873
|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|\n|
11
0.0
1

TEXTBOX
1019
394
1207
417
Parameters for normal dist
11
0.0
1

TEXTBOX
685
505
873
533
Parameters for\nre-connection
11
0.0
1

TEXTBOX
30
760
665
779
========================================================================
11
0.0
1

TEXTBOX
684
638
1008
657
====================================
11
0.0
1

SWITCH
538
179
673
212
matrix-spread?
matrix-spread?
1
1
-1000

SWITCH
539
218
694
251
matrix-trust-conn?
matrix-trust-conn?
1
1
-1000

PLOT
1457
1195
1831
1393
fragmentation
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
"default" 1.0 0 -16777216 true "" ";plot (1 / (1 + (item 0 graph-fragmentation)))"

SWITCH
1002
544
1169
577
cits-hear-from-path?
cits-hear-from-path?
1
1
-1000

CHOOSER
679
416
844
461
citizen-init-type
citizen-init-type
"whole-population" "per-group"
1

INPUTBOX
1157
414
1408
474
citizen-init-per-group-file
./gallup-cit-init-dist.json
1
0
String

CHOOSER
345
412
496
457
media-connection-type
media-connection-type
"per-group" "epsilon"
0

INPUTBOX
505
439
654
499
media-connection-file
./gallup-media-connections.json
1
0
String

PLOT
1198
34
1739
256
supportive-opinion-per-group
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
"dem" 1.0 0 -13345367 true "" "plot count citizens with [ (dict-value brain \"A\") >= 5 and member? \"DEM\" groups ]"
"mod" 1.0 0 -8630108 true "" "plot count citizens with [ (dict-value brain \"A\") >= 5 and member? \"MOD\" groups ]"
"rep" 1.0 0 -5298144 true "" "plot count citizens with [ (dict-value brain \"A\") >= 5 and member? \"REP\" groups ]"

CHOOSER
687
859
840
904
cognitive-fn
cognitive-fn
"linear-gullible" "linear-stubborn" "linear-mid" "threshold-gullible" "threshold-mid" "threshold-stubborn" "sigmoid-gullible" "sigmoid-stubborn" "sigmoid-mid"
7

SWITCH
410
1086
564
1119
group-homophily?
group-homophily?
1
1
-1000

SLIDER
253
1127
392
1160
belief-homophily
belief-homophily
0
1
0.75
0.01
1
NIL
HORIZONTAL

SLIDER
410
1125
566
1158
group-homophily
group-homophily
0
1
0.75
0.01
1
NIL
HORIZONTAL

MONITOR
1186
536
1303
581
curr-bel-homophily
;1 / (1 + (item 0 graph-homophily-belief))\n0
3
1
11

MONITOR
1185
590
1318
635
curr-group-homophily
;1 / (1 + (item 0 graph-homophily-group))\n0
3
1
11

PLOT
1889
375
2152
527
DEM-a-histogram
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
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -1 (belief-resolution + 1)\n\nhistogram [dict-value brain \"A\"] of citizens with [groups = [\"DEM\"]]"

PLOT
1889
694
2152
854
REP-a-histogram
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
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -1 (belief-resolution + 1)\n\nhistogram [dict-value brain \"A\"] of citizens with [groups = [\"REP\"]]"

PLOT
1890
532
2149
684
MOD-a-histogram
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
"default" 1.0 1 -16777216 true "" "plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range -1 (belief-resolution + 1)\n\nhistogram [dict-value brain \"A\"] of citizens with [groups = [\"MOD\"]]"

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
  <experiment name="simple-contagion-param-sweep-ER" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-param-sweep-WS" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-param-sweep-BA" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-param-sweep-ER" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-param-sweep-ER_TEST" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe) "-simple-contagion-sweep-ER-TEST")
let graphs-path (word run-dir "/graphs")
if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
  py:run (word "create_nested_dirs('" graphs-path "')")
]
let graph-file (word graphs-path "/" erdos-renyi-p "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" simple-spread-chance "/" repetition)
if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
  py:run (word "create_nested_dirs('" contagion-dir "')")
]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")</final>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="73"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="3"/>
  </experiment>
  <experiment name="cog-contagion-param-sweep-WS" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-param-sweep-BA" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-ER" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-WS" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-BA" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-BA-homophilic-bel" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe) "-complex-contagion-sweep-BA-homophily-bel")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" belief-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" belief-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-BA-homophilic-group" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-param-sweep-BA-homophilic-bel-group" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir substring date-time-safe 11 (length date-time-safe) "-complex-contagion-sweep-BA-homophily-bel-group")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" belief-homophily "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" belief-homophily "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-param-sweep-BA-homophilic-group" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" simple-spread-chance "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" simple-spread-chance "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-folders-graphs-ER" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-folders-graphs-WS" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-folders-graphs-BA" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" simple-spread-chance "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" simple-spread-chance "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-folders-graphs-ER" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-folders-graphs-WS" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-folders-graphs-BA" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-folders-graphs-ER" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-ER")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" erdos-renyi-p "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" erdos-renyi-p "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;erdos-renyi&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="erdos-renyi-p">
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-folders-graphs-WS" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-WS")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" watts-strogatz-p "-" watts-strogatz-k "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" watts-strogatz-p "/" watts-strogatz-k "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;watts-strogatz&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-p">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="watts-strogatz-k">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-folders-graphs-BA" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-BA")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" cognitive-translate "-" cognitive-exponent "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" cognitive-translate "/" cognitive-exponent "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;barabasi-albert&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-folders-graphs-BA-homophilic-group" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-folders-graphs-BA-homophilic-group" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" simple-spread-chance "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" simple-spread-chance "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-param-sweep-BA-homophilic-group" repetitions="30" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" cognitive-translate "-" cognitive-exponent "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" cognitive-translate "/" cognitive-exponent "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <final>set behavior-rand random 10000
export-world (word contagion-dir "/" behavior-rand "_world.csv")
export-plot "percent-agent-beliefs" (word contagion-dir "/" behavior-rand "_percent-agent-beliefs.csv")
export-plot "supportive-opinion-per-group" (word contagion-dir "/" behavior-rand "_opinion-timeseries.csv")</final>
    <timeLimit steps="63"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-folders-graphs-BA-homophilic-group" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-BA-group-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" cognitive-translate "-" cognitive-exponent "-" group-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" cognitive-translate "/" cognitive-exponent "/" group-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="simple-contagion-folders-graphs-BA-homophilic-bel" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "simple-contagion-sweep-BA-belief-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" simple-spread-chance "-" belief-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" simple-spread-chance "/" belief-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;simple&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simple-spread-chance">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="complex-contagion-folders-graphs-BA-homophilic-bel" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "complex-contagion-sweep-BA-belief-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" complex-spread-ratio "-" belief-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" complex-spread-ratio "/" belief-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;complex&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="complex-spread-ratio">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="cog-contagion-folders-graphs-BA-homophilic-bel" repetitions="1" runMetricsEveryStep="false">
    <setup>setup-py
let run-dir (word sim-output-dir "cognitive-contagion-sweep-BA-belief-homophily")
let graphs-path (word run-dir "/graphs")
carefully [
  if not (py:runresult (word "os.path.isdir('" graphs-path "')")) [
    py:run (word "create_nested_dirs('" graphs-path "')")
  ]
] [ ]
let graph-file (word graphs-path "/" ba-m "-" cognitive-translate "-" cognitive-exponent "-" belief-homophily "-" repetition ".csv")
ifelse (py:runresult (word "os.path.isfile('" graph-file "')")) [
  set load-graph? true
  set load-graph-path graph-file
  setup
] [
  set load-graph? false
  set save-graph-path graph-file
  setup
  save-graph
]
set contagion-dir (word run-dir "/" ba-m "/" cognitive-translate "/" cognitive-exponent "/" belief-homophily "/" repetition)
carefully [
  if not (py:runresult (word "os.path.isdir('" contagion-dir "')")) [
    py:run (word "create_nested_dirs('" contagion-dir "')")
  ]
] [ ]</setup>
    <go>go</go>
    <timeLimit steps="5"/>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="tick-end">
      <value value="63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graph-type">
      <value value="&quot;ba-homophilic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-m">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="group-homophily?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-homophily">
      <value value="0.1"/>
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-agents?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-ecosystem-file">
      <value value="&quot;./gallup-media-ecosystem.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="media-connection-file">
      <value value="&quot;./gallup-media-connections.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institution-tactic">
      <value value="&quot;predetermined&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="messages-data-file">
      <value value="&quot;./gallup-media-messages.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="message-repeats">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-type">
      <value value="&quot;per-group&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-init-per-group-file">
      <value value="&quot;./gallup-cit-init-dist.json&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spread-type">
      <value value="&quot;cognitive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-fn">
      <value value="&quot;sigmoid-stubborn&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-scalar?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-exponent">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cognitive-translate">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contagion-on?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="belief-resolution">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="brain-type">
      <value value="&quot;discrete&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-spread?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-media-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="citizen-citizen-trust?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="matrix-trust-conn?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sim-output-dir">
      <value value="&quot;D:/school/grad-school/Tufts/research/gallup-media-mask/simulation-data/&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="repetition" first="0" step="1" last="9"/>
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
