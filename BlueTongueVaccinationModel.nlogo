extensions [csv nw]          ; csv:  extension to read-in csv files                                    ;
                             ;  nw:    Network extension, tool to calculate the clustering-coefficient ;
                                                                                                       ;
breed [farms farm]                                                                                     ;
                                                                                                       ;
undirected-link-breed [larger-reach-links larger-reach-link] ; Farmers with larger network             ;
undirected-link-breed [normal-reach-links normal-reach-link] ; Farmers with smaller, local network     ;
                                                                                                       ;
globals[
                                                                                                       ;
  total-farms                ; Number of farms in the model                                            ;
  nr-of-farmers-larger-reach ; Percentage of total farms with larger network                           ;
                                                                                                       ;
  total-sus                  ; Total number of farms susceptible after updating                        ;
  total-lat                  ; Total number of farms latently infected after updating                  ;
  total-inf                  ; Total number of farms infected after updating                           ;
  total-imm-v                ; Total number of farms immune through vaccination after updating         ;
  total-imm-d                ; Total number of farms immune through infection after updating           ;
  total-dec-v                ; Total number of farms decided to vaccinate                              ;
                                                                                                       ;
  weight-A-IN                ; The weight by which the injunctive norm is updated by attitude          ;
  weight-IN-DN               ; The weight by which the injunctive norm is updated by descriptive norm  ;
                                                                                                       ;
  daily-infected-farms       ; Daily number of farms infected                                          ;
  daily-vaccinated-farms     ; Daily number of farms vaccinated                                        ;
  weekly-infected-farms      ; Weekly number of farms infected                                         ;
  weekly-willing-to-vaccinate-farms    ; Weekly number of farmers willing to vaccinate                 ;
  weekly-decided-vaccinated-farms      ; Weekly number of farms decided to vaccinate                   ;
  weekly-immunized-vaccinated-farms    ; Weekly number of farms vaccinated                             ;
  weekly-A4                            ; Weekly update of mean of attitudinal statement of farms vacc. ;
  weekly-IN1                           ; Weekly update of mean of injunctive norm statement            ;
  weekly-DN2                           ; Weekly update of mean of descriptive norm statement           ;
                                                                                                       ;
  weekly-immunized-disease-farms       ; Weekly number of farms immunized                              ;
                                                                                                       ;
  ; Beta esimation, the main effects (marginal utilities) of attributes (levels) on choices            ;
  b_SVAE_SIG                 ; Serious Vaccine Adverse Effects with level significant                  ;
  b_SVAE_SML                 ;                                 with level small                        ;
  b_SVAE_NIL                 ;                                 with level negligible (base)            ;
  b_INFO_NON                 ; Information (Communication) with level none                             ;
  b_INFO_LFL                 ;                             with level leaflet (base)                   ;
  b_INFO_VET                 ;                             with level veterinarian                     ;
  b_INFO_L&V                 ;                             with level leaflet & veterinarian           ;
  b_SUBS_NON                 ; Subsidy with level none                                                 ;
  b_SUBS_10%                 ;         with level 10% (base)                                           ;
  b_SUBS_60%                 ;         with level 60%                                                  ;
  b_VCPA                     ; Vaccine Costs Per Animal                                                ;
  b_NO_CHOI                  ; Relative preference for vaccination over no vaccination                 ;
                                                                                                       ;
  ; Gamma estimates, interaction effects between main effects and farm(er) characteristics             ;
  g_Hsize1_NO_CHOI           ; Herd size x NO_CHOI                                                     ;
  g_Mprod_NO_CHOI            ; Milk production levels x NO_CHOI                                        ;
  g_Land1_SVAE_SIG           ; Pasture land utilized x SVAE_SIG                                        ;
  g_Land1_SVAE_SML           ; Pasture land utilized x SVAE_SML                                        ;
  g_Land1_SVAE_NIL           ; Pasture land utilized x SVAE_NIL                                        ;
  g_Land1_NO_CHOI            ; Pasture land utilized x NO_CHOI                                         ;
  g_Export_NO_CHOI           ; Export x NO_CHOI                                                        ;
  g_Age_SUBS_NON             ; Age x SUB_NON                                                           ;
  g_Age_SUBS_10%             ; Age x SUB_10%                                                           ;
  g_Age_SUBS_60%             ; Age x SUB_60%                                                           ;
  g_Age_VCPA                 ; Age x VCPA                                                              ;
  g_Age_NO_CHOI              ; Age x NO_CHOI                                                           ;
  g_High_educ_SVAE_SIG       ; Higher education x SVAE_SIG                                             ;
  g_High_educ_SVAE_SML       ; Higher education x SVAE_SML                                             ;
  g_High_educ_SVAE_NIL       ; Higher education x SVAE_NIL                                             ;
                                                                                                       ;
  ; Eta estimates, interaction effects between main effects and social-psychological constructs        ;
  n_A_INFO_NON               ; Attitude x INFO_NON                                                     ;
  n_A_INFO_LFL               ; Attitude x INFO_LFL                                                     ;
  n_A_INFO_VET               ; Attitude x INFO_VET                                                     ;
  n_A_INFO_L&V               ; Attitude x INFO_L&V                                                     ;
  n_A_NO_CHOI                ; Attitude x NO_CHOI                                                      ;
  n_IN_SUBS_NON              ; Injunctive norm x SUB_NON                                               ;
  n_IN_SUBS_10%              ; Injunctive norm x SUB_10%                                               ;
  n_IN_SUBS_60%              ; Injunctive norm x SUB_60%                                               ;
  n_IN_NO_CHOI               ; Injunctive norm x NO_CHOI                                               ;
  n_DN_SVAE_SIG              ; Descriptive norm x SVAE_SIG                                             ;
  n_DN_SVAE_SML              ; Descriptive norm x SVAE_SML                                             ;
  n_DN_SVAE_NIL              ; Descriptive norm x SVAE_NIL                                             ;
  n_DN_NO_CHOI               ; Descriptive norm x NO_CHOI                                              ;
                                                                                                       ;
  weekly-upd-SVAE-mean       ; Weekly update of SVAE utility of farmers (mean)                         ;
  weekly-upd-INFO-mean       ; Weekly update of INFO utility of farmers (mean)                         ;
  weekly-upd-SUB-mean        ; Weekly update of SUB utility of farmers  (mean)                         ;
  weekly-upd-SVAE-std.dev    ; Weekly update of SVAE utility of farmers (std.dev)                      ;
  weekly-upd-INFO-std.dev    ; Weekly update of INFO utility of farmers (std.dev)                      ;
  weekly-upd-SUB-std.dev     ; Weekly update of SUB utility of farmers  (std.dev)                      ;
                                                                                                       ;
  monthly-aggreg_util-INFO   ; Monthly update of aggregate utility for INFO                            ;
  monthly-aggreg_util-SUB    ; Monthly update of aggregate utility for SUB                             ;
  monthly-aggreg_util-SVAE   ; Monthly update of aggregate utility for SVAE                            ;
  monthly-aggreg_RP          ; Monthly update of aggregate risk perception                             ;
  monthly-aggreg_SP          ; Monthly update of aggregate social pressures                            ;
                                                                                                       ;
  hsize_min                  ; Minimum value of herd size in dataset used                              ;
  hsize_max                  ; Maximum value of herd size in dataset used                              ;
]                                                                                                      ;
                                                                                                       ;
farms-own[                                                                                             ;
  ; epidemiological module                                                                             ;
  ;  inf-duration            ; Duration to infection to other farms starting at the start of infectious;
                             ; period ; Not required to be a state-variable                            ;
  inf-moment                 ; The moment (tick) the farm is infected (latent period starts)           ;
  vir-moment                 ; The moment (tick) the farm has the virus (infectious period starts)     ;
  rec-moment                 ; The moment (tick) the farm is recovered from the virus                  ;
  re                         ; Reproduction ratio                                                      ;
                                                                                                       ;
  vacc-moment                ; The moment (tick) the farm decided to vaccinate                         ;
  immune-moment              ; The moment (tick) the farm is immune through vaccination                ;
                                                                                                       ;
  ; economical module                                                                                  ;
  id_netlogo                 ; ID used to link estimation results from the choice model to the agents  ;
  age                        ; Farmer's age (standardized value [0,1])                                 ;
  high_educ                  ; Education level (1 = higher education)                                  ;
                                                                                                       ;
  hsize1                     ; Herd size (standardized value [0,1])                                    ;
  mprod                      ; Average milk production level (standardized value [0,1])                ;
  land1                      ; Amount of land utilized (standardized value [0,1])                      ;
  export                     ; Exportation of heifers (1 = yes)                                        ;
                                                                                                       ;
  dis-costs                  ; Disease costs in case of an infection (normal distr [50,10] x herd size);
                                                                                                       ;
  nr-peers-larger-reach-nw   ; Number of peers in one's network from the larger reach                  ;
  nr-peers-normal-reach-nw   ; Number of peers in one's network from the small reach                   ;
  nr-peers-network           ; Total number of peers in one's network                                  ;
                                                                                                       ;
  cluster-coef-larger-reach  ; How connected its neighbors in the larger-reach network are (clique)    ;
  cluster-coef-normal-reach  ; How connected its neighbors in the normal-reach network are (clique)    ;
  cluster-coef               ; How connected its neighbors in the network are (clique)                 ;
                                                                                                       ;
                                                                                                       ;
  initial-A                  ; Initial factor score for attitude (standardized value [0,1])            ;
  initial-IN                 ; Initial factor score for injunctive norm (standardized value [0,1])     ;
  initial-DN                 ; Initial factor score for descriptive norm (standardized value [0,1])    ;
  A4                         ; 'Participation is bad ... good  [1-7]                                   ;
  IN1                        ; People who have to do a lot with my farm expect me [1-7]                ;
  DN2                        ; Dairy farmers in my social network will vaccinate [1-7]                 ;
                                                                                                       ;
  new-A                      ; Updated factor score for attitude as the model runs                     ;
  new-IN                     ; Updated factor score for injunctive norm as the model runs              ;
  new-DN                     ; Updated factor score for descriptive norm as the model runs             ;
                                                                                                       ;
  no_choices                 ; Number of NO-choices from the respondent in the choice experiment       ;
                                                                                                       ;
  dist-farms-inf             ; Distance to the nearest infected farm (with color red)                  ;
  risk-perc                  ; Risk perception based on # infections and distance closest infection    ;
                                                                                                       ;
  utility-SVAE               ; Utilify for SVAE                                                        ;
  utility-INFO               ; Utilify for INFO                                                        ;
  utility-SUB                ; Utilify for SUB                                                         ;
  utility-COSTS              ; Utilify for COSTS                                                       ;
                                                                                                       ;
  utility-yes                ; Utility derived from vaccination                                        ;
  utility-no                 ; Utility derived from no vaccination                                     ;
  utility-diff               ; Difference between utility-yes and utility-no                           ;
  prob-yes                   ; Fixed vacc. probability                                                 ;
                                                                                                       ;
  vaccinated?                ; State vaccinated, yes or no                                             ;
                                                                                                       ;
  dn-score                   ; Number of peers in own network vaccinated x similarity                  ;
  max-dn-score               ; Number of peers in own network x similarity                             ;
                                                                                                       ;
  vacc-costs                 ; Total vacc. costs: Herd size x vaccination costs x (1 - subsidy level)  ;
]                                                                                                      ;
                                                                                                       ;
;links between farmers within a large radius                                                           ;
larger-reach-links-own [                                                                               ;
  similarity                 ; 1 / (1 + Euclidean distance based on age, hize, mprod, land)            ;
  one-end-vacc?              ; Whether the other end of the link (farm) has vaccinated, or not         ;
]                                                                                                      ;
                                                                                                       ;
;links between farmers within a normall radius                                                         ;
normal-reach-links-own [                                                                               ;
  similarity                 ; 1 / (1 + Euclidean distance based on age, hize, mprod, land)            ;
  one-end-vacc?              ; Whether the other end of the link (farm) has vaccinated, or not         ;
]                                                                                                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SET UP Procedure
to SETUP
  reset-timer
  clear-all
  reset-ticks
  resize-world -25 25 -25 25
  ;random-seed 1
  parameterize-globals
  create-nr-of-farms
  load-farm-characteristics
  create-farmers-network
  infect-first
  setup-plots
  print timer
end

;RUN model
to GO
  if ticks = 0 [reset-timer]
  vaccinate
  infect
  update-epidemiology
  update-utility


  ifelse ticks < epi-end [tick][
    print timer
    stop]

end

;START SETUP PROCEDURE ;
to parameterize-globals ;set values of global parameters
  set total-farms farm-density * world-width * world-height

  set daily-infected-farms    []
  set weekly-infected-farms   []
  set daily-vaccinated-farms  []
  set weekly-willing-to-vaccinate-farms []
  set weekly-decided-vaccinated-farms   []
  set weekly-immunized-vaccinated-farms []
  set weekly-A4  []
  set weekly-IN1 []
  set weekly-DN2 []
  set weekly-immunized-disease-farms []
  set weekly-upd-SVAE-mean []
  set weekly-upd-INFO-mean []
  set weekly-upd-SUB-mean  []
  set weekly-upd-SVAE-std.dev []
  set weekly-upd-INFO-std.dev []
  set weekly-upd-SUB-std.dev  []
  set monthly-aggreg_util-INFO[]
  set monthly-aggreg_util-SUB []
  set monthly-aggreg_util-SVAE []
  set monthly-aggreg_RP        []
  set monthly-aggreg_SP        []

  ;set parameters from Sok et al. 2018
  set b_SVAE_SIG -0.36
  set b_SVAE_SML  0.26
  set b_SVAE_NIL  0.09
  set b_INFO_NON -0.07
  set b_INFO_LFL  0.07
  set b_INFO_VET -0.54
  set b_INFO_L&V  0.54
  set b_SUBS_NON -0.44
  set b_SUBS_10% -0.94
  set b_SUBS_60%  1.38
  set b_VCPA     -0.23
  set b_NO_CHOI   3.75

  set g_Hsize1_NO_CHOI      1.71
  set g_Mprod_NO_CHOI      -1.96
  set g_Land1_SVAE_SIG     -5.69
  set g_Land1_SVAE_SML      1.70
  set g_Land1_SVAE_NIL      3.99
  set g_Land1_NO_CHOI       1.19
  set g_Export_NO_CHOI     -0.53
  set g_Age_SUBS_NON        0.06
  set g_Age_SUBS_10%        1.18
  set g_Age_SUBS_60%       -1.24
  set g_Age_VCPA            0.16
  set g_Age_NO_CHOI         0.71
  set g_High_educ_SVAE_SIG -0.49
  set g_High_educ_SVAE_SML  0.24
  set g_High_educ_SVAE_NIL  0.25

  set n_A_INFO_NON   0.09
  set n_A_INFO_LFL  -0.28
  set n_A_INFO_VET   1.10
  set n_A_INFO_L&V  -0.90
  set n_A_NO_CHOI   -4.67
  set n_IN_SUBS_NON  0.10
  set n_IN_SUBS_10%  0.40
  set n_IN_SUBS_60% -0.50
  set n_IN_NO_CHOI  -1.63
  set n_DN_SVAE_SIG -0.63
  set n_DN_SVAE_SML -0.11
  set n_DN_SVAE_NIL  0.74
  set n_DN_NO_CHOI  -0.91

  set hsize_min  45
  set hsize_max 349

  set weight-A-IN  influence-A-on-IN
  set weight-IN-DN 1 - influence-A-on-IN
end

;create farms during set up procedure
to create-nr-of-farms
   ; spatially randomly located farms
  create-farms farm-density * world-width * world-height  [
    set   shape         "house efficiency"
    setxy random-xcor random-ycor
    set   color         white
    set   size          2
  ]

  ;create a set of farmers with large radius social contacts
  set nr-of-farmers-larger-reach %-with-larger-reach * (farm-density * world-width * world-height)

  ask n-of nr-of-farmers-larger-reach farms [
    set shape "house colonial"
  ]
end

;load farm and farmer chracteristics from file
to load-farm-characteristics
  ask farms [
    set inf-moment    10 ^ 10 ; infection moment = start latency period
    set vir-moment    10 ^ 10 ; viraemia moment = start infectious period
    set vacc-moment   10 ^ 10 ; vaccination moment
    set immune-moment 10 ^ 10 ; recovery moment = end infectious period
    set re (sum  (map [ (1 - exp (-1 * inf-period * kernel ?)) ] ([distance myself] of farms))) ;calculate effective reproduction number of this farm
  ]

  let input-characters csv:from-file import-path ;import file with farmer profiles
  let max-index (length input-characters - 1); maximum index
  ask farms[
    ;set farmer charactistics by selecting a random profile from the data
    let farmer-index (random max-index ) + 1
    set id_netlogo   item 1 item farmer-index  input-characters
    set age          item 2 item farmer-index input-characters
    set high_educ    item 3 item farmer-index input-characters
    set hsize1       item 4 item farmer-index input-characters
    set mprod        item 5 item farmer-index input-characters
    set land1        item 6 item farmer-index input-characters
    set export       item 7 item farmer-index input-characters
    set initial-A    item 8 item farmer-index input-characters
    set initial-IN   item 9 item farmer-index input-characters
    set initial-DN   item 10 item farmer-index input-characters
    set A4           item 11 item farmer-index input-characters
    set IN1          item 12 item farmer-index input-characters
    set DN2          item 13 item farmer-index input-characters
    set dis-costs    0
    set vaccinated?  false
  ]

end

;create the social network among farmers
to create-farmers-network
  ask farms with [shape = "house colonial"] [
    create-larger-reach-links-with other farms with [shape = "house colonial"] in-radius larger-reach  ;create network among those farms with a large radius
  ]
  ask farms [
    create-normal-reach-links-with other farms with [shape = "house efficiency"] in-radius normal-reach  ;create network among those farms with a small radius
  ]
  ask farms [
    ask links [
      set color 1
      set hidden? hide-links-farmers
    ]
  ]
  ask larger-reach-links [
    set similarity 1 / (1 + (sqrt (([age] of end1 - [age] of end2) ^ 2 + ([hsize1] of end1 - [hsize1] of end2) ^ 2 + ([mprod] of end1 - [mprod] of end2) ^ 2 + ([land1] of end1 - [land1] of end2) ^ 2)))
    set one-end-vacc?  false
  ]
  ask normal-reach-links [
    set similarity 1 / (1 + (sqrt (([age] of end1 - [age] of end2) ^ 2 + ([hsize1] of end1 - [hsize1] of end2) ^ 2 + ([mprod] of end1 - [mprod] of end2) ^ 2 + ([land1] of end1 - [land1] of end2) ^ 2)))
    set one-end-vacc?  false
  ]
end

;initialize the infection
to infect-first
  ask n-of initial-farms farms with [color = white] [
    set color blue
    set dis-costs (average-dis-cost * (((hsize1 * (hsize_max - hsize_min)) + hsize_min)))
    set vir-moment ticks + lat-period
    set rec-moment ticks + lat-period + inf-period
    ask farms with [color = white and immune-moment > ticks] [
      let distance-to-me distance myself
      let inf-duration thinning  (distance-to-me) ([vir-moment] of myself)
      if inf-duration < [rec-moment] of myself [
        set re re - (1 - exp (-1 * inf-period * kernel distance-to-me))
        set inf-moment min (list (ticks + lat-period + inf-duration) inf-moment)
      ]
    ]
  ]
end

; END SETUP PROCEDURE ;

; START GO PROCEDURE ;
to update-epidemiology
  ask farms with [color = white or color = blue] [ ;effect vaccination of farms starts
    if (vacc-moment <= inf-moment) and (ticks = immune-moment) [
      set color green
    ]
  ]
  ask farms with [color = blue] [ ;farm becomes infectious
    if (inf-moment <= vacc-moment) and (ticks = vir-moment) [
      set color red
      set dis-costs (average-dis-cost * (((hsize1 * (hsize_max - hsize_min)) + hsize_min)))
    ]
  ]
  ask farms with [color = red] [ ; farm recovers
    if ticks = rec-moment [
      set color yellow
    ]
  ]
  ;update epidemiological counters
  set total-sus   (count farms with [color = white]  / total-farms) * 100
  set total-lat   (count farms with [color = blue]   / total-farms) * 100
  set total-inf   (count farms with [color = red]    / total-farms) * 100
  set total-imm-v (count farms with [color = green]  / total-farms) * 100
  set total-imm-d (count farms with [color = yellow] / total-farms) * 100
  set total-dec-v (count farms with [Vaccinated?]   / total-farms) * 100

  ;update weekly counters
  if ticks mod 7 = 0 [
    set weekly-willing-to-vaccinate-farms lput (precision (count farms with [prob-yes >= Threshold] / total-farms) 3) weekly-willing-to-vaccinate-farms
    set weekly-infected-farms lput (precision (count farms with [color = red and vir-moment <= ticks and rec-moment - 50 >= ticks] / total-farms) 3) weekly-infected-farms
    set weekly-immunized-disease-farms lput (precision (count farms with [color = yellow and rec-moment <= ticks and ticks <= (rec-moment + 6)] / total-farms) 3) weekly-immunized-disease-farms
  ]
  if ticks mod 7 = 0 [
    set weekly-decided-vaccinated-farms lput (precision (count farms with [vaccinated? and vacc-moment <= ticks and ticks <= (vacc-moment + 6)] / total-farms) 3) weekly-decided-vaccinated-farms
    set weekly-immunized-vaccinated-farms lput (precision (count farms with [color = green and vacc-moment <= ticks and ticks <= (immune-moment + 6)] / total-farms) 2) weekly-immunized-vaccinated-farms
    ifelse count farms with [vaccinated? and vacc-moment <= ticks and ticks <= (vacc-moment + 6)] > 0
    [set weekly-A4 lput (precision (mean [A4] of farms with [vaccinated? and vacc-moment <= ticks and ticks <= (vacc-moment + 6)] ) 3) weekly-A4
     set weekly-IN1 lput (precision (mean [IN1] of farms with [vaccinated? and vacc-moment <= ticks and ticks <= (vacc-moment + 6)] ) 3) weekly-IN1
     set weekly-DN2 lput (precision (mean [DN2] of farms with [vaccinated? and vacc-moment <= ticks and ticks <= (vacc-moment + 6)] ) 3) weekly-DN2]
    [set weekly-A4 lput 0 weekly-A4
     set weekly-IN1 lput 0 weekly-IN1
     set weekly-DN2 lput 0 weekly-DN2]
  ]
  if ticks mod 7 = 0 [
     set weekly-upd-SVAE-mean lput (mean [utility-SVAE] of farms) weekly-upd-SVAE-mean
     set weekly-upd-INFO-mean lput (mean [utility-INFO] of farms) weekly-upd-INFO-mean
     set weekly-upd-SUB-mean  lput (mean [utility-SUB] of farms) weekly-upd-SUB-mean
     set weekly-upd-SVAE-std.dev lput (standard-deviation [utility-SVAE] of farms) weekly-upd-SVAE-std.dev
     set weekly-upd-INFO-std.dev lput (standard-deviation [utility-INFO] of farms) weekly-upd-INFO-std.dev
     set weekly-upd-SUB-std.dev  lput (standard-deviation [utility-SUB] of farms) weekly-upd-SUB-std.dev
  ]

  ;update monthly counters
  if ticks mod 28 = 1 [
    set monthly-aggreg_util-INFO lput (precision (mean [utility-INFO] of farms) 3) monthly-aggreg_util-INFO
    set monthly-aggreg_util-SUB  lput (precision (mean [utility-SUB]  of farms) 3) monthly-aggreg_util-SUB
    set monthly-aggreg_util-SVAE lput (precision (mean [utility-SVAE] of farms) 3) monthly-aggreg_util-SVAE
    set monthly-aggreg_RP        lput (precision (mean [new-A] of farms - mean [initial-A] of farms) 3) monthly-aggreg_RP
    set monthly-aggreg_SP        lput (precision (mean [new-DN] of farms - mean [initial-DN] of farms) 3) monthly-aggreg_SP
  ]
end

;handle infection moment of farms
to infect
  ask farms with [color = white and inf-moment <= ticks and immune-moment > ticks]
  [
    set color blue
    set re 0
    set vir-moment ticks + lat-period
    set rec-moment ticks + lat-period + inf-period
    ask farms with [color = white and immune-moment > ticks] [
      let distance-to-me distance myself
      let inf-duration thinning  (distance-to-me) ([vir-moment] of myself)
      if inf-duration < [rec-moment] of myself [
        set re re - (1 - exp (-1 * inf-period * kernel distance-to-me))
        set inf-moment min (list (inf-duration) inf-moment)
      ]
    ]
  ]
end

;handle vaccination of farms
to vaccinate
  ask farms with [color = white or color = blue and not vaccinated?] [
    if prob-yes >= threshold and vaccination-scheme? and ticks >= start-vaccination-scheme [
      set vaccinated? true
      set vacc-moment (start-vaccination-scheme + (ticks - start-vaccination-scheme))
      set immune-moment vacc-moment + 14
      if SUB = "NON" [
        set vacc-costs precision ((((hsize1 * (hsize_max - hsize_min)) + hsize_min) * 8) * 1) 0
      ]
      if SUB = "10%" [
        set vacc-costs precision ((((hsize1 * (hsize_max - hsize_min)) + hsize_min) * 8) * 0.9) 0
      ]
      if SUB = "60%" [
        set vacc-costs precision ((((hsize1 * (hsize_max - hsize_min)) + hsize_min) * 8) * 0.4) 0
      ]
    ]
  ]
  ask farms with [vaccinated?] [
    ask my-links [
      set one-end-vacc? true
      set color 62
    ]
  ]
end

;calculate utility of vaccination
to update-utility
  ask farms with [color = white or color = blue and not vaccinated?] [
    ifelse total-inf > 0 [ ; if infection is present update risk perception
      set dist-farms-inf min [distance myself] of farms with [color = red]
      let max-distance (sqrt (world-height ^ 2 + world-width ^ 2))
      set risk-perc (total-inf / 100) ^ (dist-farms-inf / max-distance)
    ]
    [set dist-farms-inf 0
     set risk-perc 0
     ]

    set new-A precision (initial-A * (1 + risk-perc)) 2                               ;update attitude
    set dn-score sum [similarity] of my-links with [one-end-vacc?]                    ;create score of similarity with vaccinated contacts norm
    set max-dn-score sum [similarity] of my-links                                     ;calculate maximum similarity

    ifelse max-dn-score != 0 [ ; update descriptive and injunctive norm
      set new-DN precision (initial-DN * (1 + (social-pressures * (dn-score / max-dn-score) ^ network-size-sensitivity))) 2
      set new-IN precision (initial-IN * (1 + (risk-perc * weight-A-IN) + (social-pressures * (dn-score / max-dn-score) ^ network-size-sensitivity) * weight-IN-DN)) 2
      ]
    [
      set new-IN precision (initial-IN * (1 + (risk-perc * weight-A-IN))) 2
      ]
    ; set utility based on intervention scheme
    if SVAE = "SIG" [
      set utility-SVAE b_SVAE_SIG + g_High_educ_SVAE_SIG * High_educ + g_Land1_SVAE_SIG * Land1 + n_DN_SVAE_SIG * new-DN
      ]
    if SVAE = "SML" [
      set utility-SVAE b_SVAE_SML + g_High_educ_SVAE_SML * High_educ + g_Land1_SVAE_SML * Land1 + n_DN_SVAE_SML * new-DN
      ]
    if SVAE = "NIL" [
      set utility-SVAE b_SVAE_NIL + g_High_educ_SVAE_NIL * High_educ + g_Land1_SVAE_NIL * Land1 + n_DN_SVAE_NIL * new-DN
    ]
    if INFO = "NON"[
      set utility-INFO b_INFO_NON + n_A_INFO_NON * New-A
    ]
    if INFO = "LFL"[
      set utility-INFO b_INFO_LFL + n_A_INFO_LFL * New-A
    ]
    if INFO = "VET"[
      set utility-INFO b_INFO_VET + n_A_INFO_VET * New-A
    ]
    if SUB = "NON" [
      set utility-SUB b_SUBS_NON + g_Age_SUBS_NON * Age + n_IN_SUBS_NON * new-IN
    ]
    if SUB = "10%" [
      set utility-SUB b_SUBS_10% + g_Age_SUBS_10% * Age + n_IN_SUBS_10% * new-IN
    ]
    if SUB = "60%" [
      set utility-SUB b_SUBS_60% + g_Age_SUBS_60% * Age + n_IN_SUBS_60% * new-IN
    ]
    if COSTS = 4 [
      set utility-COSTS b_VCPA * 4 + g_Age_VCPA * 4 * Age
    ]
    if COSTS = 8 [
      set utility-COSTS b_VCPA * 8 + g_Age_VCPA * 8 * Age
    ]
    if COSTS = 12 [
      set utility-COSTS b_VCPA * 12 + g_Age_VCPA * 12 * Age
    ]
    ; set utility in favour of vaccination
    set utility-yes utility-SVAE + utility-INFO + utility-SUB + utility-COSTS

    ;set utility against vaccination
    set utility-no  (b_NO_CHOI + g_Hsize1_NO_CHOI * Hsize1 + g_Mprod_NO_CHOI * Mprod + g_Land1_NO_CHOI * Land1 + g_Export_NO_CHOI * Export + g_Age_NO_CHOI * Age + n_A_NO_CHOI * New-A + n_IN_NO_CHOI * New-IN + n_DN_NO_CHOI * New-DN)

    ;calculate difference and set probability deciding in favour
    set utility-diff  utility-yes - utility-no
    set prob-yes 1 / (1 + exp(-1 * utility-diff))
  ]
end

;END GO PROCEDURE ;


;FUNCTIONS
; spatial transmission kernel = rate of infection by an infectious farm to a farm at distance r
to-report kernel[r]
  report  (labda0 * 10 ^ -6 / (1 + ( (r ) / r0 ) ^ alpha))
end

;thinning algorithm to determine next random event given a rate function
to-report thinning [dist vm]
   let time vm
   let tries 0
   while [tries < 10 ^ 10] [
     set time time + round((ln random-float 1) / (-1 * kernel dist))
     if (random-float 1) < (seasonality time) [
       report time
     ]
     if time > epi-end [
       report 10 ^ 10
     ]
     set tries tries + 1
   ]
   error word (word "thinning procedure did not end up with a proper outcome after" tries) "tries"
   report 10 ^ 10
end

;function to add seasonality in the transmission kernel
to-report seasonality [t]
  if t <= end-spring[report t / (end-spring + 10 ^ -10)]
  if t >  end-spring and t <= epi-end - start-autumn [ report 1]
  if t >  epi-end - start-autumn and t < epi-end [report (epi-end - t) / (start-autumn + 10 ^ -10)]
  if t >= epi-end [report 0]
end
@#$#@#$#@
GRAPHICS-WINDOW
365
50
889
595
25
25
10.08
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
10
10
65
43
SETUP
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
70
10
125
43
GO
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

PLOT
920
255
1430
500
Disease-evolution
Time
Percentage of farms
0.0
0.0
0.0
1.0
true
true
"set-plot-x-range 0 epi-end\nset-plot-y-range 0 100" ""
PENS
"Latent" 1.0 0 -14070903 true "" "plot total-lat"
"Infected" 1.0 0 -2674135 true "" "plot total-inf"
"Immune_thrgh_vacc" 1.0 0 -13840069 true "" "plot total-imm-v"
"Immune_thrgh_inf" 1.0 0 -1184463 true "" "plot total-imm-d"
"Susceptible" 1.0 0 -7500403 true "" "plot total-sus"

SLIDER
185
182
355
215
start-vaccination-scheme
start-vaccination-scheme
0
84
56
28
1
NIL
HORIZONTAL

SLIDER
6
217
176
250
normal-reach
normal-reach
0
5
2
1
1
NIL
HORIZONTAL

SWITCH
185
147
355
180
vaccination-scheme?
vaccination-scheme?
0
1
-1000

TEXTBOX
11
127
179
153
Social network parameters\n\n
12
0.0
1

TEXTBOX
188
127
363
157
Vacc. scheme parameters
12
0.0
1

SWITCH
6
287
176
320
hide-links-farmers
hide-links-farmers
1
1
-1000

PLOT
920
5
1430
251
Farmers-behaviour
Time
Percentage of farmers
0.0
0.0
0.0
1.0
false
true
"set-plot-x-range 0 epi-end\nset-plot-y-range 0 100" ""
PENS
"Willing-to-vaccinate" 1.0 0 -16777216 true "" "plot 100 * (count farms with [prob-yes >= Threshold] / (farm-density * world-width * world-height))"
"Decided-to-vaccinate" 1.0 0 -7500403 true "" "plot 100 * (count farms with [Vaccinated?] / (farm-density * world-width * world-height))"
"Immune-thrgh-vacc" 1.0 0 -2674135 true "" "plot 100 * (count farms with [color = green] / (farm-density * world-width * world-height))"

SLIDER
6
182
177
215
%-with-larger-reach
%-with-larger-reach
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
6
252
176
285
larger-reach
larger-reach
0
8
4
2
1
NIL
HORIZONTAL

SLIDER
6
147
176
180
farm-density
farm-density
0.1
1
0.5
0
1
NIL
HORIZONTAL

INPUTBOX
240
10
308
70
epi-end
175
1
0
Number

BUTTON
130
10
185
45
NIL
GO
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
6
562
176
595
lat-period
lat-period
7
28
14
7
1
NIL
HORIZONTAL

SLIDER
6
598
176
631
inf-period
inf-period
28
112
112
28
1
NIL
HORIZONTAL

SLIDER
185
527
355
560
influence-A-on-IN
influence-A-on-IN
0
1
0
0.5
1
NIL
HORIZONTAL

SLIDER
185
598
355
631
network-size-sensitivity
network-size-sensitivity
0
1
0.25
0.25
1
NIL
HORIZONTAL

SLIDER
6
526
176
559
initial-farms
initial-farms
1
11
6
5
1
NIL
HORIZONTAL

SLIDER
6
349
176
382
labda0
labda0
500
2500
1500
500
1
NIL
HORIZONTAL

SLIDER
6
384
176
417
r0
r0
0
10
3.9
0.1
1
NIL
HORIZONTAL

SLIDER
6
420
176
453
alpha
alpha
1
5
2
1
1
NIL
HORIZONTAL

SLIDER
6
455
176
488
end-spring
end-spring
0
84
0
28
1
NIL
HORIZONTAL

SLIDER
6
491
176
524
start-autumn
start-autumn
0
84
0
28
1
NIL
HORIZONTAL

SLIDER
6
633
176
666
average-dis-cost
average-dis-cost
0
0
0
0
1
NIL
HORIZONTAL

SLIDER
185
563
355
596
threshold
threshold
0.90
1
0.974
0.001
1
NIL
HORIZONTAL

CHOOSER
202
237
340
282
SVAE
SVAE
"SML" "NIL"
0

CHOOSER
202
287
340
332
INFO
INFO
"LFL" "VET"
0

CHOOSER
202
337
340
382
SUB
SUB
"10%" "60%"
0

CHOOSER
202
387
340
432
COSTS
COSTS
8
0

CHOOSER
202
438
340
483
social-pressures
social-pressures
0 1
1

TEXTBOX
11
328
161
346
Transmission parameters\n
12
0.0
1

TEXTBOX
190
502
355
532
Update function parameters\n
12
0.0
1

INPUTBOX
5
50
222
110
import-path
mock-agent-characteristics.csv
1
0
String

@#$#@#$#@
## WHAT IS IT?

To simulate the effectiveness of different government-initiated vaccination scheme designs, in terms of uptake.

## HOW IT WORKS

Farmers start collecting information on the number and closeness of disease infections (summarized into a risk perception) and on the number of farmers vaccinated in the social network (summarized into a measure of perceived social pressure). This information is used to update scores on attitude and perceived norms, and with these new scores preferences for different vaccination scheme attributes (utility). If the vaccination choice probability exceeds a threshold, the farmer switches from the unvaccinated to the vaccinated state, provided that the farm is not infected in the meantime.

Sub-models:
- Bluetongue transmission model, which calculates for each infected farm the probability of infecting each susceptible farm, as a function of inter-farm distance and the infectious period.
- Social network structure, which divides first all farmers in two groups, a major group assigned with a small radius and a minor group assigned with a larger radius. Farmers are connected based on a large and a small radius network.
- ‘Update function’, which equips each farmers with a utility and probability function, that during the simulation is updated using information about disease risk and vaccination behaviour of others. Depending of preferences for certain attributes, farm and farmer characteristics, and scores on attitude and perceived norms, each farmers has a unique update function.


## CREDITS
Dr Jaap Sok
Wageningen UR
The Netherlands
jaap.sok@wur.nl

Dr Egil A.J. Fischer
Utrecht University
The Netherlands
e.a.j.fischer@uu.nl

##REFERENCES
1. Sok, J., van der Lans, I. A., Hogeveen, H., Elbers, A. R. W., Oude Lansink, A. G. J. M. (2018). Farmers’ Preferences For Bluetongue Vaccination Scheme Attributes: An Integrated Choice and Latent Variable Approach. Journal of Agricultural Economics 69: 537-560.
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

house colonial
false
0
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 45 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 60 195 105 240
Rectangle -16777216 true false 60 150 105 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Polygon -7500403 true true 30 135 285 135 240 90 75 90
Line -16777216 false 30 135 285 135
Line -16777216 false 255 105 285 135
Line -7500403 true 154 195 154 255
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 135 150 180 180

house efficiency
false
0
Rectangle -7500403 true true 180 90 195 195
Rectangle -7500403 true true 90 165 210 255
Rectangle -16777216 true false 165 195 195 255
Rectangle -16777216 true false 105 202 135 240
Polygon -7500403 true true 225 165 75 165 150 90
Line -16777216 false 75 165 225 165

house ranch
false
0
Rectangle -7500403 true true 270 120 285 255
Rectangle -7500403 true true 15 180 270 255
Polygon -7500403 true true 0 180 300 180 240 135 60 135 0 180
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 45 195 105 240
Rectangle -16777216 true false 195 195 255 240
Line -7500403 true 75 195 75 240
Line -7500403 true 225 195 225 240
Line -16777216 false 270 180 270 255
Line -16777216 false 0 180 300 180

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

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

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
NetLogo 5.3
@#$#@#$#@
@#$#@#$#@
1.0
    org.nlogo.sdm.gui.AggregateDrawing 1
        org.nlogo.sdm.gui.ConverterFigure "attributes" "attributes" 1 "FillColor" "Color" 130 188 183 221 154 50 50
            org.nlogo.sdm.gui.WrappedConverter "" ""
@#$#@#$#@
<experiments>
  <experiment name="Default scheme" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <metric>total-dec-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="No vaccination" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.95"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="176"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="Threshold" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <steppedValueSet variable="threshold" first="0.915" step="0.001" last="0.925"/>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Schemes 1 -5_final_simulation analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.974"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="0"/>
      <value value="28"/>
      <value value="56"/>
      <value value="84"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Initial number of infectious farms_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="1"/>
      <value value="6"/>
      <value value="11"/>
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Default scheme with different levels of immunized farms from previous year" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percentage-immunized-last-year-farms" first="0" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Correcting V3 with SVAE being nil" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="28"/>
      <value value="56"/>
      <value value="84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Injunctive norm updating_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Infectious period_final_sensitivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="28"/>
      <value value="56"/>
      <value value="84"/>
      <value value="112"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Latent period_final_sensitivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="7"/>
      <value value="14"/>
      <value value="21"/>
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Minor farm group_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Small radius_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Large radius_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Network-size-sensitivity_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.25"/>
      <value value="0.5"/>
      <value value="0.75"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Initial transmission rate_final_sensitivity analysis" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="500"/>
      <value value="1000"/>
      <value value="1500"/>
      <value value="2000"/>
      <value value="2500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Injunctive norm updating under different schemes" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Willing to vaccinate" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-willing-to-vaccinate-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Retrieving vaccinated farm profiles for each scheme" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Experiment with the 5 schemes and social pressure on/off" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="0"/>
      <value value="28"/>
      <value value="56"/>
      <value value="84"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="The effect of social pressures on scheme 4 with varying weights A-IN" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>private-vacc-costs</metric>
    <metric>public-vacc-costs</metric>
    <metric>private-dis-costs</metric>
    <metric>total-costs</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-A-IN">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="The effect of social pressures with varying weights on updating IN" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>weekly-upd-SVAE-mean</metric>
    <metric>weekly-upd-INFO-mean</metric>
    <metric>weekly-upd-SUB-mean</metric>
    <metric>weekly-upd-SVAE-std.dev</metric>
    <metric>weekly-upd-INFO-std.dev</metric>
    <metric>weekly-upd-SUB-std.dev</metric>
    <metric>total-sus</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Uptake schemes with and without social pressure" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-farmsI">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Test" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-farmsI">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc3&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Calibrating threshold and social pressure to get a smooth vaccination uptake" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.25"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <steppedValueSet variable="threshold" first="0.97" step="0.002" last="0.98"/>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="5 scheme designs with and without SP" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>monthly-aggreg_util-INFO</metric>
    <metric>monthly-aggreg_util-SUB</metric>
    <metric>monthly-aggreg_util-SVAE</metric>
    <metric>monthly-aggreg_RP</metric>
    <metric>monthly-aggreg_SP</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="0"/>
      <value value="28"/>
      <value value="56"/>
      <value value="84"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.978"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Epidemiology sensitivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>monthly-aggreg_util-INFO</metric>
    <metric>monthly-aggreg_util-SUB</metric>
    <metric>monthly-aggreg_util-SVAE</metric>
    <metric>monthly-aggreg_RP</metric>
    <metric>monthly-aggreg_SP</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="750"/>
      <value value="1500"/>
      <value value="2250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.978"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
  </experiment>
  <experiment name="Network structure sensitivity" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>weekly-A4</metric>
    <metric>weekly-IN1</metric>
    <metric>weekly-DN2</metric>
    <metric>weekly-decided-vaccinated-farms</metric>
    <metric>weekly-immunized-vaccinated-farms</metric>
    <metric>weekly-infected-farms</metric>
    <metric>monthly-aggreg_util-INFO</metric>
    <metric>monthly-aggreg_util-SUB</metric>
    <metric>monthly-aggreg_util-SVAE</metric>
    <metric>monthly-aggreg_RP</metric>
    <metric>monthly-aggreg_SP</metric>
    <metric>vacc-moment-farms</metric>
    <metric>ID_dec-v_farms</metric>
    <metric>ID_imm-v_farms</metric>
    <metric>total-sus</metric>
    <metric>total-lat</metric>
    <metric>total-inf</metric>
    <metric>total-imm-d</metric>
    <metric>total-imm-v</metric>
    <enumeratedValueSet variable="farm-density">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-with-larger-reach">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="normal-reach">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="larger-reach">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labda0">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r0">
      <value value="3.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="end-spring">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-autumn">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-period">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lat-period">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-farms">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="influence-A-on-IN">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-size-sensitivity">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-perc-choice">
      <value value="&quot;risk-perc1&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-pressures">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-vaccination-scheme">
      <value value="56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.978"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SVAE">
      <value value="&quot;SML&quot;"/>
      <value value="&quot;NIL&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="INFO">
      <value value="&quot;LFL&quot;"/>
      <value value="&quot;VET&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SUB">
      <value value="&quot;10%&quot;"/>
      <value value="&quot;60%&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="COSTS">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="50"/>
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
1
@#$#@#$#@
