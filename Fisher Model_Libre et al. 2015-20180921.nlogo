;;version 11

globals [
          ;; Parameters
  time-step-fish ; (day) the time-step of fish growth and fish diffusion
  liters-per-mile; (liters/mile) liters of fuel used per mile of travel
  skipjack-price ; (USD/ton) price of skipjack tuna per ton
  skjharvest; (tons) total skipjack catch for the year

           ;; Variables
  x3 ; ratio of available sites to total sites (the same for all companies)
  x4 ; ratio of companies who bought a vessel + new entrants / total companies the previous year
  buyers ; number of companies who bought a vessel this year
  number-deciding-companies ; number of companies who made the buying-selling vessel decision
  selling-ratio; ratio of companies who sold a vessel / total companies
  total-industry-profit; (USD) sum of profit of all companies per year
  total-industry-catch; (tons) sum of catch of all vessels per year
  probability-entry; probability of a company entering the industry
  industry-catch-target-ratio
  entrants; number of companie that entered the industry
  exited ; number of companies that exited from the industry

]

patches-own
[
 patch-carrying-capacity ; (tons/patch) maximum tons of fish a patch can have
 fish-density ; (tons/patch) amount of fish in a patch
 fish-density-after-growth; (tons/patch) amount of fish after growth but before diffusion (needed for diffusion action)
 patch-distance ; (miles) distance from port
]

breed [companies company] ; main agent, decides on buying and selling of vessels,
breed [vessels vessel] ; owned by a company, does the harvest, spatially explicit

companies-own
[
  capital ; (USD) amount of capital available to company
  company-profit ; (USD) profit of company every end of the year
  x1 ; average of industry-catch-target-ratio and company-catch-target-ratio
  x2b ; capital coverage (how much of the company's capital covers cost of investment in one fleet)
  x2s ; company's ratio of earning vessels to total vessels
  probability-sell ;probability of selling a vessel
  probability-buy ;probability of buying a vessel
  risk-behaviour; [0,1] level of risk-taking behaviour influencing buying a vessel and choice of patch for vessel
  buy; (0,1) = 1 if company decides to buy a vessel, needed in compute-probabilities procedure and used in counting buying-ratio
  sell; (0,1) = 1 if company decides to sell a vessel
  earning-vessels-ratio; ratio of vessels with positive profit to total vessels of company
  company-catch-target-ratio
  own-vessels ; number of vessels company owns
]

vessels-own
[
 harvest-yes ; (0,1) = 1 if vessel decides to harvest, used to identify which vessel will proceed to the harvest procedure
 count-days-fishing; (days) number of days vessel is doing the harvest within the year
 catch-on-board ; (tons) total amount of fish on the vessel (skj + yft + other small pelagic fishes), emptied every transport to port
 catch-capacity-list; [ ] list of catch to capacity ratio, filled in every transport to port
 vessel-catch-capacity; average catch to capacity ratio of vessel within the year (average of items in the catch-capacity-list)
 yearly-vessel-catch ; (tons) vessel's total amount of fish catch for the year (skj + yft + other small pelagic fishes), updated every transport to port (i.e. all catch = transported to port)
 yearly-vessel-profit ; (USD) profit of vessel per year
 vessel-year ; age of the vessel, code needed to compute average-vessel-profit
 average-vessel-profit ; (USD) average yearly profit of the vessel (based on decay average formula, i.e. weight-recent * vessel-profit this year + (1- weight-recent) * average-vessel-profit previous year )
 my-company ; tags vessel to company, needed when company needs to get values from its vessels
]


;; This function sets-up the model
to setup
  clear-all
  reset-ticks
                 ;;In settings we set coordinates such that we have a 14x14 grid with total size of 176,400 square miles
                 ;; Set each patch's variable
  ask patches
    [
      set patch-carrying-capacity maximum-amount-fish-system / ( count patches) ; maximum amount of fish per patch (assumption: equal for all patches)
      set fish-density initial-fish-stock / ( count patches) ;; initial tons of fish per patch (assumption: equal for all patches)
      set pcolor scale-color blue fish-density patch-carrying-capacity 0; make color darker for patches with highest density
      set patch-distance ( ( 420 / sqrt (count patches) ) * (sqrt ( (abs(pycor))^ 2 + (abs(pxcor))^ 2 ) ) ) ; where 420/sqrt(196) = 30 miles (the length of one patch)
    ]
               ;; Set values of variables needed for fish processes (fish-growth and fish-diffuse)
  set time-step-fish (1 / 3) ;; fish process is done 3 times a day for stability purposes
  set skipjack-price initial-skipjack-price ; set skipjack price to the initial skipjack price
  set liters-per-mile 7; 7 liters is needed to travel one mile
  set-default-shape vessels "boat"
               ;; Create companies and their vessels
  repeat initial-companies [ if count patches with [ ( (count turtles-on neighbors / count neighbors) <= congestion-threshold) and count turtles-here = 0] > 0 [create-company]]
  reset-ticks
end

to nominal-values  ;; set all paremeters to their nominal values
  set initial-companies 15 ; (companies) number of companies at the start of the simulation
  set potential-entrants 10; (companies) maximum number of companies that can enter the system every end of the year (i.e. created)
  set resistance-to-exit 0.50 ; level of resistance to selling the last vessel (used in computing probability-buy)
  set risk-behaviour-left 0.4   ; left bound of the risk-behaviour uniform distribution
  set risk-behaviour-right 1.0 ; right bound of the risk-behaviour uniform distribution
  set weight-recent 0.80 ; weight given to the most recent year in computing average (decay average)
  set congestion-threshold 0.50 ; maximum congestion-ratio of a patch acceptable to companies (congestion-ratio = number of vessels around a patch / total patches around it)

  set weight-catch-buy 0.45; weight given to catch informaiton in probability to buy a vessel
  set weight-profit-buy 0.25; weight given to profit information in probability to buy a vessel
  set weight-buying-ratio 0.1; weight given to buying behaviour of all copmanies in probability to buy a vessel
  set weight-site-buy 0.1 ; weight given to available sites information in probability to buy a vessel
  set weight-risk 0.1 ; weight given to risk-behaviour in probability to buy a vessel

  set weight-catch-sell 0.7; weight given to catch information in probability to sell a vessel
  set weight-profit-sell 0.30; weight given to profit information in probability to buy a vessel

  set weight-buyingratio-entry 0.2; value given to buying behaviour of all companies in probability of entry of potential companies
  set weight-catch-entry 0.6 ; value given to catch information in probability of entry of potential companies
  set weight-site-entry 0.20 ; weight given to available sites in probability of entry of potential companies

  set starting-capital 27000 ; (USD) initial capital of the company (after buying its first vessels)
  set investment-cost 1700000 ; (USD) cost of one investment (fleet + FADs)
  set second-hand-ratio 0.30 ;ratio of price of second hand vessel / new vessel
  set fixedcost 23000 ; (USD/vessel/year) cost of maintaining each vessel in a year
  set fuel-cost-liter 1.01 ; (USD/liter) price of fuel
  set labor-share 0.10; share of the revenue which goes to the crew
  set fuel-share 0.60 ;; based on avarege of interviews

  set initial-skipjack-price 1661  ;; (USD) initial price of the skipjack per tonnage (based on Owen 2001 regression)
  set delta-skipjack-price-catch -0.00041 ;; change in price per 1 unit change in total cattch (based on Owen 2001)
  set yellowfin-skipjack-price-ratio 1.21 ; ratio of juvenile yellowfin price to skipjack price. Yellowfin here is the juvenile yellowfin caught by purse seiners, and not the sashimi-grade yellowfin
  set otherfish-skipjack-price-ratio 0.86 ; ratio of other small pelagic fishes prices to skipjack price. Purse seines also catch small pelagic fishes

  set vessel-capacity 60 ; (tons) the maximum amount of fish each vessel can hold
  set skipjack-ratio-to-catch 0.56 ;; 56% of total catch is skipjack
  set yellowfin-ratio-to-catch 0.13 ;; 13% of total catch is juvenile yellowfish
  set otherfish-ratio-to-catch 0.31 ;; 31% of total catch is other fish
  set target-catch-capacity 0.40 ; target ratio of amount of fish catch to vessel-capacity

  set maximum-amount-fish-system 48369 ;; estimated carrying capacity based on WCPFC region 2 stock assessment of skipjack tuna
  set initial-fish-stock  33205 ;; rounded off (from 33205) initial fish stock based on Q4 of1974
  set daily-growth-rate 0.00224 ;; (/day) growth rate of fish population per day
  set daily-diffusivity 310.56;; (square miles/day) rate of diffusion to adjacent site per time-step
  set harvest-ratio-mean 0.013 ; average of the harvest-ratio distribution (how much of the fish in the patch can be caught)
  set harvest-ratio-standard-deviation 0.004
end


;; To go is the model process
to go
                ;;Weekly process, i.e. first day of week: each vessel makes a harvest-decision
  if ticks mod 7 = 0 or ticks = 0 [ ; coded using the modulo operation (i.e. remainder of ticks/7).
                                    ; In Netlogo 1st week is ticks 0 to 6, 2nd week is 7 to 13, 3rd week is 14 to 27, and so on. So 14 mod 7 = 0 (beginning of the week)
    harvest-decision
    ]
                ;; Daily processes: harvest, fish-growth and fish-diffuse
  harvest-action
  repeat 3 [     ; fish process is repeated 3 times a day to satisfy stability conditions
    fish-growth  ; fish grows according to logistic equation
    fish-diffuse ; fish of patch diffuses to 4 neighbouring patches
   ]
                ;;Yearly process: last day of the year
  if ticks mod 365 = 364 or ticks = 364 [ ; coded using modulo operation. Year 1: ticks 0 to 364, year 2: ticks 365 to 729; example: 729 mod 365 = 364)
    update-vessel-variables               ; computation of vessel variables at the end of the year
    update-fishery-output-variables       ; computation of year-end fishery output level variables
    update-company-variables              ; computation of company variables at the end of the year
    compute-probabilities                 ; computation of company's probability-sell and probability-buy
    vessel-investment                     ; actual decision to buy or sell a vessel or do nothing
    company-entry                         ; new company being created (based on probability)
    ]
  tick
end

              ;;Harvest-decision is a vessel action of deciding whether or not to harvest the coming week
              ;;This is done every first day of the week
              ;;The order of which vessel acts first is random
              ;;Variables are made local if it is used only in this procedure (not needed in any other part of the code nor for analysis purposes)
to harvest-decision
   ask vessels [
              ;; First, fisher computes expected harvest
              ;; the numerator is expected skipjack harvest based on the harvest-ratio-mean (fisher knows this based on experience) and fish-density of patch (assume: he can estimate this)
              ;; we divide it by skipjack-ratio-to-catch, assumming that skipjack catch is a fixed ratio of total catch
    let total-expected-harvest ( harvest-ratio-mean * [fish-density] of patch-here / skipjack-ratio-to-catch)
              ; Next, fisher computes harvest-target which is the minimum required harvest for a day ( we divide it by 7 because there are 7 days in one week of fishing trip)
              ; This is based on the target-catch-capacity set by the company (assumed to be equal for all companies)
    let harvest-target ( target-catch-capacity * vessel-capacity / 7)
              ; Now, it compares expected harvest with harvest threshold.
              ; If expected > = threshold, then it will decide to harvest
    ifelse total-expected-harvest >= harvest-target [
      set harvest-yes  1   ; harvest
      ]
      [ ;; If expected < threshold, vessel can still harvest given probability-harvest which is the ratio of expected/threshold
        ;; The closer the expected to threshold is (i.e. value is closer to 1), the higher the probability of a vessel harvesting
      let probability-harvest (total-expected-harvest / harvest-target)
      ifelse probability-harvest > random-float 1 [
        set harvest-yes 1 ; harvest
        ]
        [
        set harvest-yes 0 ; do not harvest
        ]
      ]
   ]
end
               ;; Harvest is the process of harvesting fish from the patch
               ;; This is a daily procedure
               ;; The order of which vessel acts first is random
               ;; Variables are made local if it is used only in this procedure (not needed in any other part of the code nor for analysis purposes)
to harvest-action
               ;; First, we ask all vessels to reset their count-fishing-days to zero at the beginning of the year (ticks 0, 265, 730, 1095, ...)
  ask vessels [
    if ticks mod 365 = 0 or ticks = 0 [
     set count-days-fishing 0
    ]
  ]
               ;; In every time step (daiy), we ask vessels with harvest-yes = 1 to harvest fish from the patch
   ask vessels with [ harvest-yes = 1 ];  those that have decided to harvest
    [          ;; Add one day to count-days-fishing
     set count-days-fishing count-days-fishing + 1
               ;; Draw value for harvest-ratio (how much of the skipjack fish from the patch is harvested)
     let harvest-ratio random-gamma (harvest-ratio-mean * harvest-ratio-mean / (harvest-ratio-standard-deviation * harvest-ratio-standard-deviation)) (harvest-ratio-mean / (harvest-ratio-standard-deviation * harvest-ratio-standard-deviation))

               ;; From the harvest-ratio and fish-density of the vessel's patch, and remaining space in the vessel, we can compute the amount of skipjack catch
               ;; Amount of skipjack catch is the minimum of what can be harvested out of the patch (harvest-ratio * fish density) and how much space is left in the vessel ( second term on the list)
     let skj-vessel-catch min (list ( harvest-ratio  * [fish-density] of patch-here) ( skipjack-ratio-to-catch * (vessel-capacity - catch-on-board )) )
               ;; The amount of skipjack harvested by the vessel is deducted from the fish density of the patch where it is located
     ask patch-here [
        set fish-density (fish-density - [skj-vessel-catch] of myself )   ;remove amount of fish catch from patch density
      ]
               ;; We add this daily catch to the catch-on-board (including yellowfin and other small pelagic fishes based on skipjack-ratio-to-catch)
               ;; Total catch is skj-vessel-catch / skipjack-ratio-to-catch because we assume a fixed ratio of skipjack to total catch
     set catch-on-board catch-on-board + ( skj-vessel-catch / skipjack-ratio-to-catch)
             ;; If the vessel is full, then transport the catch to the port and stop harvesting for the remaning week
     if catch-on-board >= vessel-capacity [
       transport-to-port     ;; proceed to transport-to-port procedure
       set harvest-yes 0     ;; reset harvest decision to zero (stop harvest for the week)
     ]
              ;; Transport catch to port at the end of the week even if vessel is not full
              ;; Week1: ticks 0 to 6, week2: ticks 7 to 13, week3: ticks 14 to 27. 13 mod 7 = 6, 27 mod 7 = 6
     if ( ticks mod 7 = 6 or ticks = 6) and harvest-yes = 1[
       transport-to-port     ;; proceed to transport-to-port procedure
       ]
     ]
end
                 ;;Transport-to-port is a procedure nested in harvest-action procedure (a vessel which decides to transport-to-port in the harvest-action procedure proceeds to this procedure)
                 ;;This is the process of a vessel transporting its catch-on-board to the port
                 ;; Variables are made local if it is used only in this procedure (not needed in any other part of the code nor for analysis purposes)
to transport-to-port ;; the process of transporting the catch-on-board to the port
                 ;; Every transport-to-port, the vessel earns a trip revenue from its skipjack, yellowfin, and other small pelagic fish catch
    let trip-revenue (catch-on-board * (skipjack-price * skipjack-ratio-to-catch + skipjack-price * yellowfin-skipjack-price-ratio * yellowfin-ratio-to-catch + skipjack-price * otherfish-skipjack-price-ratio * otherfish-ratio-to-catch) )
                 ;; The vessel also incurs trip-expenses (labor cost + operating expenses)
                 ;; Labor cost is a share of the trip-revenue
                 ;; Operating expenses is based on fuel cost and how much fuel cost is as a ratio to total operating expenses
    let trip-expenses ( labor-share * trip-revenue + ((fuel-cost-liter * liters-per-mile * [ patch-distance] of patch-here ) / fuel-share ) )
                 ;; The trip profit (trip-revenue - trip-expenses) is added to the yearly-vessel-profit (profit of the vessel for the year)
    set yearly-vessel-profit yearly-vessel-profit + (trip-revenue - trip-expenses)

                 ;; The amount transported to port (catch-on-board) is added to the yearly-vessel-catch (catch of the vessel for the year)
    set yearly-vessel-catch ( yearly-vessel-catch + catch-on-board) ; add transported fish to total catch for the year
                 ;; We create a  list of the catch to vessel capacity ratio every transport (this is filled with the values within the year)
    set catch-capacity-list fput ( catch-on-board / vessel-capacity) catch-capacity-list
                 ;; Lastly, reset catch-on-board (i.e. remove fish from vessel to port)
    set catch-on-board 0
end

                 ;; Fish-growth is the process of each patch updating its fish density through the logistic growth equation
                 ;; This process is repeated three times per time step (3 times per day)
                 ;; The order of which patch acts first is random
to fish-growth
  ask patches [
     set fish-density-after-growth ( fish-density * patch-carrying-capacity * exp(daily-growth-rate * time-step-fish) / (patch-carrying-capacity + fish-density * (exp (daily-growth-rate * time-step-fish) - 1)) )
  ]
end
                 ;; Fish-diffuse is the process of each patch upating its fish density through diffusion
                 ;; This process is repeated three times per time step
                 ;; The order of which patch acts first is random
to fish-diffuse
  ask patches [
                 ;; New fish density is original fish density - fish that diffused to its four neighbors (above, below, right, left) + fish that diffused from its four neighbors
                 ;; daily diffusivity is multiplied by time-step-fish (delta t-prime in model description) and divided by the square of patch-length (30 miles)
    set fish-density (1 - count neighbors4 * ( daily-diffusivity * time-step-fish / ( (30)^ 2)) ) * fish-density-after-growth + ( daily-diffusivity * time-step-fish / ( (30)^ 2)) * sum [fish-density-after-growth] of neighbors4
                 ;; For visual purpose, we recolor patch after diffusion to indicate fish density
    set pcolor scale-color blue fish-density patch-carrying-capacity 0
  ]
end

                 ;;Update vessel variables is the process of each vessel computing year-end variables
                 ;; This process is done every last day of the year
                 ;; The order of which vessel acts first is random
to update-vessel-variables
  ask vessels [
                 ;; (1) The profit of a vessel at the end of the year is its yearly-vessel-profit less the fixedcost (annual fixed cost)
    set yearly-vessel-profit yearly-vessel-profit - fixedcost
                 ;; (2) we compute the average-vessel-profit based on decay average formula.
                 ;; average-vessel-profit is needed when choosing which vessel to sell
    ifelse vessel-year = 1 [
                 ;; For the first year, the average is just the value of the first year
       set average-vessel-profit yearly-vessel-profit
       ]
      [          ;; For year 2 onwards, average is computed using decay average formula, with greater weight given to most recent value
      set average-vessel-profit ( ( weight-recent * yearly-vessel-profit) + (1 - weight-recent) * (average-vessel-profit))
      ]
                 ;; (3) We compute the average ratio of catch to capacity of a vessel within the year (from catch-capacity-list)
      ifelse empty? catch-capacity-list [
                 ;; if this list is empty (because there has been no catch), then vessel's average catch/capacity is zero
       set vessel-catch-capacity 0
     ]
     [           ;; if it is not empty, then we get the average of the items in the list (1 year worth of list)
       set vessel-catch-capacity mean catch-capacity-list
     ]
                ;; add one  year to the age of the vessel
    set vessel-year vessel-year + 1
                ;; at the end of one year, we empty the catch-capacity-list (ready for next year's list)
    set catch-capacity-list [ ]
  ]
end

                 ;; Update-fishery-variables is the procedure when system-level variables are computed
                 ;; This is done every last day of the year, before any (dis)investment decision and company entry
to update-fishery-output-variables
                 ;; (1) we update the price of skipjack. This is done by first computing how much skipjack was harvetsed for the year
                 ;; Skipjack harvest is equal to the fixed skipjack-ratio-to-catch multiplied by the total catch of all vessels
  set skjharvest skipjack-ratio-to-catch * sum [yearly-vessel-catch] of vessels
                 ;; Price is computed using the equation based on regression of Bangkok skipjack prices and catch by Owen (2001), data used by OWEN was from 1984 to 1999.
  set skipjack-price ( initial-skipjack-price + (delta-skipjack-price-catch * skjharvest ) )
                ;; (2) we compute the total industry profit which is the sum of yearly profit of all vessels
  set total-industry-profit sum [yearly-vessel-profit] of vessels
                ;; (3) we compute total industry catch which is the sum of yearly catch of all vessels
  set total-industry-catch sum [yearly-vessel-catch] of vessels

                ;; (4) Compute the ratio of the average catch to capacity of all vessels to the target catch capacity
  ifelse count vessels = 0 [
                ;; if there is no vessel, then the industry's catch to target ratio is zero
      set industry-catch-target-ratio 0
      ]
     [         ;; If there are vessels, we compute it as the ratio of the average vessel-catch-capacity of all vessels to the target-catch-capacity
        ifelse ( (mean [vessel-catch-capacity] of vessels / target-catch-capacity ) < 1 ) [
            set industry-catch-target-ratio ( mean [vessel-catch-capacity] of vessels / target-catch-capacity )
        ]
                ;; if average > target, then vessels have, on average, met the target and thus industry-catch-target-ratio is 1 (they've met the target)
        [
            set industry-catch-target-ratio 1
         ]
       ]

end


to update-company-variables
  ask companies [
                 ;; (1) We add year-end profit of all the company's vessels to its capital
    set capital capital + sum [yearly-vessel-profit] of vessels with [my-company = myself]

                ;; (2) Compute company's catch to target ratio based on the average vessel/catch capcity of its vessels
    ifelse ( mean [vessel-catch-capacity] of vessels with [my-company = myself] / target-catch-capacity ) < 1 [
        set company-catch-target-ratio ( mean [vessel-catch-capacity] of vessels with [my-company = myself] / target-catch-capacity )
       ]
       [         ;; if company met its target, then this ratio is equal to 1
          set company-catch-target-ratio 1
       ]
  ]


end
             ;; Compute-probabilities is the procedure of each company computing its probability to sell and probability to buy a vessel
                 ;; This is done every last day of the year
                 ;; The order of which company acts first is random
to compute-probabilities
             ;; We compute x3 (ratio of available sites to total sites), and x4 (buying ratio) which is the same for all companies, and will be used to compute probability to buy and sell a vessel
       set x3 count patches with [ (count turtles-on neighbors / count neighbors) <= congestion-threshold and count turtles-here = 0 ] / count patches
             ;; x4: this is the ratio of companies that added a vessel (including new entrants) last year to the initial number of companies last year
             ;; We compute x4 only after year 1 (since there is still no historical information at the end of year 1)
     if ticks > 364 [
       ifelse (buyers + entrants) > number-deciding-companies [
             ;; if buyers (buyers from the initial companies last year) + entrants > initial number of companies last year, then x4 = 1
         set x4 1
       ]
       [
         ifelse number-deciding-companies > 0 [
            set x4 ( buyers + entrants ) / number-deciding-companies
         ]
         [ set x4 0]  ;;if there was no company the previous year and also no buyer and entrant this year, then x4 = 0
        ]

     ]

            ;; Now each company computes x1 and x2 (which varies across companies), and its probability to sell and buy a vessel
  ask companies
    [
             ;; x1 (catch to target ratio) is the average of company-catch-target-ratio and industry-catch-target-ratio, this is the catch information used by the company
      set x1 ( industry-catch-target-ratio + company-catch-target-ratio ) / 2
             ;; x2b is capital coverage, and is used as the profitability information in probability to buy a vessel
             ;; if capital >= cost of investment, then company has enough capital to cover cost of investmetn (so x2b is 1)
      ifelse capital >= investment-cost [
        set x2b 1
      ]
      [
         set x2b capital / investment-cost
      ]
             ;; x2s is ratio of earning vessels, and is used as the profitability information in probability to sell a vessel
      set x2s (  count vessels with [yearly-vessel-profit > 0 and my-company = myself] / count vessels with [my-company = myself] )

             ;; Probability-sell computation is as follows:
             ;; If company has any vessel with average-vessel-profit <= 0 (has not been earning for some time), then it will automaticall sell a vessel
      ifelse any? vessels with [average-vessel-profit <= 0 and my-company = myself][
        set probability-sell 1
      ]
      [      ;; Otherwise, it computes probability-sell based on x1, and x2s, and resistance to exit
             ;; We get the weighted average of x1 and x2s
             ;; Then, we multiply this with a ( 1 - resistance-to-exit/vessel). resistance to exit decreases the value of probability-sell, reflecting reluctance of companies to sell a vessel
        set probability-sell ( weight-catch-sell * (1 - x1) + weight-profit-sell * (1 - x2s) ) * ( 1 - ( resistance-to-exit / count vessels with [my-company = myself]))]

             ;; Probability-buy computation is as follows
             ;; If there is no available patch and this year's average catch per vessel is zero, then probability-buy is zero
      ifelse x3 = 0 [
        set probability-buy 0
      ]
      [      ;; Otherwise, it computes probability-buy based on x1, x2b, x3, x4, and risk-behaviour
        ifelse ticks = 364 [
             ;; During the first year of simulation (ticks = 364), there is still no historical information on buying-ratio (x4). Thus, this is not included in the first year of computation
             ;; The weight-buying-ratio is then divided equally to the other four weights for x1, x2b, x3, and risk-behaviour
          let additional-weight weight-buying-ratio / 4
          set probability-buy (  ( ( weight-catch-buy + additional-weight) * x1 + (weight-profit-buy + additional-weight) * x2b + ( weight-site-buy + additional-weight) * x3 + (weight-risk + additional-weight) * risk-behaviour ) * (1 - probability-sell) )
          ]
        [    ;; From year two onwards, all five variables are included in the computation
          set probability-buy (  ( weight-catch-buy * x1 + weight-profit-buy * x2b + weight-site-buy * x3 + weight-buying-ratio * x4 + weight-risk * risk-behaviour ) * (1 - probability-sell) )
           ]
      ]
    ]


               ;; After computation of probabilities, we can now reset yearly vessel variables catch profit to zero since they are no longer needed.
ask vessels  [
    set yearly-vessel-catch 0
    set yearly-vessel-profit 0
  ]
end

                ;; Vesel-investment is the process of each company deciding to sell or buy a vessel, based on its computed probabilities
                ;; This is done every last day of the year
                ;; The order of which company acts first is from the company with the highest probability-buy to the lowest probability-buy
to vessel-investment
  ;; We ask companies that has been sorted from highest to lowest probability to buy
  foreach sort-on [(- probability-buy)] companies [ ?1 ->
    ask ?1 [
      ;; First check if company wants to sell a vessel
      ifelse probability-sell > random-float 1 [
                ;; If company chooses to sell a vessel, it proceeds to sell-vessel procedure
        sell-vessel
        set sell 1
           ]
      [
                ;;If it did not choose to sell a vessel, it will check first if there is any available patch
        if any? patches with [ ((count turtles-on neighbors / count neighbors) <= congestion-threshold ) and count turtles-here = 0] [
                ;; If there is an available patch, it will decide whether or not to buy a vessel given a probability-buy
          if ( probability-buy > random-float 1 ) [
              set buy 1
                ;; We assume that a company that decides to buy a vessel will have enough capital (whether own capital or getting extra capital)
                ;; Company will get additional capital if its own capital is not sufficient
              if capital < investment-cost [
                set capital ( capital + ( investment-cost - capital) )
              ]
              buy-vessel
             ]
            ]
          ]
             ;; ask each company to count how many vessels it owns to track how many companies exited from the industry
        set own-vessels count vessels with [my-company = myself]
       ]
      ]      ;; end of code asking a sorted company


             ;; After all companies have made their (dis)investment decision, we count the number of companies who bought a vessel and companies which made a decision
    if count companies > 0 [
      set buyers ( count companies with [buy = 1] ); number of companies who bought a vessel this year
      set number-deciding-companies ( count companies ) ; this will be used in the next year's buying ratio computation (thus the last year name)
             ;; We also compute selling ratio (this is not used in any equation in the model, but we track this variable for analysis purposes)
      set selling-ratio (count companies with [sell = 1 ] / count companies)
             ;; We also count how many companies exited from the industry (not used in model equation, but tracked for analysis purposes)
      set exited count companies with [own-vessels = 0]

     ]

            ;; Once we have counted buyers, selling ratio, and number of companies who exited, we can reset the buy and sell identifies of each company
            ;; We can also now remove the companies with zero vessels
  ask companies [
    set buy  0
    set sell 0
            ;; after counting, ask companies with no vessel to exit from the industry
    if count vessels with [my-company = myself] = 0 [
      die ; company with no vessel dies
    ]
  ]

end

              ;; Sell-vessel is the process of a company selling one of its vessels
              ;; This procedure is nested in vessel-investment process, and is only done by a company if it calls this action
to sell-vessel
              ;; the company sells the vessel (let's it die) with the lowest average-vessel-profit
  ask min-one-of (vessels with [my-company = myself]) [average-vessel-profit] [
    die ; vessel with lowest profit is sold
   ]
              ;; We add the sales from the sold vessel to the company's capital
  set capital capital + second-hand-ratio * investment-cost

end
              ;; Buy-vessel is the process of a company buying one vessel
              ;; This procedure is nested in vessel-investment process, and is only done by a company if it calls this action
              ;; This procedure is also nested in create-company process (i.e. a new company also buys a vessel to start its operation)
to buy-vessel
              ;; We deduct the investment-cost from the company's capital
  set capital capital - ( investment-cost)
              ;; We create one vessel and set its initial variables
  hatch-vessels 1 [
    set vessel-year 1 ; first year of vessel
    set size 1
    set my-company myself ;;set vessel's company to the company creating it
    set catch-on-board 0 ; initial catch on board is zero
    set yearly-vessel-profit 0 ; initial profit is zero
    set yearly-vessel-catch 0 ; initial catch is zero
    set catch-capacity-list [ ] ; list of the vessels catch to capacity ratio
              ;;Next, we compute the company's preferred distance from the port to decide where to place the new vessel
              ;; More risk-taking companies will have a longer preferred-distance (i.e. they are more willing to be farther from the port)
    let preferred-distance ( [risk-behaviour] of my-company * max [patch-distance] of patches )
              ;; If there is still an available patch within the company's preferred distance, then we place the vessel on any available patch within preferred distance
    ifelse count patches with [ (patch-distance <= preferred-distance ) and (count turtles-on neighbors / count neighbors <= congestion-threshold and count turtles-here = 0) ] > 0  [ ;; count preferred available patches for company > 0
      move-to one-of patches with [ (count turtles-on neighbors / count neighbors <= congestion-threshold) and (count turtles-here = 0) and (patch-distance <= preferred-distance) ]
    ]
    [        ;; If there is no more available patch within preferred distance, just place vessel on any available patch in the system
      move-to one-of patches with [ count turtles-on neighbors / count neighbors <= congestion-threshold and count turtles-here = 0  ]
    ]
  ]
end

              ;; Company-entry is the process of deciding whether or not a new company enters the system
              ;; this procedure is done every last day of the year
to company-entry
              ;; We repeat this procedure for every potential entrant (i.e. potential-entrants it the maximum possible new companies created every last day of the year)
  set entrants 0 ; we start with zero entrants this year
  repeat potential-entrants [
             ;; If there is still an available patch then procedure proceeds
             ;; If there is no more available patch, company-entry proceedure for this potential entrant is stopped
    if any? patches with [ ( (count turtles-on neighbors / count neighbors) <= congestion-threshold ) and count turtles-here = 0] [
             ;; If there is still a company in the system, then probability of entry is based on the experience of these existing companies
        ifelse number-deciding-companies > 0 [
                               ;; available-patch-ratio is the ratio of available patches to total patches
            let available-patch-ratio count patches with [count turtles-on neighbors / count neighbors <= congestion-threshold and count turtles-here = 0] / count patches
            set probability-entry ( weight-site-entry * available-patch-ratio + weight-buyingratio-entry *  (buyers / number-deciding-companies) +   weight-catch-entry * industry-catch-target-ratio )
          ]
         [  ;; If there was no company (prior to any entry this year) in the system, a potential entrant will look at abundance of fish in the unfished system
           let expected-harvest-per-patch harvest-ratio-mean * mean [fish-density] of patches / skipjack-ratio-to-catch
           set probability-entry expected-harvest-per-patch / ( target-catch-capacity * vessel-capacity / 7)
           if probability-entry > 1 [ ; if expected > target, probability of entry is 1
             set probability-entry 1
           ]
         ]
                  ;; Given probability-entry, potential entrant decides whether or not to enter
        if random-float 1 < probability-entry [
        create-company
        set entrants entrants + 1
        ]
     ]

  ]
end

             ;;Create company is the process of creating one company
             ;;this procedure is nested in the set-up (when we initialize the mode)
             ;; this procedure is also nested in company-entry (if a new company enters, then we create a company)
to create-company
  create-companies 1 [                         ;; We set the initial values of company variables
    setxy 0 0                                  ;; position companies at the port
    set color one-of base-colors ;
              ;; risk-behaviour of company is drawn from a uniform distribution
    set risk-behaviour risk-behaviour-left + random-float (risk-behaviour-right - risk-behaviour-left)
              ;; the company starts with either 1 or 2 vessels
    ifelse random-float 1 < 0.90 [
      buy-vessel
    ]
    [
      buy-vessel
            ;; only buy the second vessel if there is still an available patch
      if count patches with [ ( (count turtles-on neighbors / count neighbors) <= congestion-threshold) and count turtles-here = 0] > 0 [
        buy-vessel
      ]
    ]
              ;; company starts with starting-capital (amount after buying its vessels)
    set capital starting-capital
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
833
10
1109
287
-1
-1
19.143
1
10
1
1
1
0
0
0
1
-13
0
-13
0
0
0
1
ticks
30.0

BUTTON
9
10
72
43
NIL
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
79
11
134
44
NIL
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
9
90
181
123
initial-companies
initial-companies
0
70
15.0
1
1
companies
HORIZONTAL

PLOT
1370
12
1610
171
Fish stock
Year
Tons
0.0
10.0
0.0
3.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ( sum [fish-density] of patches )"

BUTTON
139
11
214
44
Go forever
Go
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
7
420
173
453
starting-capital
starting-capital
0
1700000
27000.0
10000
1
USD
HORIZONTAL

SLIDER
7
528
175
561
fixedcost
fixedcost
10000
200000
23000.0
2000
1
USD/year
HORIZONTAL

SLIDER
7
565
175
598
fuel-cost-liter
fuel-cost-liter
0.5
5
1.01
0.25
1
USD/liter
HORIZONTAL

SLIDER
660
585
849
618
harvest-ratio-mean
harvest-ratio-mean
0.0025
0.06
0.013
0.0025
1
/day
HORIZONTAL

SLIDER
9
455
175
488
investment-cost
investment-cost
200000
3000000
1700000.0
500000
1
USD
HORIZONTAL

SLIDER
190
91
362
124
potential-entrants
potential-entrants
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
444
418
597
451
vessel-capacity
vessel-capacity
10
500
60.0
10
1
tons
HORIZONTAL

SLIDER
445
458
598
491
target-catch-capacity
target-catch-capacity
0.01
1
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
8
157
163
190
resistance-to-exit
resistance-to-exit
0
1
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
7
231
158
264
congestion-threshold
congestion-threshold
0
1
0.5
0.125
1
NIL
HORIZONTAL

BUTTON
222
12
286
45
Go 7 days
repeat 7 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
288
12
407
45
Go 365 days
repeat 365 [go]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
1123
10
1363
170
Number of vessels
NIL
NIL
0.0
10.0
0.0
50.0
true
false
"" ""
PENS
"vessels" 1.0 0 -16777216 true "" "if ticks mod 365 = 364 or ticks = 364 [ plot count vessels]"

PLOT
1368
326
1609
474
Total industry profit (year-end)
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
"default" 1.0 0 -16777216 true "" "if ticks mod 365 = 364 or ticks = 364 [plot sum [yearly-vessel-profit] of vessels]"

SLIDER
340
153
473
186
weight-catch-sell
weight-catch-sell
0
1
0.7
0.05
1
NIL
HORIZONTAL

SLIDER
177
155
321
188
weight-catch-buy
weight-catch-buy
0
1
0.45
0.05
1
NIL
HORIZONTAL

SLIDER
178
230
322
263
weight-buying-ratio
weight-buying-ratio
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
178
267
322
300
weight-site-buy
weight-site-buy
0
1
0.1
0.05
1
NIL
HORIZONTAL

PLOT
1127
327
1365
476
Total days fishing
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
"default" 1.0 0 -16777216 true "" "if ticks mod 365 = 364 or ticks = 364 [plot sum [count-days-fishing] of vessels]"

PLOT
1376
180
1605
320
Total catch
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
"default" 1.0 0 -16777216 true "" "if ticks mod 365 = 364 or ticks = 364 [plot sum [yearly-vessel-catch] of vessels]"

MONITOR
1271
108
1359
153
NIL
count vessels
17
1
11

SLIDER
513
151
674
184
weight-buyingratio-entry
weight-buyingratio-entry
0
5
0.2
1
1
NIL
HORIZONTAL

MONITOR
1534
111
1601
156
Fish stock
sum [fish-density] of patches
17
1
11

SLIDER
512
191
673
224
weight-catch-entry
weight-catch-entry
0
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
7
196
160
229
weight-recent
weight-recent
0
1
0.8
0.05
1
NIL
HORIZONTAL

SLIDER
178
191
320
224
weight-profit-buy
weight-profit-buy
0
1
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
340
190
472
223
weight-profit-sell
weight-profit-sell
0
1
0.3
0.05
1
NIL
HORIZONTAL

PLOT
1125
176
1365
317
Number of companies
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
"default" 1.0 0 -16777216 true "" "if ticks mod 365 = 364 or ticks = 364 [\nplot count companies]"

SLIDER
512
227
671
260
weight-site-entry
weight-site-entry
0
1
0.2
0.05
1
NIL
HORIZONTAL

SLIDER
7
601
177
634
labor-share
labor-share
0.05
0.95
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
6
637
177
670
fuel-share
fuel-share
0.05
0.95
0.6
0.05
1
NIL
HORIZONTAL

SLIDER
198
420
410
453
initial-skipjack-price
initial-skipjack-price
400
2400
1661.0
100
1
USD/ton
HORIZONTAL

SLIDER
197
456
406
489
delta-skipjack-price-catch
delta-skipjack-price-catch
-0.001
-0.0001
-4.1E-4
0.00001
1
NIL
HORIZONTAL

SLIDER
200
498
382
531
yellowfin-skipjack-price-ratio
yellowfin-skipjack-price-ratio
0.5
5
1.21
0.25
1
NIL
HORIZONTAL

SLIDER
199
539
383
572
otherfish-skipjack-price-ratio
otherfish-skipjack-price-ratio
0.25
3
0.86
0.25
1
NIL
HORIZONTAL

SLIDER
445
500
608
533
skipjack-ratio-to-catch
skipjack-ratio-to-catch
0.1
1
0.56
0.05
1
NIL
HORIZONTAL

SLIDER
444
538
609
571
yellowfin-ratio-to-catch
yellowfin-ratio-to-catch
0
.9
0.13
0.05
1
NIL
HORIZONTAL

SLIDER
658
411
880
444
maximum-amount-fish-system
maximum-amount-fish-system
12500
200000
48369.0
500
1
tons
HORIZONTAL

SLIDER
659
447
847
480
initial-fish-stock
initial-fish-stock
2000
48369
33205.0
1000
1
tons
HORIZONTAL

SLIDER
660
513
849
546
daily-growth-rate
daily-growth-rate
0.0005
0.0095
0.00224
0.00001
1
/day
HORIZONTAL

SLIDER
660
551
875
584
daily-diffusivity
daily-diffusivity
30
500
310.56
100
1
square miles/day
HORIZONTAL

BUTTON
8
45
120
78
nominal values
nominal-values
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
15
134
165
152
Company parameters
11
0.0
1

TEXTBOX
17
393
167
411
Cost parameters
11
0.0
1

TEXTBOX
210
391
360
409
Price parameters
11
0.0
1

TEXTBOX
444
387
594
405
Vessel parameters
11
0.0
1

TEXTBOX
662
387
812
405
Ecological parameters
11
0.0
1

TEXTBOX
179
133
287
151
P(buy) parameters
11
0.0
1

TEXTBOX
343
132
438
150
P(sell) parameters
11
0.0
1

TEXTBOX
518
129
668
147
P(entry) parameters
11
0.0
1

SLIDER
8
268
159
301
risk-behaviour-left
risk-behaviour-left
0.1
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
8
302
159
335
risk-behaviour-right
risk-behaviour-right
0.1
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
445
574
610
607
otherfish-ratio-to-catch
otherfish-ratio-to-catch
0
.9
0.31
0.05
1
NIL
HORIZONTAL

SLIDER
660
620
900
653
harvest-ratio-standard-deviation
harvest-ratio-standard-deviation
0.001
0.016
0.004
0.001
1
/day
HORIZONTAL

SLIDER
7
495
179
528
second-hand-ratio
second-hand-ratio
0.1
1
0.3
0.05
1
NIL
HORIZONTAL

TEXTBOX
683
485
889
513
initial fish stock must be <= maximum fish
11
0.0
1

TEXTBOX
34
345
184
373
risk-behaviour left <= risk-behaviour-right
11
0.0
1

SLIDER
179
302
321
335
weight-risk
weight-risk
0
1
0.1
0.05
1
NIL
HORIZONTAL

MONITOR
177
340
290
385
sum of buying weights
weight-catch-buy + weight-profit-buy + weight-buying-ratio + weight-site-buy + weight-risk
3
1
11

MONITOR
340
237
463
282
sum of selling weights
weight-catch-sell + weight-profit-sell
17
1
11

MONITOR
512
268
662
313
sum of entry weights
weight-buyingratio-entry + weight-catch-entry + weight-site-entry
17
1
11

MONITOR
444
614
605
659
sum of catch ratios (must be 1)
skipjack-ratio-to-catch + yellowfin-ratio-to-catch + otherfish-ratio-to-catch
5
1
11

MONITOR
1254
258
1359
303
NIL
count companies
17
1
11

@#$#@#$#@
## WHAT IS IT?

TunaFisher ABM simulates the decisions of fishing companies and fishing vessels of the Philippine tuna purse seinery operating in the Celebes and Sulu Seas. 

High fishing effort remains in many of the world’s fisheries, including the Philippine tuan purse seinery, despite a variety of policies that have been implemented to reduce it. These policies have predominantly focused on models of cause and effect which ignore the possibility that the intended outcomes are altered by social behavior of autonomous agents at lower scales. 

This model is a spatially explicit Agent-based Model (ABM) for the Philippine tuna purse seine fishery, specifically designed to include social behavior and to study its effects on fishing effort, fish stock and industry profit. The model includes economic and social factors of decision making by companies and fishing vessels that have been informed by interviews.

The model has 196 (14 x 14) fishing sites, which has fish stock.

The model consists of two levels of agents: fishing companies and fishing vessels. Companies have no physical location, and are therefore not spatially explicit (but they are shown at the upper right corner of the simulation world). The second agent level is the operational level (represented by the vessel figures).

## HOW IT WORKS

The simulation starts with initialization of parameter values.
 
An overview of the actions for each time-step is as follows:

1. Vessel actions:
	a. Decision to harvest or not harvest for the week (every 1st day of simulation week)
	b. Harvest action for those vessels who have decided to harvest

2. Skipjack growth and diffusion

3. Year-end actions (only every 365th time step)
	a. update year-end variables
	b. companies compute probabilities to buy and sell vessel
	c. vessel (dis)investment decision of companies
	d. Entry of new vessels

One time-step is equivalent to one simulation day, 7 time-steps to one simulation week, and 365 time-steps to  one simulation year.

The code contains description of each parameter and variable, and of each process.
A complete model description can also be requested from George van Voorn (george.vanvoorn@wur.nl)

## HOW TO USE IT

The first step is to press the "set-up" button. This initializes the parameter values.

To run the simulation using the nominal values, press "nominal values". Otherwise, adjust the value of the parameter as you will. The parameters are categorized into 1/ company parameters, 2/ parameters that affect the probability of buying a vessel (P(buy)), 3/ parameters that affect the probability of selling a vessel (P(sell)), 4/ parameters that affect the probability of entry of a potential entrant (P(entry)), 5/ cost parameters, 6/price parameters, 7/vessel parameters, and 8/ecological parameters.

To run the simulation, you can press: 1/ go = run for one simulation step (= 1 day), 2/go forever = simulation runs until you again press the "go forever" button; 3/ go 7 days = run for 1 simulation week; or 4/ go 365 days = run for 1 simulation year.

Test how changing the values of parameters affect the different outputs: 1/total number of vessels, 2/number of companies, 3/fishing days, 4/total catch, 5/total fishing days, and 6/industry profit.

## THINGS TO NOTICE

What happens to the outcomes when you change the values of the following?

1. Resistance to exit (resistance-to0exit)
2. Congestion threshold
3. Potential entrants
4. Fuel cost per liter (fuel-cost-liter)
5. Target harvest to capacity ratio (target-catch-capacity)


## CREDITS AND REFERENCES
This ABM is a part of the project of Stella Libre under the BESTTuna project of Wageningen University, the Netherlands. A paper titled "Effects of social factors on fishing effort: the case of the Philippine tuna purse seine fishery" by S.V. Libre, G. van Voorn, G.A. ten Broeke, P. Berentsen, M. Bailey and S. Bush is published using this model.

The model was coded by Stella Libre, with the assistance of Guus ten Broeke. It was tested by Stella Libre, and tested independently by Guus ten Broeke and George van Voorn.

## HOW TO CITE
If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:

* Libre, SVD., ten Broeke, G.A., and G. van Voorn (2014). ???? Wageningen University, Netherlands.

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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

boat
false
0
Polygon -7500403 true true 0 150 300 150 225 225 60 225 0 150 105 150 105 15 225 150 105 150
Line -16777216 false 105 150 240 150

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
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="nominal values 50 years" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="35000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OAT starting capital" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="35000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="starting-capital" first="0" step="10000" last="260000"/>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OAT initial fish stock" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="35000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OAT harvest ratio" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="33205"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <steppedValueSet variable="harvest-ratio-mean" first="0.005" step="0.001" last="0.03"/>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OAT otherfish price" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="33205"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="otherfish-skipjack-price-ratio" first="0.1" step="0.1" last="3"/>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="OAT fuel-share" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="33205"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="fuel-share" first="0.05" step="0.05" last="0.95"/>
  </experiment>
  <experiment name="OAT labor share" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="33205"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="labor-share" first="0.05" step="0.05" last="0.95"/>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="congestion ratio 1 50 years" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18250"/>
    <metric>total-catch</metric>
    <metric>total-profit</metric>
    <metric>count vessels</metric>
    <metric>count companies</metric>
    <metric>sum [fish-density] of patches</metric>
    <metric>sum [count-days-fishing] of vessels</metric>
    <metric>mean [risk-behaviour] of companies</metric>
    <metric>mean [capital] of companies</metric>
    <metric>mean [probability-buy] of companies</metric>
    <metric>mean [probability-sell] of companies</metric>
    <metric>buying-ratio</metric>
    <metric>selling-ratio</metric>
    <metric>mean [(count turtles-on neighbors / count neighbors)] of patches</metric>
    <enumeratedValueSet variable="yellowfin-skipjack-price-ratio">
      <value value="1.21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="delta-skipjack-price-catch">
      <value value="-0.0041"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="skipjack-ratio-to-catch">
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="labor-share">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yellowfin-ratio-to-catch">
      <value value="0.13"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-timesteps">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-cost-liter">
      <value value="1.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-intervals">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-sell">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-standard-deviation">
      <value value="0.004"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resistance-to-exit">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-right">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-diffusivity">
      <value value="310.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-variables">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-skipjack-price">
      <value value="1661"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-others-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-left">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harvest-ratio-mean">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-growth-rate">
      <value value="0.0024"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-ratio-to-catch">
      <value value="0.31"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-recent">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-entry">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="go-function?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-capacity">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-sell">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-fish-stock">
      <value value="35000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-entrants">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="congestion-threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vessel-cost">
      <value value="1700000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-companies">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-catch-buy">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fuel-share">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="maximum-amount-fish-system">
      <value value="48369"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="otherfish-skipjack-price-ratio">
      <value value="0.86"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-entry">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="second-hand-ratio">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-samples">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-catch-capacity">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="risk-behaviour-standard-deviation">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-parameters">
      <value value="38"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-profit-buy">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="starting-capital">
      <value value="27000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-site-buy">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="n-replicates">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fixedcost">
      <value value="23000"/>
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
