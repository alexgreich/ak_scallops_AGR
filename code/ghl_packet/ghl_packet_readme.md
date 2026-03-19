# Instructions for Observer Data Summary 

Traditionally scallop GHLs have been set in by reviewing a summary of observer data from the fishing season prior and making a qualitative decision on tailoring GHL up or down based on fishery performance, size structure, discards, etc. This document is an explanation of what information is used to develop the 'GHL Packet'.

# Data

The observer manual is in the link below for a full detailed explanation of observer sampling.
[Scallop Observer Manual](http://kodweb.fishgame.state.ak.us/index/Wiki:Observer_Program:Scallop_Observer_Program:Scallop_Observer_Manual:)

## Catch by Haul

[Catch by Haul Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/catchbyhaul/index)

You can download a year's worth of data by selecting year and leaving fishery blank, change output to .csv file. This process will be true for all data types.  

These data are scallop catch by haul, for all hauls, including observed and non-observed hauls. Essentially these data are the log book. The scallop fishery is managed in terms of shucked meat weight, but observers measure round (i.e., whole) weight and number of scallops. The observer sampling design is to weight and count X number of 'baskets' and to enumerate all baskets. So, round weight and number of scallops per haul are extrapolated from the number of baskets and the observer samples. There *is* a column for meat weight for each haul, this is approximated using the total meat weight by day from the vessel's required Daily Production Log (i.e., these are technically catcher/processors) and apportioning it to hauls given the number of retained baskets. These data also include the various haul level characteristics (e.g., location, duration, speed, etc.). The column 'Bed_Code' may be blank if the haul falls outside of the established bed boundary.  

Store these data in ./data/observer/catch

## Discard by Haul

[Bycatch by Haul Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/bycatchbyhaul/index)

These data pair 1:1 with the catch by haul data. Boats typically are fishing two dredges, but the observer only samples from one, so 'sample hours' is the unit of effort that the bycatch sample corresponds to. Dredge hours here should be equal to what you see in the catch by haul data, which is for both dredges. Same with round weight and meat weight. These data include counts of crab and halibut bycatch and a breakdown of the scallop discards. Observers gather up all discards, take a subsample and sort the subsample into catagories. The columns 'Discard_Weight' and 'Discard_Count' refer to the intact discards and 'Broken_weight' refers to the cracked and crushed discards of the subsample. This is no count on broken scallops (sometime they are just pieces). The column 'Rem_Disc_Wt' is the weight of unsubsampled portion. The estimation notation is described below. The column on clapper count refers to the full sample.  

Store these data in ./data/observer/bycatch

## Shell Height by Haul

[Shell Height by Haul Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/shellheight/index)

These data also have a 'Haul_ID' column so they can be paired to each haul. Should be fairly self explanatory, shell height is in mm. The column 'Rtnd_Disc' is D for discard, R for retained, M for shell height / shell height collection (explained in more detail below). Observers measure 40 retained and 40 discarded scallops per haul.

Store these data in ./data/observer/shell_height  

## Shell Height / Meat Weight

[Shell Height / Meat Weight by Haul Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/reports/shellmeat/index)

These data started as a special project but are now a standard collection. From the size composition sample, observer shuck and save meats from a subsample that are later weighted in the lab. Meat weights fluctate asynchronously with the reproductive cycle, so deoending on the timing of the fishing within the season, the number of scallops at a given size that are needed to achieve a GHL varies. Meat weight here is in grams. This is also the data that are used for the summary of gonad condition and weak meats ('Meat_Condition').  

Store these data in ./data/observer/meat_weight

[Gonad Codes](http://kodweb.fishgame.state.ak.us/view/Wiki:Observer_Program:Scallop_Observer_Program:Scallop_Observer_Manual:Appendix_5)

[Meat Condition Codes](http://kodweb.fishgame.state.ak.us/view/Wiki:Observer_Program:Scallop_Observer_Program:Scallop_Observer_Manual:Appendix_6)

## Other Observer Data

Haul composition and daily production log data are also available, but not needed for the GHL packet. Observers no longer collect crab size composition.

[Haul Composition Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/haulcomp/index)

[Daily Production Log Query](http://kodweb.fishgame.state.ak.us/apps/scalobs/prodlogbyday/index)

## GHL and Crab Bycatch Limit Data

Annual GHL (lb of meat) and crab bycatch limits by district. Not all districts use crab bycatch limits. 

Store these data in ./data/observer/metadata



# Analysis

The analysis to produce figures and tables for the GHL packet are found here: [Observer Data Report](https://github.com/commfish/ak_scallops/tree/main/code/observer/observer_report). Files are named by year - 'YYYY_observer_report_analysis.R'.

## scalobservR.R

I (Tyler) started writing functions for an R package to download and analysis observer data analogous to the one I had written for BSAI crab observer data, but since the scallop data have an audience of one, it probably suffices to leave it as an R script. The purpose was to put a lot of the data wrangling, management, and math behind the scenes so you can write more succinct code. ```{r 2025_observer_report_analysis.R}``` includes all of what the ```{r scalobservR.R}``` do, but I produced the GHL packet in enough haste that I didn't update the script to actually use the new functions. I'll explain them as I go. FYI, the database pull functionality is not implemented, may never be implemented.  

## Loading and Cleaning Data

If anything, this process should use the ```{r scalobservR.R}``` functions. See annotation for each function to understand what each does. Generally, they load and combine data files by year, clean up the names (I avoid uppercases almost always) and make some corrections:

- Districts D and D16 are combined and renamed YAK for Yakutat
- Several scallop beds that are geographically within the Kodiak Shelikof District are reassigned to the Kodiak Southwest District (they are managed as part of Southwest)
- An observer mistake for the 2018/19 Ducth Harbor fishery is corrected (there'd be no data otherwise for that year)
- Renames the YAKB bed EK1. The East Kayak Island bed spans the East Kayak and Yakutat management boundary and the Yakutat portion may have been recorded as either EK1 or YAKB, so this makes it consistent. 
- Join to haul data wherever appropriate

## Retained Catch

Retained catch is simply the sum of round and meat weight or number of scallops across hauls by district. The observers collect data in kg and it is reported in lb to managers. One thing to note is that retained catch for the scallop fishery in terms of round weight and numbers is still technically an estimate for two reasons 1) total weight is extrapolated from the average basket weight and the number of baskets, and 2) in non observed hauls the total number of baskets is a guestimate by the captain (which we have some old data on that suggests its not too bad). I've never tried to quantify uncertainty.

```{r scalobservR.R}``` function is ```{r get_retained_summary()``` which produces retained catch and nominal CPUE (weight or number per dredge hour).

## Discarded Catch

Discarded catch ($\hat{D}$) is computed as 

\[
\hat{D} = A(\frac{1}{n}\sum^{n}_{i=1}{\frac{d_i}{a_i}})
\]

where $d_i$ is the discarded round weight and $a_i$ is the dredge hours corresponding to the sampled dredge in tow $i$, $n$ is the number of observed tows in the fishery, and $A$ is the total fishery dredge hours. Standard errors for discards were estimated directly as

\[
\sigma_{D} = \sqrt{A^{2}(\frac{1}{n}\text{Var}(\frac{d_i}{a_i}))}
\]




















