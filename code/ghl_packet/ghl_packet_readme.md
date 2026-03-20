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

Annual GHL (lb of meat) and crab bycatch limits by district. Not all districts use crab bycatch limits. Opening this link downloads the data. It is all years, all districts.

[GHL Download](http://kodweb.fishgame.state.ak.us/apps/dda/scallop/ghls/ghlData?)

Store these data in ./data/observer/metadata



# Analysis

The analysis to produce figures and tables for the GHL packet are found here: [Observer Data Report](https://github.com/commfish/ak_scallops/tree/main/code/observer/observer_report). Files are named by year - 'YYYY_observer_report_analysis.R'.

## scalobservR.R

[```scalobservR.R```](https://github.com/commfish/ak_scallops/blob/main/code/observer/scalobservR.R)

I (Tyler) started writing functions for an R package to download and analysis observer data analogous to the one I had written for BSAI crab observer data, but since the scallop data have an audience of one, it probably suffices to leave it as an R script. The purpose was to put a lot of the data wrangling, management, and math behind the scenes so you can write more succinct code. ```2025_observer_report_analysis.R``` includes all of what the ```scalobservR.R``` do, but I produced the GHL packet in enough haste that I didn't update the script to actually use the new functions. I'll explain them as I go. FYI, the database pull functionality is not implemented, may never be implemented.  

## Loading and Cleaning Data

If anything, this process should use the ```scalobservR.R``` functions. See annotation for each function to understand what each does. Generally, they load and combine data files by year, clean up the names (I avoid uppercases almost always) and make some corrections:

- Districts D and D16 are combined and renamed YAK for Yakutat
- Several scallop beds that are geographically within the Kodiak Shelikof District are reassigned to the Kodiak Southwest District (they are managed as part of Southwest)
- An observer mistake for the 2018/19 Ducth Harbor fishery is corrected (there'd be no data otherwise for that year)
- Renames the YAKB bed EK1. The East Kayak Island bed spans the East Kayak and Yakutat management boundary and the Yakutat portion may have been recorded as either EK1 or YAKB, so this makes it consistent. 
- Join to haul data wherever appropriate

Though not combined by the load data functions, Unimak Bight (UB), West Chignik (WC), and Central (C) Districts of the Alaska Peninsula (Area M) often get combined.

## Retained Catch

Retained catch is simply the sum of round and meat weight or number of scallops across hauls by district. The observers collect data in kg and it is reported in lb to managers. One thing to note is that retained catch for the scallop fishery in terms of round weight and numbers is still technically an estimate for two reasons 1) total weight is extrapolated from the average basket weight and the number of baskets, and 2) in non observed hauls the total number of baskets is a guestimate by the captain (which we have some old data on that suggests its not too bad). I've never tried to quantify uncertainty.

The ```scalobservR.R``` function is ```get_retained_summary()``` which produces retained catch and nominal CPUE (weight or number per dredge hour).

## Discarded Catch

Discarded catch ($\hat{D}$) is computed as 

$$
\hat{D} = A(\frac{1}{n}\sum^{n}_{i=1}{\frac{d_i}{a_i}})
$$

where $d_i$ is the discarded round weight and $a_i$ is the dredge hours corresponding to the sampled dredge in tow $i$, $n$ is the number of observed tows in the fishery, and $A$ is the total fishery dredge hours. Standard errors for discards were estimated directly as

$$
\sigma_{D} = \sqrt{A^{2}(\frac{1}{n}\text{Var}(\frac{d_i}{a_i}))}
$$

It's a bit more involved in the R code given the different classifcations of discards, but it should be easy enough to follow. The ```scalobservR.R``` function is ```get_discards()``` will produce only estimates of discard weight and number. What is reported in the GHL packet is a bit more detailed, it includes discard ratio (lb discarded:retained), discard rate (discards per dredge hour), and discard mortality (handling mortality is 0.2). Presumably it would be full mortality for crush discards, but the data aren't specific enough to separate broken and crushed.

## Shell Height Composition

Because shell height data are collected at a fixed sample size,shell height is extrapolated as the weighted composition of retained ($L_R$) or discarded ($L_D$) scallops 

$$
L_{R,j} = \omega_{R,i} \sum_{i = 1}{l_{R,j,i}}
$$

$$
L_{D,j} = \omega_{D,i} \sum_{i = 1}{l_{D,j,i}}
$$

where $l_{j}$ is the retained or discarded number at shell height $j$, and $\omega_{i}$ is the proportion of the total annual catch (retained + discards) in haul $i$. The plot used in the GHL packet is probably overly complicated-to-make ridgline plot that Tyler thought looked good at one point in time.  

## CPUE Standardization

The CPUE standardization is documented in modelling efforts presented to the NPFMC. The method has not changed in a long time and is just updated with new data. From NPFMC documents:

<blockquote>

CPUE standardization was derived from at-sea observer data from the 1996/7 - 2024/25 seasons. CPUE was defined as the total round weight of the catch per dredge-hour. Prior to analysis, fishery log-book data were filtered so that core data only included hauls that employed 13 or 15 ft dredges and adequate dredge performance. Zero catches were removed since they are typically rare and indicate poor gear performance. Hauls were also limited to the inner 95% of CPUE and depth.

CPUE standardization models were fit using general additive models (GAM) as implemented in the R package $mgcv$ (Wood 2004). All models assumed a Gamma error distribution with log-link. Null models by district included only year (of season opening) as an explanatory variable

$$
\text{ln}(CPUE_{i}) = Year_{y,i}
$$

The full scope of models evaluated included vessel, depth, dredge width, month and bed. Bed was not included for WKI District, since it only contains a single bed. Depth was fit as a thin plate regression spline, with smoothness determined by generalized cross-validation (Wood 2004). All other variables were fit as factors. The effects of variable addition were evaluated by forward and backward stepwise selection. The addition of a new variable was considered significant if CAIC (Anderson et al., 1998) decreased by at least two per degree of freedom lost and deviance explained ($R^2$) increased by at least 0.01. The best model forms by district are listed in Table XXX. The marginal effects of selected covariates are in Figures XXX.

The standardized CPUE index was extracted from the models as the year coefficient ($\beta_i$) with the first level set to zero and scaled to canonical coefficients ($\beta^\prime_i$) as

$$
\beta^{\prime}_{i} = \frac{\beta_{i}}{\bar{\beta}}
$$

where

$$
\bar{\beta} = \sqrt[n_{i}]{\prod_{i = 1}^{n_j}{\beta_{i}}}
$$

and $n_j$ is the number of levels in the year variable. Nominal CPUE was scaled by the same method for comparison.

</blockquote>

In practice this uses a custom function Tyler wrote for forward and backward selection of $mgcv$ style GAMs called ```f_step_gam()``` within a for loop to standardize CPUE for each ditrict that was fished in the previous season. There is no need to updated this index for areas that were not fished. The code produces plots of DHARMa residuals, but those don't need to be in the GHL packet.  

The code then extracts and plots marginal effects. This needs to be stepped through somewhat carefully, because the plot code may change depending on what covariated are selected for the final, 'best' model (i can changed from year to year on occasion).  

Then the code makes step plots using the custom ```f_step_plot()``` function whcih plots the standardized index for the null, full and intermediate model after each covariate is added. This is a good way to evaluate the influence of each covariate. Lastly, the code makes plots of the standardized CPUE in comparison to the nominal CPUE. The custom function ```f_getCPUE_gam()``` is used extract the standardized index from the model, given an $mgcv$ object.  

The custom functions mentioned here are in [```scallop_obs_functions.R```](https://github.com/commfish/ak_scallops/blob/main/code/observer/scallop_obs_functions.R) which is the legacy version of ```scalobservR.R```. Some of these functions **should** eventually get moved over, but some can be forgotten in the annals of time.  

## Fishery Extent

Heat maps of effort are produced by season. The R code bins lat/lon data to the nearest 0.05 degree and summarises the proportion of dredge hours in each location bin. Then, a ggplot is made with geom tile to plot the cells.

It's usually useful to interpret trends in fishery performance in the context of where fishing occured due to risk of hyperstability. To help with that Tyler developed an index of fishery extent that is the average pairwise distance between all hauls, excluding those that contribute the bottom 10% of catch (to exclude prospecting hauls). The intuition is that when fishing is spread out, the average pairwise distance is larger than when it's contracted. Also, using pairwise data allows the index to be weighted over all hauls instead of a few wayward hauls having undue influence like in a hull. 

## Clappers

Clapper catches can be used as an index of natural mortality, and in the past few seasons there was a very large mortality event that occured in Kodiak Shelikof and Northeast. So the R code summarise clapper counts and makes several plots, including plots of clapper catch by vessel.

## Retention Curves

There is no legal size for scallops, though $\geq$ 100 mm shell height is consider 'exploitable'. The R code uses a simple binomial regression and the size composition data which are designated as 'retained' and 'discarded' to estimate annual retention curves. Variability in annual size at 50% and 10% retention provide some context of fishing behavior and what portion of the stock being exploited.

## Gonads

Gonad plots are simply stacked bar plots of the proportion by gonad condition.

## Meat Weight ~ Shell Height (Round Weight)

These are just scatter plots of meat weight as a function of shell height or round weight. Meat weight ~ shell height is allometric, whereas meat weight ~ round weight is linear.
































