set more off
set matsize 2500
cd "~\radius\Tanzania\Data"
import delim "tanzania_dyadic_with_communities.csv", clear
cd "~\radius\Tanzania\Tables"

* generate variables
gen supported_samecomm = supported*samecomm
gen frfam_samecomm = frfam*samecomm
gen frfam2_samecomm = frfam2*samecomm
gen frfam3_samecomm = frfam3*samecomm

gen supported_samegroups = supported*samegroups
gen frfam_samegroups= frfam*samegroups
gen frfam2_samegroups = frfam2*samegroups
gen frfam3_samegroups = frfam3*samegroups
gen samecomm_samegroups = samecomm*samegroups
gen unsupported = frfam*(1-supported)

gen shell2 = frfam + frfam2
gen shell2_samecomm = shell2*samecomm
gen shell3 = frfam + frfam2 + frfam3
gen shell3_samecomm = shell3*samecomm


gen category = "d1 supported" if supported == 1
replace category = "d1 unsupported" if frfam == 1 & supported == 0
replace category = "d2" if frfam2 == 1
replace category = "d3" if frfam3 == 1
replace category = "d4+" if category == ""

cor supported unsupported frfam frfam2 frfam3 samecomm samegroups
tab category samecomm
tab samegroup samecomm
/*------------------------------------------------------------------------------
Table 3
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg any_transfer supported, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam, id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam samecomm, id(ha hb) symmetric
esttab using tanzania_main_any_nocontrols.tex, ///
    title(Main Effects on Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table 7: The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network sans Controls
Note: we must omit frfam3 x samecomm from this regression, since left out group 
would then be only three dyads! (otherwise we get a conformability error) 
Could also omit samecomm (which is better?)
------------------------------------------------------------------------------*/

eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg any_transfer frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm ///
	supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric

*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_any_nocontrols.tex, ///
    title(The Effects of Longer Walks and Interactions on Transfers: Tanzania Nyakatoke Network sans Controls) replace

/*------------------------------------------------------------------------------
Table 8: The Role of Groups in Effects on Transfers: Tanzania Nyakatoke Network 
sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg any_transfer samegroups, id (ha hb) symmetric
quietly eststo: ngreg any_transfer supported samegroups, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam samegroups, id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm samegroups, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samegroups, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm samegroups, id(ha hb) symmetric
esttab using tanzania_groups_any_nocontrols.tex, ///
    title(The Role of Groups in Effects on Transfers: Tanzania Nyakatoke Network sans Controls) replace


/*------------------------------------------------------------------------------
Table 9: Main Effects on Total Transfers: Tanzania Nyakatoke Network sans 
Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg total_transfers supported, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam, id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam samecomm, id(ha hb) symmetric
esttab using tanzania_main_total_nocontrols.tex, ///
    title(Main Effects on Total Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table 10: The Effects of Longer Walks and Interactions on Total Transfers: 
Tanzania Nyakatoke Network sans Controls
------------------------------------------------------------------------------*/

eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg total_transfers frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_total_nocontrols.tex, ///
    title(The Effects of Longer Walks and Interactions on Total Transfers: Tanzania Nyakatoke Network sans Controls) replace

/*------------------------------------------------------------------------------
Table 11: The Role of Groups in Effects on Total Transfers: Tanzania Nyakatoke 
Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg total_transfers samegroups, id (ha hb) symmetric
quietly eststo: ngreg total_transfers supported samegroups, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam samegroups, id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm samegroups, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samegroups, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm samegroups, id(ha hb) symmetric
esttab using tanzania_groups_total_nocontrols.tex, ///
    title(The Role of Groups in Effects on Total Transfers: Tanzania Nyakatoke Network sans Controls) replace


/*------------------------------------------------------------------------------
Table A: Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network sans
Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg reciprocal_transfer supported, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam samecomm, id(ha hb) symmetric
esttab using tanzania_main_recip_nocontrols.tex, ///
    title(Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network sans Controls) replace

/*------------------------------------------------------------------------------
Table B: The Effects of Longer Walks and Interactions on Reciprocal Transfers: 
Tanzania Nyakatoke Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_recip_nocontrols.tex, ///
    title(The Effects of Longer Walks and Interactions on Reciprocal Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table C: The Role of Groups in Effects on Reciprocal Transfers: Tanzania 
Nyakatoke Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg reciprocal_transfer samegroups, id (ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported samegroups, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam samegroups, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm samegroups, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samegroups, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm samegroups, id(ha hb) symmetric
esttab using tanzania_groups_recip_nocontrols.tex, ///
    title(The Role of Groups in Effects on Reciprocal Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
local controls "age_sum age_diff both_male one_male both_highedu one_highedu both_muslim one_muslim same_tribe same_clan wealth_sum wealth_diff"

/*------------------------------------------------------------------------------
Table F: Main Effects on Transfers: Tanzania Nyakatoke Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg any_transfer supported `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam samecomm `controls', id(ha hb) symmetric
esttab using tanzania_main_any_controls.tex, ///
    title(Main Effects on Transfers: Tanzania Nyakatoke Network with Controls) replace
	
/*------------------------------------------------------------------------------
Table G: The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network with Controls
------------------------------------------------------------------------------*/

eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm `controls', id(ha hb) symmetric
*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_any_controls.tex, ///
    title(The Effects of Longer Walks and Interactions on Transfers: Tanzania Nyakatoke Network with Controls) replace

/*------------------------------------------------------------------------------
Table H: The Role of Groups in Effects on Transfers: Tanzania Nyakatoke Network 
with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg any_transfer samegroups `controls', id (ha hb) symmetric
quietly eststo: ngreg any_transfer supported samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm samegroups `controls', id(ha hb) symmetric
esttab using tanzania_groups_any_controls.tex, ///
    title(The Role of Groups in Effects on Transfers: Tanzania Nyakatoke Network with Controls) replace


/*------------------------------------------------------------------------------
Table I: Main Effects on Total Transfers: Tanzania Nyakatoke Network with 
Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg total_transfers supported `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam samecomm `controls', id(ha hb) symmetric
esttab using tanzania_main_total_controls.tex, ///
    title(Main Effects on Total Transfers: Tanzania Nyakatoke Network with Controls) replace
	
/*------------------------------------------------------------------------------
Table J: The Effects of Longer Walks and Interactions on Total Transfers: 
Tanzania Nyakatoke Network with Controls
------------------------------------------------------------------------------*/

eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm `controls', id(ha hb) symmetric
*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_total_controls.tex, ///
    title(The Effects of Longer Walks and Interactions on Total Transfers: Tanzania Nyakatoke Network with Controls) replace

/*------------------------------------------------------------------------------
Table K: The Role of Groups in Effects on Total Transfers: Tanzania Nyakatoke 
Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg total_transfers samegroups `controls', id (ha hb) symmetric
quietly eststo: ngreg total_transfers supported samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm samegroups `controls', id(ha hb) symmetric
esttab using tanzania_groups_total_controls.tex, ///
    title(The Role of Groups in Effects on Total Transfers: Tanzania Nyakatoke Network with Controls) replace


/*------------------------------------------------------------------------------
Table L: Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network with 
Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg reciprocal_transfer supported `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam samecomm `controls', id(ha hb) symmetric
esttab using tanzania_main_recip_controls.tex, ///
    title(Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network with Controls) replace

/*------------------------------------------------------------------------------
Table M: The Effects of Longer Walks and Interactions on Reciprocal Transfers: 
Tanzania Nyakatoke Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm `controls', id(ha hb) symmetric
*test supported + supported_samecomm = 0
*test frfam + frfam_samecomm = 0
*test frfam2 + frfam2_samecomm = 0
*test frfam3 + frfam3_samecomm = 0
esttab using tanzania_interaction_recip_controls.tex, ///
    title(The Effects of Longer Walks and Interactions on Reciprocal Transfers: Tanzania Nyakatoke Network with Controls) replace
	
/*------------------------------------------------------------------------------
Table N: The Role of Groups in Effects on Reciprocal Transfers: Tanzania 
Nyakatoke Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: ngreg reciprocal_transfer samegroups `controls', id (ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samegroups `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm samegroups `controls', id(ha hb) symmetric
esttab using tanzania_groups_recip_controls.tex, ///
    title(The Role of Groups in Effects on Reciprocal Transfers: Tanzania Nyakatoke Network with Controls) replace
	

/*------------------------------------------------------------------------------
Visualizations: use putexcel to build easy to read output for R
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
Export Conditional Expectations from Any Trasfer 
------------------------------------------------------------------------------*/
quietly ngreg any_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
cd "~\radius\Tanzania\Figures"
putexcel set cond_exp_any_transfer, replace sheet("results")
putexcel A1 = "dyadic_relationship"
putexcel B1 = "coef_sum"
putexcel C1 = "se"
* supported, between communities
lincom frfam + supported
sca x = r(estimate)
sca y = r(se)
putexcel A2 = "Supported, Between Communities"
putexcel B2 = x
putexcel C2 = y
sca drop x y
* distance-1, between communities
lincom frfam
sca x = r(estimate)
sca y = r(se)
putexcel A3 = "Distance-1, Between Communities"
putexcel B3 = x
putexcel C3 = y
sca drop x y
* distance-2, between communities
lincom frfam2
sca x = r(estimate)
sca y = r(se)
putexcel A4 = "Distance-2, Between Communities"
putexcel B4 = x
putexcel C4 = y
sca drop x y
* distance-1, between communities
lincom frfam3
sca x = r(estimate)
sca y = r(se)
putexcel A5 = "Distance-3, Between Communities"
putexcel B5 = x
putexcel C5 = y
sca drop x y
* supported, within community 
lincom frfam + supported + samecomm + frfam_samecomm + supported_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A6 = "Supported, Within Community"
putexcel B6 = x
putexcel C6 = y
sca drop x y
* distance-1, within community
lincom frfam + samecomm + frfam_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A7 = "Distance-1, Within Community"
putexcel B7 = x
putexcel C7 = y
sca drop x y
* distance-2, within community
lincom frfam2 + samecomm + frfam2_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A8 = "Distance-2, Within Community"
putexcel B8 = x
putexcel C8 = y
sca drop x y
*distance3, within community: frfam3_samcomm dropped because it is a linear combo of other vars
lincom frfam3 + samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A9 = "Distance-3, Within Community"
putexcel B9 = x
putexcel C9 = y
sca drop x y

/*------------------------------------------------------------------------------
Export Conditional Expectations from Reciprocal Transfers
------------------------------------------------------------------------------*/

quietly ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
putexcel set cond_exp_reciprocal_transfer, replace sheet("results")
putexcel A1 = "dyadic_relationship"
putexcel B1 = "coef_sum"
putexcel C1 = "se"
* supported, between communities
lincom frfam + supported
sca x = r(estimate)
sca y = r(se)
putexcel A2 = "Supported, Between Communities"
putexcel B2 = x
putexcel C2 = y
sca drop x y
* distance-1, between communities
lincom frfam
sca x = r(estimate)
sca y = r(se)
putexcel A3 = "Distance-1, Between Communities"
putexcel B3 = x
putexcel C3 = y
sca drop x y
* distance-2, between communities
lincom frfam2
sca x = r(estimate)
sca y = r(se)
putexcel A4 = "Distance-2, Between Communities"
putexcel B4 = x
putexcel C4 = y
sca drop x y
* distance-1, between communities
lincom frfam3
sca x = r(estimate)
sca y = r(se)
putexcel A5 = "Distance-3, Between Communities"
putexcel B5 = x
putexcel C5 = y
sca drop x y
* supported, within community 
lincom frfam + supported + samecomm + frfam_samecomm + supported_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A6 = "Supported, Within Community"
putexcel B6 = x
putexcel C6 = y
sca drop x y
* distance-1, within community
lincom frfam + samecomm + frfam_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A7 = "Distance-1, Within Community"
putexcel B7 = x
putexcel C7 = y
sca drop x y
* distance-2, within community
lincom frfam2 + samecomm + frfam2_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A8 = "Distance-2, Within Community"
putexcel B8 = x
putexcel C8 = y
sca drop x y
*distance3, within community: frfam3_samcomm dropped because it is a linear combo of other vars
lincom frfam3 + samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A9 = "Distance-3, Within Community"
putexcel B9 = x
putexcel C9 = y
sca drop x y

/*------------------------------------------------------------------------------
Export Conditional Expectations from Total Transfers 
------------------------------------------------------------------------------*/

quietly ngreg total_transfers supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
putexcel set cond_exp_total_transfers, replace sheet("results")
putexcel A1 = "dyadic_relationship"
putexcel B1 = "coef_sum"
putexcel C1 = "se"
* supported, between communities
lincom frfam + supported
sca x = r(estimate)
sca y = r(se)
putexcel A2 = "Supported, Between Communities"
putexcel B2 = x
putexcel C2 = y
sca drop x y
* distance-1, between communities
lincom frfam
sca x = r(estimate)
sca y = r(se)
putexcel A3 = "Distance-1, Between Communities"
putexcel B3 = x
putexcel C3 = y
sca drop x y
* distance-2, between communities
lincom frfam2
sca x = r(estimate)
sca y = r(se)
putexcel A4 = "Distance-2, Between Communities"
putexcel B4 = x
putexcel C4 = y
sca drop x y
* distance-1, between communities
lincom frfam3
sca x = r(estimate)
sca y = r(se)
putexcel A5 = "Distance-3, Between Communities"
putexcel B5 = x
putexcel C5 = y
sca drop x y
* supported, within community 
lincom frfam + supported + samecomm + frfam_samecomm + supported_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A6 = "Supported, Within Community"
putexcel B6 = x
putexcel C6 = y
sca drop x y
* distance-1, within community
lincom frfam + samecomm + frfam_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A7 = "Distance-1, Within Community"
putexcel B7 = x
putexcel C7 = y
sca drop x y
* distance-2, within community
lincom frfam2 + samecomm + frfam2_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A8 = "Distance-2, Within Community"
putexcel B8 = x
putexcel C8 = y
sca drop x y
*distance3, within community: frfam3_samcomm dropped because it is a linear combo of other vars
lincom frfam3 + samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A9 = "Distance-3, Within Community"
putexcel B9 = x
putexcel C9 = y
sca drop x y

cd "~\radius\Tanzania\Tables"
* new tables adjusted
/*------------------------------------------------------------------------------
Table XX: Main Effects on Transfers: Tanzania Nyakatoke Network sans Controls + 
The Effects of Longer Walks and Interactions on Transfers: Tanzania Nyakatoke 
Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg any_transfer supported, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam, id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
esttab using tanzania_combined_any_nocontrols.tex, ///
    title(Effects of Dyadic Relationships on Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table XX: Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network sans 
Controls + The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg reciprocal_transfer supported, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
esttab using tanzania_combined_recip_nocontrols.tex, ///
    title(Effects of Dyadic Relationships on Reciprocal Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table XX: Main Effects on Total Transfers: Tanzania Nyakatoke Network sans 
Controls + The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg total_transfers supported, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam, id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3, id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm, id(ha hb) symmetric
esttab using tanzania_combined_total_nocontrols.tex, ///
    title(Effects of Dyadic Relationships on Total Transfers: Tanzania Nyakatoke Network sans Controls) replace
	
	
/*------------------------------------------------------------------------------
Table XX: Main Effects on Transfers: Tanzania Nyakatoke Network sans Controls + 
The Effects of Longer Walks and Interactions on Transfers: Tanzania Nyakatoke 
Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg any_transfer supported `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
esttab using tanzania_combined_any_controls.tex, ///
    title(Effects of Dyadic Relationships on Transfers: Tanzania Nyakatoke Network with Controls) replace
	
/*------------------------------------------------------------------------------
Table XX: Main Effects on Reciprocal Transfers: Tanzania Nyakatoke Network sans 
Controls + The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg reciprocal_transfer supported `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
esttab using tanzania_combined_recip_controls.tex, ///
    title(Effects of Dyadic Relationships on Reciprocal Transfers: Tanzania Nyakatoke Network with Controls) replace
	
/*------------------------------------------------------------------------------
Table XX: Main Effects on Total Transfers: Tanzania Nyakatoke Network sans 
Controls + The Effects of Longer Walks and Interactions on Transfers: Tanzania 
Nyakatoke Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions
* longer walks and communities: frfam2 and frfam3 
quietly eststo: ngreg total_transfers supported `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm `controls', id(ha hb) symmetric
esttab using tanzania_combined_total_controls.tex, ///
    title(Effects of Dyadic Relationships on Total Transfers: Tanzania Nyakatoke Network with Controls) replace

eststo clear
quietly eststo: ngreg any_transfer supported frfam samecomm, id(ha hb) logit symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3, id(ha hb) logit symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm, id(ha hb) logit symmetric
quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) logit symmetric
esttab using tanzania_any_logit.tex, ///
    title(Effects of Dyadic Relationships on Transfers: Logistic Specification) replace
	
margins, dydx(supported frfam samecomm) atmeans

logit any_transfer i.supported i.frfam i.samecomm

eststo clear
local am "age_sum age_diff both_highedu both_lowedu sumcons_ppp difcons_ppp"
quietly eststo: ngreg frfam `am', id(ha hb) symmetric
quietly eststo: ngreg samecomm `am', id(ha hb) symmetric
esttab using tanzania_am.tex, ///
    title(Assortative Matching in Tanzania Risk Sharing Networks and Communities) replace

	
reg any_transfer supported frfam frfam2 frfam3 samecomm
estat vif