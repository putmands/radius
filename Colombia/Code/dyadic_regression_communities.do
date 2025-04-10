set more off
set matsize 2500
cd "~\radius\Colombia\Data\"
import delim "colombia_dyads_with_communities.csv", clear
cd "~\radius\Colombia\Results\"

gen frfam_unsupported = frfam*(1-frfam_supported)

* fix these -- though they should be fixed from the R code!
replace frfam2 = 0 if frfam == 1
replace frfam3 = 0 if frfam == 1 | frfam2 == 1

gen supported_samecomm = frfam_supported*samecomm
gen frfam_samecomm = frfam*samecomm
gen frfam2_samecomm = frfam2*samecomm
gen frfam3_samecomm = frfam3*samecomm
gen shell2 = frfam + frfam2
gen shell2_samecomm = shell2*samecomm

gen supportedcl_samecommcl = frfamcl*samecommcl
gen frfamcl_samecommcl = frfamcl*samecommcl
gen frfamcl2_samecommcl = frfamcl2*samecommcl
gen frfamcl3_samecommcl = frfamcl3*samecommcl
gen shellcl2 = frfamcl + frfamcl2
gen shellcl2_samecommcl = shellcl2*samecommcl

gen category = ""
replace category = "d1 supported" if frfam_supported == 1
replace category = "d1 unsupported" if frfam == 1 & frfam_supported == 0
replace category = "d2" if frfam2 == 1
replace category = "d3" if frfam3 == 1
replace category = "d4+" if category == ""

tab category
tab samecomm

cor frfam_supported frfam_unsupported frfam frfam2 frfam3 samecomm samegroup
* Data for Table 1 
tab category samecomm

xtset municode
/*------------------------------------------------------------------------------
Table 3: Main Effects on Group Co-Membership: Colombia Friends and Family 
Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfam_supported, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam, fe vce(cluster municode)
quietly eststo: xtreg samegroup samecomm,fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam samecomm, fe vce(cluster municode)
esttab using main_lpm_frfam.tex, ///
    title(Main Effects on Group Co-Membership: Colombia Friends and Family Network sans Controls) replace

/*------------------------------------------------------------------------------
Table 4: The Effects of Longer Walks and Interactions on Group Co-Membership: 
Colombia Friends and Family Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: xtreg samegroup frfam frfam2 frfam3, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3 samecomm, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm, fe vce(cluster municode)
test frfam_supported + supported_samecomm = 0
test frfam + frfam_samecomm = 0
test frfam2 + frfam2_samecomm = 0
test frfam3 + frfam3_samecomm = 0

esttab using interaction_lpm_frfam.tex, ///
    title(The Effects of Longer Walks and Interactions on Group Co-Membership: Colombia Friends and Family Network sans Controls) replace
	
/*------------------------------------------------------------------------------
Table 4: The Effects of Longer Walks and Interactions on Group Co-Membership: 
Colombia Friends and Family Network sans Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfam_supported, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam, fe vce(cluster municode)
quietly eststo: xtreg samegroup samecomm,fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam samecomm, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3 samecomm, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm, fe vce(cluster municode)
esttab using combined_lpm_frfam.tex, ///
    title(Main Effects on Group Co-Membership: Colombia Friends and Family Network sans Controls) replace

/*------------------------------------------------------------------------------
Table XX: Main Effects on Group Co-Membership: Colombia Close Friends and Family 
Network sans Controls
------------------------------------------------------------------------------*/

eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfamcl_supported, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup samecommcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl_supported frfamcl samecommcl, fe vce(cluster municode)
esttab using main_lpm_frfamcl.tex, ///
    title(Main Effects on Group Co-Membership: Colombia Close Friends and Family Network sans Controls) replace
eststo clear

/*------------------------------------------------------------------------------
Table XX: The Effects of Longer Walks and Interactions on Group Co-Membership: 
Colombia Close Friends and Family Network sans Controls
------------------------------------------------------------------------------*/
* longer walks and communities: frfam2 and frfam3
quietly eststo: xtreg samegroup frfamcl frfamcl2 frfamcl3, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl frfamcl2 frfamcl3 samecommcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl_supported frfamcl frfamcl2 frfamcl3 samecommcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl_supported frfamcl frfamcl2 frfamcl3 samecommcl supportedcl_samecommcl frfamcl_samecommcl frfamcl2_samecommcl frfamcl3_samecommcl, fe vce(cluster municode)
test frfamcl_supported + supportedcl_samecommcl = 0
test frfamcl + frfamcl_samecommcl = 0
test frfamcl2 + frfamcl2_samecommcl = 0
test frfamcl3 + frfamcl3_samecommcl = 0

esttab using interaction_lpm_frfamcl.tex, ///
    title(The Effects of Longer Walks and Interactions on Group Co-Membership: Colombia Close Friends and Family Network sans Controls) replace
eststo clear



/*------------------------------------------------------------------------------
Table XX: Main Effects on Group Co-Membership: Colombia Close Friends and Family 
Network sans Controls
------------------------------------------------------------------------------*/

eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfamcl_supported, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup samecommcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl_supported frfamcl samecommcl, fe vce(cluster municode)
* longer walks and communities: frfam2 and frfam3
quietly eststo: xtreg samegroup frfamcl frfamcl2 frfamcl3, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl frfamcl2 frfamcl3 samecommcl, fe vce(cluster municode)
quietly eststo: xtreg samegroup frfamcl_supported frfamcl frfamcl2 frfamcl3 samecommcl, fe vce(cluster municode)
esttab using combined_lpm_frfamcl.tex, ///
    title(The Effects Dyadic Relationships on Group Co-Membership: Colombia Close Friends and Family Network sans Controls) replace
eststo clear

/*------------------------------------------------------------------------------
Selection-on-Observables Results
------------------------------------------------------------------------------*/
local controls "difchoice1 difwin1 sumwin1 sumurban sumfemale sumyage sumysch nummarried sumcons sumhhsize difurban diffemale difyage difysch difmarried difcons difhhsize maxgpffopt"

/*------------------------------------------------------------------------------
Table XX: Main Effects on Group Co-Membership: Colombia Friends and Family Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfam_supported `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam samecomm `controls', fe vce(cluster municode)
esttab using main_controls_frfam.tex, ///
    title(Main Effects on Group Co-Membership: Colombia Close Friends and Family Network with Controls) replace

/*------------------------------------------------------------------------------
Table XX: The Effects of Longer Walks and Interactions on Group Co-Membership: Colombia Friends and Family Network with Controls
------------------------------------------------------------------------------*/
eststo clear
* longer walks and communities: frfam2 and frfam3
quietly eststo: xtreg samegroup frfam frfam2 frfam3 `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3 samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm `controls', fe vce(cluster municode)
esttab using interaction_controls_frfam.tex, ///
    title(The Effects of Longer Walks and Interactions on Group Co-Membership: Colombia Close Friends and Family Network with Controls) replace	

/*------------------------------------------------------------------------------
Table XX: The Effects Dyadic Relationships on Group Co-Membership: Colombia Friends and Family Network with Controls
------------------------------------------------------------------------------*/	
eststo clear
* support, neighborhood, distance-s walks, and community, no interactions 
quietly eststo: xtreg samegroup frfam_supported `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3 `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam frfam2 frfam3 samecomm `controls', fe vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm `controls', fe vce(cluster municode)
esttab using combined_controls_frfam.tex, ///
    title(Main Effects on Group Co-Membership: Colombia Friends and Family Network with Controls) replace

/*------------------------------------------------------------------------------
Export of Conditional Expectations from Risk Pooling Group Co-Membership
------------------------------------------------------------------------------*/
quietly xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm, fe vce(cluster municode)
*cd "C:\Users\Daniel Putman\Dropbox\Research\Colombia_Scope\Results"
cd "C:\Users\putmands\Dropbox\Research\Colombia_Scope\Results"
putexcel set cond_exp_samegroup, replace sheet("results")
putexcel A1 = "dyadic_relationship"
putexcel B1 = "coef_sum"
putexcel C1 = "se"
* supported, between communities
lincom frfam + frfam_supported
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
lincom frfam + frfam_supported + samecomm + frfam_samecomm + supported_samecomm
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
*distance3, within community
lincom frfam3 + samecomm + frfam3_samecomm
sca x = r(estimate)
sca y = r(se)
putexcel A9 = "Distance-3, Within Community"
putexcel B9 = x
putexcel C9 = y
sca drop x y



*support, neighborhood, distance-s walks, and community, no interactions 

eststo clear
quietly eststo: reg samegroup frfam_supported frfam samecomm, vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam samecomm, fe vce(cluster municode)
quietly eststo: logit samegroup frfam_supported frfam samecomm, vce(cluster municode)
quietly eststo: logit samegroup frfam_supported frfam samecomm i.municode, vce(cluster municode)
quietly eststo: reg samegroup frfam_supported frfam frfam2 frfam3 samecomm, vce(cluster municode)
quietly eststo: xtreg samegroup frfam_supported frfam frfam2 frfam3 samecomm, fe vce(cluster municode)
quietly eststo: logit samegroup frfam_supported frfam frfam2 frfam3 samecomm, vce(cluster municode)
quietly eststo: logit samegroup frfam_supported frfam frfam2 frfam3 samecomm i.municode, vce(cluster municode)

esttab using main_logit_frfam.tex, ///
    title(Panel Logitistic Regression: Group Co-Membership on Colombia Friends and Family Network) replace

	
*quietly logit samegroup i.frfam_supported i.frfam i.samecomm, vce(cluster municode)
*margins frfam_supported frfam samecomm, atmeans
*quietly reg samegroup i.frfam_supported i.frfam i.samecomm, vce(cluster municode)
*margins frfam_supported frfam samecomm, atmeans
*quietly xtreg samegroup i.frfam_supported i.frfam i.samecomm, fe vce(cluster municode)
*margins frfam_supported frfam samecomm, atmeans


logit samegroup frfam frfam2 frfam3, vce(cluster municode)
logit samegroup frfam_supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm, vce(cluster municode)


eststo clear
local am "sumyage difyage both_highed both_lowed sumcons_ppp difcons_ppp"
quietly eststo: reg frfam `am', vce(cluster municode)
quietly eststo: reg samecomm `am', vce(cluster municode)
esttab using colombia_am.tex, ///
    title(Assortative Matching in Tanzania Risk Sharing Networks and Communities) replace


reg samegroup frfam_supported frfam frfam2 frfam3 samecomm i.municode, vce(cluster municode)
estat vif
	
	
/*------------------------------------------------------------------------------
Table 9: Groups Analysis
------------------------------------------------------------------------------*/
clear
cd "~\radius\Colombia\Data"
import delim "defaults_by_group.csv"
cd "~\radius\Colombia\Results"

xtset municode
/*------------------------------------------------------------------------------
Table 9: Defaults
------------------------------------------------------------------------------*/
eststo clear
quietly eststo: xtreg p_default nn density_supported nn_x_supported choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn density nn_x_density choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn density2 nn_x_density2 choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn density3 nn_x_density3 choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn density_comm nn_x_density_comm choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
esttab using defaults.tex, ///
    title(Defaults by Group using Colombia Friends and Family Network) replace
eststo clear

/*------------------------------------------------------------------------------
Table 10: Defaults, Close Network
------------------------------------------------------------------------------*/
eststo clear
quietly eststo: xtreg p_default nn densitycl_supported nn_x_supportedcl choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn densitycl nn_x_densitycl choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn densitycl2 nn_x_densitycl2 choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn densitycl3 nn_x_densitycl3 choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
quietly eststo: xtreg p_default nn densitycl_comm nn_x_densitycl_comm choice p_female yage p_urb ysch p_married lcons hhsize, fe vce(cluster municode)
esttab using defaults_close.tex, ///
    title(Defaults by Group using Colombia Close Friends and Family Network) replace
eststo clear

