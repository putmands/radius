set more off
set matsize 2500
cd "~\radius\Colombia_Scope\Data\"
import delim "colombia_dyads_with_communities.csv", clear

gen frfam_unsupported = frfam*(1-frfam_supported)
replace frfam2 = 0 if frfam == 1
replace frfam3 = 0 if frfam == 1 | frfam2 == 1

gen supported_samecomm = frfam_supported*samecomm
gen frfam_samecomm = frfam*samecomm
gen frfam2_samecomm = frfam2*samecomm
gen frfam3_samecomm = frfam3*samecomm
rename frfam_supported supported

xtset municode

eststo clear
quietly eststo: xtreg samegroup supported frfam frfam2 frfam3 samecomm supported_samecomm frfam_samecomm frfam2_samecomm frfam3_samecomm, fe vce(cluster municode)


cd "~\radius\Tanzania\Data\"
import delim "tanzania_dyadic_with_communities.csv", clear
cd "~\radius\Tanzania\Tables\"

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

quietly eststo: ngreg any_transfer supported frfam frfam2 frfam3 samecomm ///
	supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
quietly eststo: ngreg reciprocal_transfer supported frfam frfam2 frfam3 samecomm ///
	supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
quietly eststo: ngreg total_transfers supported frfam frfam2 frfam3 samecomm ///
	supported_samecomm frfam_samecomm frfam2_samecomm, id(ha hb) symmetric
esttab using interactions_nocontrols.tex, ///
    title(Fully Satured Specifications sans Controls) replace