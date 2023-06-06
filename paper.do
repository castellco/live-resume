*******************************************************
*******************************************************
****                                               ****
****                 Andrew B. Hall                ****
****        Ph.D. Candidate, Harvard University    ****
****              andrewbenjaminhall.com           ****
****               hall@fas.harvard.edu            ****  
****            APSR Replication Code For:         ****
**** "What Happens When Extremists Win Primaries?" ****
****                                               ****
*******************************************************
*******************************************************

*** Note: run this code before R code!


*** set working directory
cd "C:/Users/Carolina/Documents/0cast/master/15_causal-inference/final/Hall_Extremist_Primaries_APSR_Replication"

*** main replication data
use primary_analysis, clear

*** create main cutoff value in terms of ideological distance
sum absdist, d
gl cutoff = r(p50)

***************
**  Figure 1 **
***************

* See R code

**************
**  Table 1 **
**************

	* sample sizes for table
	count if safe_for_party == 1
	count if competitive == 1
	count if safe_for_party == 0 & competitive == 0
	
	local tot3 = r(N)
	
	local tot4 = 0

	* loop to create values for cells in table
	foreach v in fully_open_general this_primary_open_other_inc this_primary_inc_other_open both_primaries_inc {

		count if `v' == 1 & safe_for_party == 1
		local v0 = r(N)
		count if `v' == 1 & competitive == 1
		local v1 = r(N)
		count if `v' == 1 & safe_for_party == 0 & competitive == 0
		local v2 = r(N)
	
		local tot = `v0' + `v1' + `v2'
		local tot4 = `tot4' + `tot'
	
	}


***************
**  Figure 2 **
***************

* See R code

**************
**  Table 2 **
**************

	* Note: each regression run twice to get max of robust and conventional SEs
	
	
	*** "dv" is vote share in general
	*** "dv_win" is victory in general
	
	
	* column 1
	reg dv treat rv rv_treat if margin < .05 & absdist > $cutoff
	reg dv treat rv rv_treat if margin < .05 & absdist > $cutoff, r
	
	* column 2
	reg dv treat rv rv2 rv3 if absdist > $cutoff
	reg dv treat rv rv2 rv3 if absdist > $cutoff, r
	
	* column 3, IK bandwidth
	* preserve because rdob doesn't accept if statements
	preserve
	keep if absdist > $cutoff
	rdob dv rv
	restore
	
	* set these locals to output from rdob
	local treat_ik1 = -.07505812
	local se_ik1 = .04147984
l	ocal ik1 = .08507714
	
	* sample size for IK, column 3
	count if absdist > $cutoff & margin < `ik1'
	local n_ik1 = r(N)
	
	* column 4
	reg dv_win treat rv rv_treat  if margin < .05 & absdist > $cutoff
	reg dv_win treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	* column 5
	reg dv_win treat rv rv2 rv3  if absdist > $cutoff
	reg dv_win treat rv rv2 rv3  if absdist > $cutoff, r
	
	* column 6
	preserve
	keep if absdist > $cutoff
	rdob dv_win rv
	restore
		
	local ik2 = .09679846
	local treat_ik2 =  -.34833014
	local se_ik2 =  .16635613
	
	count if absdist > $cutoff & margin < `ik2'
	local n_ik2 = r(N)
	
	* test for footnote 28
	gen treat_safe = treat*safe
	reg dv_win treat treat_safe safe rv rv2 rv3 if absdist > $cutoff, r
	drop treat_safe
	
	
	
***************************
** Results for Figure 3  **
***************************

	* overall effect, re-do from Table 2
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff, r

	* open seats
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff & fully_open_general == 1, r
	
	* safe seats
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff & safe_for_party == 1, r


***************************
** Results for Figs 4, 5 **
***************************


	*** vote share effect across cutoffs
	
	preserve
	matrix B = J(200, 5, .)
	local count = 1
	sum absdist
	local m = r(max)
	forvalues j=0(.01)`m'{
		cap reg dv treat rv rv2 rv3 if absdist > `j', r
		if !_rc {
			matrix B[`count', 1] = `j'
			matrix B[`count', 2] = _b[treat]
			matrix B[`count', 3] = _b[treat] - 1.96*_se[treat]
			matrix B[`count', 4] = _b[treat] + 1.96*_se[treat]
			matrix B[`count', 5] = e(N)
			local count = `count' + 1
		}
	}
	
	svmat B
	keep B*
	keep if B1 != .
	rename B1 cutoff
	rename B2 est
	rename B3 lower
	rename B4 upper
	rename B5 n
	saveold cutoff_vote_for_r, replace
	
	restore
	
	*** victory effect across cutoffs
	
	preserve
	matrix B = J(200, 5, .)
	local count = 1
	sum absdist
	local m = r(max)
	forvalues j=0(.01)`m'{
		cap reg dv_win treat rv rv2 rv3 if absdist > `j', r
		if !_rc {
			matrix B[`count', 1] = `j'
			matrix B[`count', 2] = _b[treat]
			matrix B[`count', 3] = _b[treat] - 1.96*_se[treat]
			matrix B[`count', 4] = _b[treat] + 1.96*_se[treat]
			matrix B[`count', 5] = e(N)
			local count = `count' + 1
		}
	}
	
	svmat B
	keep B*
	keep if B1 != .
	rename B1 cutoff
	rename B2 est
	rename B3 lower
	rename B4 upper
	rename B5 n
	saveold cutoff_win_for_r, replace
	
	restore


************************
**  Data for Figure 6 **
************************

	preserve
	gen cand_abs = abs(winner_score)
	keep cand_abs dwnom1 dem dv_win
	saveold dwnom_for_r, replace
	restore

**************
**  Table 3 **
**************

	* generate interaction variable
	gen treat_safe = treat*safe_for_party

	* column 1
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist > $cutoff  & dem==0 & margin < .05
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist > $cutoff & dem==0 & margin < .05, r

	* column 2
	reg dwnom1 treat treat_safe safe_for_party rv rv2 rv3  if absdist> $cutoff & dem==0
	reg dwnom1 treat treat_safe safe_for_party rv rv2 rv3  if absdist> $cutoff & dem==0, r

	
	preserve
	keep if absdist > $cutoff & dem == 0
	rdob dwnom1 rv
	restore

	local ik1 = .06492315

	*column 3
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist> $cutoff & dem==0 & dv != . & abs(rv) < `ik1'
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist> $cutoff & dem==0 & dv != . & abs(rv) < `ik1', r

	* column 4
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist > $cutoff & dem==1 & margin < .05
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist > $cutoff & dem==1 & margin < .05, r

	* column 5
	reg dwnom1 treat treat_safe safe_for_party rv rv2 rv3  if absdist> $cutoff & dem==1
	reg dwnom1 treat treat_safe safe_for_party rv rv2 rv3  if absdist> $cutoff & dem==1, r

	preserve
	keep if absdist > $cutoff & dem == 1
	rdob dwnom1 rv
	restore

	local ik2 = .21727154

	* column 6
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist> $cutoff & dem==1  & abs(rv) < `ik2'
	reg dwnom1 treat treat_safe safe_for_party rv rv_treat  if absdist> $cutoff & dem==1 & abs(rv) < `ik2', r

***************************
** Results for Figure 7  **
***************************

	preserve
	rename dv dv1
	rename dv_win dv_win1
	matrix B = J(10, 7, .)
	quietly forvalues j=1(1)5 {
		reg dv`j' treat rv rv2 rv3  if absdist > $cutoff, r
		local se_tmp = _se[treat]
		reg dv`j' treat rv rv2 rv3 absdist if absdist > $cutoff 
		local se = max(`se_tmp', _se[treat])
		matrix B[`j', 1] = _b[treat]
		matrix B[`j', 2] = _b[treat] - 1.96*`se'
		matrix B[`j', 3] = _b[treat] + 1.96*`se'
		reg dv_win`j' treat rv rv2 rv3  if absdist > $cutoff  , r
		local se_tmp = _se[treat]
		reg dv_win`j' treat rv rv2 rv3  if absdist > $cutoff 
		matrix B[`j', 4] = _b[treat]
		matrix B[`j', 5] = _b[treat] - 1.96*`se'
		matrix B[`j', 6] = _b[treat] + 1.96*`se'
		matrix B[`j', 7] = `j'
	
	}
	
	svmat B
	keep B*
	rename B1 est_vote
	rename B2 lower_vote
	rename B3 upper_vote
	rename B4 est_win
	rename B5 lower_win
	rename B6 upper_win
	keep if est_vote != .
	saveold downstream_for_r, replace
	
	restore

***************************
** Results for Figure 8  **
***************************
	
	preserve
	matrix B = J(10, 7, .)
	rename dwnom1 dw0
	local i = 1
	quietly forvalues j=0(1)4 {
		reg dw`j' treat rv rv2 rv3 if absdist > $cutoff & dem==1, r
		local se_tmp = _se[treat]
		reg dw`j' treat rv rv2 rv3 if absdist > $cutoff & dem == 1
		local se = max(`se_tmp', _se[treat])
		matrix B[`i', 1] = _b[treat]
		matrix B[`i', 2] = _b[treat] - 1.96*`se'
		matrix B[`i', 3] = _b[treat] + 1.96*`se'
		reg dw`j' treat rv rv2 rv3 if absdist > $cutoff & dem==0  , r
		local se_tmp = _se[treat]
		reg dw`j' treat rv rv2 rv3 if absdist > $cutoff & dem==0
		matrix B[`i', 4] = _b[treat]
		matrix B[`i', 5] = _b[treat] - 1.96*`se'
		matrix B[`i', 6] = _b[treat] + 1.96*`se'
		matrix B[`i', 7] = `j'
		local i = `i' + 1
	}
	
	svmat B
	keep B*
	rename B1 est_dem
	rename B2 lower_vote
	rename B3 upper_vote
	rename B4 est_rep
	rename B5 lower_win
	rename B6 upper_win
	keep if est_dem != .
	saveold downdwnom_for_r, replace
	
	restore

***************************
** Results for Figure 9  **
***************************

	use primary_analysis, clear

	sum absdist, d
	gl cutoff = r(p50)
	
	preserve
	
	matrix B = J(4, 4, .)
	
	reg winner_female treat rv rv2 rv3 if absdist > $cutoff, r
	matrix B[1,1] = _b[treat]
	matrix B[1,2] = _b[treat] - 1.96*_se[treat]
	matrix B[1,3] = _b[treat] + 1.96*_se[treat]
	matrix B[1,4] = 1
	
	reg inc_winner treat rv rv2 rv3 if absdist > $cutoff, r
	matrix B[2,1] = _b[treat]
	matrix B[2,2] = _b[treat] - 1.96*_se[treat]
	matrix B[2,3] = _b[treat] + 1.96*_se[treat]
	matrix B[2,4] = 2
	
	reg qual treat rv rv2 rv3 if absdist > $cutoff, r
	matrix B[4,1] = _b[treat]
	matrix B[4,2] = _b[treat] - 1.96*_se[treat]
	matrix B[4,3] = _b[treat] + 1.96*_se[treat]
	matrix B[4,4] = 4
	

	
	reg winner_share treat rv rv2 rv3 if absdist > $cutoff, r
	matrix B[3,1] = _b[treat]
	matrix B[3,2] = _b[treat] - 1.96*_se[treat]
	matrix B[3,3] = _b[treat] + 1.96*_se[treat]
	matrix B[3,4] = 3
	
	svmat B
	keep B1-B4
	rename B1 est
	rename B2 lower
	rename B3 upper
	rename B4 type
	keep if est != .
	saveold cand_characteristics, replace
	
	restore
	
	*** check effect in level for statement in paper
	reg prim_total_winner treat rv rv2 rv3 if absdist > $cutoff, r
		

	
**************
**  Table 4 **
**************

	* get IK bandwidths
	preserve
	keep if absdist > $cutoff
	rdob group_share rv
	rdob dv_win rv
	restore
	
	local ik_group =  .04682024
	local ik_cov = .09679846
	
	* column 1
	reg group_share treat rv rv_treat if margin < `ik_group' & absdist > $cutoff, r
	
	* standardize congruence measure to run 0 to 1 for sample
	sum cov2dc if absdist > $cutoff
	replace cov2dc = (cov2dc - r(min))/(r(max)-r(min))
	
	gen treat_cov2dc = treat*cov2dc
	
	* column 2
	reg dv_win treat treat_cov2dc cov2dc rv rv_treat if margin < `ik_cov' & absdist > $cutoff, r
	
	
	*** group total $ for footnote
	gen log_q = log(tot_amountQ)
	reg log_q treat rv rv2 rv3 if absdist > $cutoff, r
	
	drop log_q
	
*********************
*** APPENDIX ********
*********************

use primary_analysis, clear

**************
** Table A1 **
**************

	local cutoff = 0
	local mar = 100
	
	sum dv if treat == 0 & margin < `mar' & absdist > $cutoff
	
	sum dv if treat == 1 & margin < `mar' & absdist > $cutoff
	
	sum dv_win if treat == 0 & margin < `mar' & absdist > $cutoff
	
	sum dv_win if treat == 1 & margin < `mar' & absdist > $cutoff
	
	sum party_share if treat == 0 & margin < `mar' & absdist > $cutoff
	sum party_share if treat == 1 & margin < `mar' & absdist > $cutoff
	
	sum group_share if treat == 0 & margin < `mar' & absdist > $cutoff
	
	sum group_share if treat == 1 & margin < `mar' & absdist > $cutoff
	sum tot_amountY if treat == 0 & margin < `mar' & absdist > $cutoff
	
	sum tot_amountY if treat == 1 & margin < `mar' & absdist > $cutoff
	
	sum tot_amountQ if treat == 0 & margin < `mar' & absdist > $cutoff
	sum tot_amountQ if treat == 1 & margin < `mar' & absdist > $cutoff
	

***************
** Figure A1 **
***************

	* See R code

**************
** Table A2 **
**************

	use primary_analysis, clear
	sum absdist, d
	gl cutoff = r(p50)
	keep if fully_open_general == 1
	
	reg dv treat rv rv_treat if margin < .05 & absdist > $cutoff
	reg dv treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	
	
	reg dv treat rv rv2 rv3   if absdist > $cutoff
	reg dv treat rv rv2 rv3  if absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob dv rv
	
	restore
	
	local treat_ik1 = -.0900019
	local se_ik1 = .04112608
	local ik1 =  .11708822
	
	reg dv_win treat rv rv_treat  if margin < .05 & absdist > $cutoff
	reg dv_win treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	
	reg dv_win treat rv rv2 rv3  if absdist > $cutoff
	
	reg dv_win treat rv rv2 rv3  if absdist > $cutoff, r
	
	
	preserve
	keep if absdist > $cutoff
	rdob dv_win rv
	
	restore
	
	local ik2 = .09972145
	local treat_ik2 = -.43203814
	local se_ik2 = .191221
	
**************
** Table A3 **
**************

	use primary_analysis, clear
	sum absdist, d
	gl cutoff = r(p50)
	
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff & fully_open_general==1, r
	reg dv treat rv rv2 rv3 if absdist > $cutoff & fully_open_general == 1, r
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff & this_primary_inc_other_open == 1, r
	reg dv treat rv rv2 rv3 if absdist > $cutoff & this_primary_inc_other_open == 1, r
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff & this_primary_open_other_inc==1, r
	reg dv treat rv rv2 rv3 if absdist > $cutoff & this_primary_open_other_inc==1, r
	
**************
** Table A4 **
**************
	
	gen treat_dem = treat*dem
		
	reg dv treat treat_dem dem rv rv_treat  if margin < .05 & absdist > $cutoff
	reg dv treat treat_dem dem rv rv2 rv3  if absdist > $cutoff, r

	preserve
	keep if absdist > $cutoff
	rdob dv rv
	restore
	local bw1 = .08507714
	
	reg dv treat treat_dem dem rv rv_treat if absdist > $cutoff & margin < `bw1', r
	
	reg dv_win treat treat_dem dem rv rv_treat  if margin < .05 & absdist > $cutoff
	reg dv_win treat treat_dem dem rv rv2 rv3  if absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob dv_win rv
	restore
	local bw2 =  .09679846
	
	reg dv_win treat treat_dem dem rv rv_treat if absdist > $cutoff & margin < `bw2', r

	*** test of distance across parties
	reg absdist dem, r
	
**************
** Table A5 **
**************

	use primary_analysis, clear
	
	
	sum absdist, d
	gl cutoff = r(p50)
	
	gen pres_normal_vote = abs(lag_pnv - 0.5)
	
	* pres normal vote, distance from 50
	
	reg pres_normal_vote treat rv rv_treat if margin < .05 & absdist > $cutoff 
	reg pres_normal_vote treat rv rv_treat if margin < .05 & absdist > $cutoff , r
	
	preserve
	keep if absdist > $cutoff
	rdob pres_normal_vote rv
	restore
	
	reg pres_normal_vote treat rv rv2 rv3  if absdist > $cutoff, 
	reg pres_normal_vote treat rv rv2 rv3  if absdist > $cutoff, r
	
	* extremist share of primary donations
	
	reg prim_share treat rv rv_treat  if margin < .05 & absdist > $cutoff
	reg prim_share treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob prim_share rv
	restore
	
	reg prim_share treat rv rv2 rv3 if absdist > $cutoff
	reg prim_share treat rv rv2 rv3 if absdist > $cutoff, r
	
	* extremist share of primary PAC donations
	
	reg prim_pac_share treat rv rv_treat if margin < .05 & absdist > $cutoff
	reg prim_pac_share treat rv rv_treat if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob prim_pac_share rv
	restore
	
	reg prim_pac_share treat rv rv2 rv3  if absdist > $cutoff
	reg prim_pac_share treat rv rv2 rv3  if absdist > $cutoff, r
	
	* extremist total primary donations
	
	* rescale to 100,000 dollars
	replace prim_total0 = prim_total0/100000
	
	reg prim_total0 treat rv rv_treat  if margin < .05 & absdist > $cutoff
	reg prim_total0 treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob prim_total0 rv
	restore
	
	reg prim_total0 treat rv rv2 rv3   if absdist > $cutoff
	reg prim_total0 treat rv rv2 rv3   if absdist > $cutoff, r
	
	* lagged dw-nom score
	
	reg abs_dw_lag treat rv rv_treat if margin < .05 & absdist > $cutoff
	reg abs_dw_lag treat rv rv_treat if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob abs_dw_lag rv
	restore
	
	reg abs_dw_lag treat rv rv2 rv3  if absdist > $cutoff 
	reg abs_dw_lag treat rv rv2 rv3  if absdist > $cutoff , r
	
	
	
	* lagged w-nom score
	* note inclusion of year FEs, see footnote 41
	
	reg abs_lag_wnom treat rv rv_treat i.year if margin < .05 & absdist > $cutoff
	reg abs_lag_wnom treat rv rv_treat i.year if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	* residualize for year FEs since rdob won't accept them
	reg abs_lag_wnom i.year
	predict res, r
	reg rv i.year
	predict res_rv, r
	
	rdob res res_rv
	
	restore
	
	reg abs_lag_wnom treat rv rv2 rv3 i.year  if absdist > $cutoff 
	reg abs_lag_wnom treat rv rv2 rv3 i.year  if absdist > $cutoff , r
	
	* lagged vote share
	
	reg dv_lag treat rv rv_treat if margin < .05 & absdist > $cutoff
	reg dv_lag treat rv rv_treat if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	rdob dv_lag rv
	restore
	
	reg dv_lag treat rv rv2 rv3  if absdist > $cutoff
	reg dv_lag treat rv rv2 rv3  if absdist > $cutoff , r
	
	
	* lagged victory
	
	reg dv_win_lag treat rv rv_treat  if margin < .05 & absdist > $cutoff
	reg dv_win_lag treat rv rv_treat  if margin < .05 & absdist > $cutoff, r
	
	preserve
	keep if absdist > $cutoff
	
	
	rdob dv_win_lag rv
	restore
	
	reg dv_win_lag treat rv rv2 rv3  if absdist > $cutoff
	reg dv_win_lag treat rv rv2 rv3  if absdist > $cutoff , r
	
	
***************
** Figure A2 **
***************
	use primary_analysis, clear
	sum absdist, d
	gl cutoff = r(p50)
	
	gen pres_normal_vote = abs(lag_pnv - 0.5)
	keep if absdist > $cutoff
	keep if abs(rv) < .2
	
	preserve

	keep rv pres_normal_vote prim_share prim_pac_share prim_total0 abs_dw_lag abs_lag_wnom dv_win_lag dv_lag

	gen bin = .
	local count = 1
	forvalues j = -.2(.02).2 {
		replace bin = `count' if rv >= `j' & rv < `j'+.02
		local count = `count' + 1
	}

	saveold for_balance_plots, replace
	
	restore

	gen bin = .
	local count = 1
	forvalues j = -.2(.02).2 {
		replace bin = `count' if rv >= `j' & rv < `j'+.02
		local count = `count' + 1
	}

	binscatter pres_normal_vote rv, xq(bin) rd(0) savedata(graph1) replace
	binscatter prim_share rv, xq(bin) rd(0) savedata(graph2) replace
	binscatter prim_pac_share rv, xq(bin) rd(0) savedata(graph3) replace
	binscatter prim_total0 rv, xq(bin) rd(0) savedata(graph4) replace
	binscatter abs_dw_lag rv, xq(bin) rd(0) savedata(graph5) replace
	binscatter abs_lag_wnom rv, xq(bin) rd(0) savedata(graph6) replace
	binscatter dv_lag rv, xq(bin) rd(0) savedata(graph7) replace
	binscatter dv_win_lag rv, xq(bin) rd(0) savedata(graph8) replace
	
	
**************
** Table A6 **
**************
	
	use primary_analysis, clear
	
	sum absdist
	replace absdist = (absdist - r(min))/(r(max)-r(min))
	gen treat_dist = absdist * treat
	
	reg dv treat treat_dist absdist rv rv_treat if abs(rv) < .05, r
	test treat + treat_dist == 0
	
	reg dv treat treat_dist absdist rv rv2 rv3, r
	test treat + treat_dist == 0
	
	rdob dv rv
	local ik1 = .07964827
	reg dv treat treat_dist absdist rv rv_treat if abs(rv) < `ik1', r
	test treat + treat_dist == 0
	
	reg dv_win treat treat_dist absdist rv rv_treat if abs(rv) < .05, r
	test treat + treat_dist == 0
	
	reg dv_win treat treat_dist absdist rv rv2 rv3, r
	test treat + treat_dist == 0
	
	rdob dv_win rv
	local ik2 = .0600189
	reg dv_win treat treat_dist absdist rv rv_treat if abs(rv) < `ik2', r
	test treat + treat_dist == 0
	
*******************
** Figures A3-A7 **
*******************

	use primary_analysis, clear
	sum absdist, d
	gl cutoff = r(p50)
	replace rv = 100*rv
	
	matrix B = J(50, 5, .)
	quietly forvalues j=3(2)50 {
		reg dv treat rv_treat if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 1] = _b[treat]
		reg dv treat rv rv2 if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 2] = _b[treat]
		reg dv treat rv rv2 rv3 if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 3] = _b[treat]
		reg dv treat rv rv2 rv3 rv4 if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 4] = _b[treat]
		matrix B[`j', 5] = `j'
	}
	
	preserve
	svmat B
	keep B1-B5
	keep if B1 != .
	saveold "for_robust_dv.dta", replace
	restore
	
	
	matrix B = J(50, 5, .)
	quietly forvalues j=3(2)50 {
		reg dv_win treat rv_treat  if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 1] = _b[treat]
		reg dv_win treat rv rv2  if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 2] = _b[treat]
		reg dv_win treat rv rv2 rv3  if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 3] = _b[treat]
		reg dv_win treat rv rv2 rv3 rv4  if abs(rv) < `j' & absdist > $cutoff, r
		matrix B[`j', 4] = _b[treat]
		matrix B[`j', 5] = `j'
	}
	
	preserve
	svmat B
	keep B1-B5
	keep if B1 != .
	saveold "for_robust_dv_win.dta", replace
	restore
	
	*** for local linear plots
	use primary_analysis, clear
	replace rv = 100*rv
	
	sum absdist, d
	gl cutoff = r(p50)
	set matsize 10000
	
	matrix B = J(500, 4, .)
	local j = 1
	quietly forvalues i=3(.1)50 {
		reg qual treat rv_treat if abs(rv) < `i' & absdist > $cutoff, r
		matrix B[`j', 1] = _b[treat]
		matrix B[`j', 2] = _b[treat] - 1.96 * _se[treat]
		matrix B[`j', 3] = _b[treat] + 1.96*_se[treat]
		matrix B[`j', 4] = `i'
		local j = `j' + 1
	}
	
	preserve
	svmat B
	keep B1-B4
	keep if B1 != .
	saveold "for_robust_qual.dta", replace
	
	restore
	set matsize 10000
	
	matrix B = J(500, 4, .)
	local j = 1
	quietly forvalues i=3(.1)50 {
		reg dv treat rv_treat if abs(rv) < `i' & absdist > $cutoff, r
		matrix B[`j', 1] = _b[treat]
		matrix B[`j', 2] = _b[treat] - 1.96 * _se[treat]
		matrix B[`j', 3] = _b[treat] + 1.96*_se[treat]
		matrix B[`j', 4] = `i'
		local j = `j' + 1
	}
	
	preserve
	svmat B
	keep B1-B4
	keep if B1 != .
	saveold "for_robust_dv_locallinear.dta", replace
	
	
	restore
	local j = 1
	matrix B = J(500, 4, .)
	quietly forvalues i=3(.1)50 {
		reg dv_win treat rv_treat if abs(rv) < `i' & absdist > $cutoff, r
		matrix B[`j', 1] = _b[treat]
		matrix B[`j', 2] = _b[treat] - 1.96 * _se[treat]
		matrix B[`j', 3] = _b[treat] + 1.96*_se[treat]
		matrix B[`j', 4] = `i'
		local j = `j' + 1
	}
	
	preserve
	svmat B
	keep B1-B4
	keep if B1 != .
	saveold "for_robust_dv_win_locallinear.dta", replace
	restore
	
**************
** Table A7 **
**************

	use stleg_robust, clear
	
	sum absdist, d
	gl cutoff = r(p50)
	
	reg dv_vote treat rv rv2 rv3 if absdist > $cutoff, r
	reg dv_win treat rv rv2 rv3 if absdist > $cutoff, r
	

