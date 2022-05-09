* DETRENDING 

* First detrend all momentary assessment measures a La Rovine & Walls
* This is detrending with OLS regressions per individual and then adding the PersonMeans to the residuals:

* use centered version of Time: Time_cent
* centered at middle measure

* use for ID a variable with ascending numbers

* Open file: rawdata.dta

summarize Time
summarize Time_cen


tsset ID Time
tsset ID Time_cen

gen INT_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg INTraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace INT_DMDT = res if ID == `i'
	drop res
	}

gen JOY_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg JOYraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace JOY_DMDT = res if ID == `i'
	drop res
	}


gen SAD_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg SADraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace SAD_DMDT = res if ID == `i'
	drop res
	}


gen IRR_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg IRRraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace IRR_DMDT = res if ID == `i'
	drop res
	}
	
	
gen WOR_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg WORraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace WOR_DMDT = res if ID == `i'
	drop res
	}	

gen POS_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg POSraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace POS_DMDT = res if ID == `i'
	drop res
	}	

gen NEG_DMDT = .
tsset ID Time_cen
forv i = 1/`=r(imax)' {
	reg NEGraw Time_cen afternoo evening if ID == `i'
	predict res if e(sample), res 
	replace NEG_DMDT = res if ID == `i'
	drop res
	}	
	
*add the person mean 


by ID: egen INT_mean = mean(INTraw)
gen INT = INT_DMDT+INT_mean

by ID: egen JOY_mean = mean(JOYraw)
gen JOY = JOY_DMDT+JOY_mean

by ID: egen SAD_mean = mean(SADraw)
gen SAD = SAD_DMDT+SAD_mean

by ID: egen IRR_mean = mean(IRRraw)
gen IRR = IRR_DMDT+IRR_mean

by ID: egen WOR_mean = mean(WORraw)
gen WOR = WOR_DMDT+WOR_mean

by ID: egen POS_mean = mean(POSraw)
gen POS = POS_DMDT+POS_mean

by ID: egen NEG_mean = mean(NEGraw)
gen NEG = NEG_DMDT+NEG_mean


* save as spss file
*download package: http://www.radyakin.org/transfer/savespss/savespss.htm

findit savespss

savespss "raw and detrended data.sav"
