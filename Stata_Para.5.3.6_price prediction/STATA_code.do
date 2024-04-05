
*******CODE FOR PREDICTION OF GERMAN DAY-AHEAD PRICE - section 5.3.6 ******************

import delimited selectdata.csv, clear

gen double stdate = clock(datestamp, "YMD hm") 
format stdate %tc 
rename stdate tseries
tsset tseries, delta(1 hour)

gen f_daprice3 =.
local start 19200
local end 37248
forv T=`start'(24)`end' {
	local T_temp=`T'+1
	quietly regress de_daprice de_dfcast wind_fcast_de pv_fcast_de L24.de_daprice L48.oil_price L48.coal_price L48.gas_price L48.eua_price dayofwk_2 dayofwk_3 dayofwk_4 dayofwk_5 dayofwk_6 dayofwk_7 week_2 week_3 week_4 week_5 week_6 week_7 week_8 week_9 week_10 week_11 week_12 week_13 week_14 week_15 week_16 week_17 week_18 week_19 week_20 week_21 week_22 week_23 week_24 week_25 week_26 week_27 week_28 week_29 week_30 week_31 week_32 week_33 week_34 week_35 week_36 week_37 week_38 week_39 week_40 week_41 week_42 week_43 week_44 week_45 week_46 week_47 week_48 week_49 week_50 week_51 week_52 week_53 hour_2 hour_3 hour_4 hour_5 hour_6 hour_7 hour_8 hour_9 hour_10 hour_11 hour_12 hour_13 hour_14 hour_15 hour_16 hour_17 hour_18 hour_19 hour_20 hour_21 hour_22 hour_23 hour_24 in 1/`T'
while `T_temp' < `T'+25 {
quietly replace f_daprice3 = _b[_cons] + _b[de_dfcast]*de_dfcast[`T_temp'] + _b[wind_fcast_de]*wind_fcast_de[`T_temp'] + _b[pv_fcast_de]*pv_fcast_de[`T_temp'] + _b[L24.de_daprice]*de_daprice[`T_temp'-24] + _b[L48.oil_price]*oil_price[`T_temp'-48]  + _b[L48.coal_price]*coal_price[`T_temp'-48] + _b[L48.gas_price]*gas_price[`T_temp'-48] + _b[L48.eua_price]*eua_price[`T_temp'-48] + _b[hour_2]*hour_2[`T_temp'] + _b[hour_3]*hour_3[`T_temp'] + _b[hour_4]*hour_4[`T_temp'] + _b[hour_5]*hour_5[`T_temp'] + _b[hour_6]*hour_6[`T_temp'] + _b[hour_7]*hour_7[`T_temp'] + _b[hour_8]*hour_8[`T_temp'] + _b[hour_9]*hour_9[`T_temp'] + _b[hour_10]*hour_10[`T_temp'] + _b[hour_11]*hour_11[`T_temp'] + _b[hour_12]*hour_12[`T_temp'] + _b[hour_13]*hour_13[`T_temp'] + _b[hour_14]*hour_14[`T_temp'] + _b[hour_15]*hour_15[`T_temp'] + _b[hour_16]*hour_16[`T_temp'] + _b[hour_17]*hour_17[`T_temp'] + _b[hour_18]*hour_18[`T_temp'] + _b[hour_19]*hour_19[`T_temp'] + _b[hour_20]*hour_20[`T_temp'] + _b[hour_21]*hour_21[`T_temp'] + _b[hour_22]*hour_22[`T_temp'] + _b[hour_23]*hour_23[`T_temp'] + _b[hour_24]*hour_24[`T_temp'] + _b[week_2]*week_2[`T_temp'] + _b[week_3]*week_3[`T_temp'] + _b[week_4]*week_4[`T_temp'] + _b[week_5]*week_5[`T_temp'] + _b[week_6]*week_6[`T_temp'] + _b[week_7]*week_7[`T_temp'] + _b[week_8]*week_8[`T_temp'] + _b[week_9]*week_9[`T_temp'] + _b[week_10]*week_10[`T_temp'] + _b[week_11]*week_11[`T_temp'] + _b[week_12]*week_12[`T_temp'] + _b[week_13]*week_13[`T_temp'] + _b[week_14]*week_14[`T_temp'] + _b[week_15]*week_15[`T_temp'] + _b[week_16]*week_16[`T_temp'] + _b[week_17]*week_17[`T_temp'] + _b[week_18]*week_18[`T_temp'] + _b[week_19]*week_19[`T_temp'] + _b[week_20]*week_20[`T_temp'] + _b[week_21]*week_21[`T_temp'] + _b[week_22]*week_22[`T_temp'] + _b[week_23]*week_23[`T_temp'] + _b[week_24]*week_24[`T_temp'] + _b[week_25]*week_25[`T_temp'] + _b[week_26]*week_26[`T_temp'] + _b[week_27]*week_27[`T_temp'] + _b[week_28]*week_28[`T_temp'] + _b[week_29]*week_29[`T_temp'] + _b[week_30]*week_30[`T_temp'] + _b[week_31]*week_31[`T_temp'] + _b[week_32]*week_32[`T_temp'] + _b[week_33]*week_33[`T_temp'] + _b[week_34]*week_34[`T_temp'] + _b[week_35]*week_35[`T_temp'] + _b[week_36]*week_36[`T_temp'] + _b[week_37]*week_37[`T_temp'] + _b[week_38]*week_38[`T_temp'] + _b[week_39]*week_39[`T_temp'] + _b[week_40]*week_40[`T_temp'] + _b[week_41]*week_41[`T_temp'] + _b[week_42]*week_42[`T_temp'] + _b[week_43]*week_43[`T_temp'] + _b[week_44]*week_44[`T_temp'] + _b[week_45]*week_45[`T_temp'] + _b[week_46]*week_46[`T_temp'] + _b[week_47]*week_47[`T_temp'] + _b[week_48]*week_48[`T_temp'] + _b[week_49]*week_49[`T_temp'] + _b[week_50]*week_50[`T_temp'] + _b[week_51]*week_51[`T_temp'] + _b[week_52]*week_52[`T_temp'] + _b[week_53]*week_53[`T_temp'] + _b[dayofwk_2]*dayofwk_2[`T_temp'] + _b[dayofwk_3]*dayofwk_3[`T_temp'] + _b[dayofwk_4]*dayofwk_4[`T_temp'] + _b[dayofwk_5]*dayofwk_5[`T_temp'] + _b[dayofwk_6]*dayofwk_6[`T_temp'] + _b[dayofwk_7]*dayofwk_7[`T_temp'] in `T_temp'/`T_temp'	
local T_temp=`T_temp'+1
	}
}

**Compute RMSFE

gen e3 = de_daprice - f_daprice3
summarize e3
di sqrt(r(mean)^2 + r(sd)^2)

export excel using "predicteddata.xlsx"

clear
