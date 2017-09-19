*Homework 1 / Question3*

clear all
cls
cd "C:\Users\yulan\Desktop\Advanced Macro\HW1"

*Data used: SCF 2007 Extract Data	*

use "C:\Users\yulan\Desktop\Advanced Macro\HW1\rscfp2007.dta", clear

*******************************
*  Part I: Replicate Table 1  *
*******************************
*  1 Earning  *

*  Construct a variable stands for earnings according definition in DGR(2011)  *

generate Earning = wageinc+0.863*bussefarminc

*  Get Quantiles of Earning  *
_pctile Earning [pweight=wgt], p(1 5 10 20 40 60 80 90 95 99)
matrix EPer=(r(r1), r(r2), r(r3), r(r4), r(r5), r(r6), r(r7), r(r8), r(r9), r(r10))
sum Earning
matrix E=(r(min), EPer, r(max))

*  Export the Resuts  *
putexcel A1=("Quantiles") B1=("0") C1=("1") D1=("5") E1=("10") F1=("20") G1=("40")H1=("60") I1=("80") J1=("90") K1=("95") L1=("99") M1=("100") using Quantiles, replace
putexcel A2=("Earnings") B2=matrix (E) using Quantiles,modify


*  2 Income 

*  Construct a variable stands for income according definition in DGR(2011)  *

generate Income = wageinc+bussefarminc+kginc+intdivinc+ssretinc+transfothinc

*  Get Quantiles of Income  *
_pctile Income [pweight=wgt], p(1 5 10 20 40 60 80 90 95 99)
matrix IncPer=(r(r1), r(r2), r(r3), r(r4), r(r5), r(r6), r(r7), r(r8), r(r9), r(r10))
sum Income
matrix Inc=(r(min), IncPer, r(max))

*  Export the Resuts  *
putexcel A3=("Income") B3=matrix (Inc) using Quantiles,modify

*  3 Wealth  *

*  Construct a variable stands for wealth according definition in DGR(2011)  *

generate Wealth = networth

*  Get Quantiles of Wealth  *
_pctile Wealth [pweight=wgt], p(1 5 10 20 40 60 80 90 95 99)
matrix WPer=(r(r1), r(r2), r(r3), r(r4), r(r5), r(r6), r(r7), r(r8), r(r9), r(r10))
sum Wealth
matrix W=(r(min), WPer, r(max))

*  Export the Resuts  *
putexcel A4=("Wealth") B4=matrix (W) using Quantiles,modify


**********************
*  Replicate Table 2  *
**********************
*  1 Get Coefficient of Variation and Mean/Median  *

*  Earning  *

sum Earning [aw=wgt],detail
scalar ECofV=r(sd)/r(mean)
scalar EMM=r(mean)/r(p50)

*  Income  *

sum Income [aw=wgt],detail
scalar IncCofV=r(sd)/r(mean)
scalar IncMM=r(mean)/r(p50)

*  Wealth  *

sum Wealth [aw=wgt],detail
scalar WCofV=r(sd)/r(mean)
scalar WMM=r(mean)/r(p50)

*  Export the results  *

putexcel B1=("Earnings") C1=("Income") D1=("Wealth") A2=("Coefficient of Vaiation")A3=("Variance of the logs") A4=("Gini index") A5=("Top 1%/ lowest 40%")A6=("Location of mean(%)") A7=("Mean/median") B2=(ECofV) C2=(IncCofV)D2=(WCofV) B7=(EMM) C7=(IncMM) D7=(WMM) using Concentration, replace

*  2 Variance of the logs  *

*  Earning  *
generate lnEarning=log(Earning)
sum lnEarning [aw=wgt], detail
scalar ElnVar=r(Var)

*  Income  *

gen lnIncome=log(Income)
sum lnIncome [aw=wgt], detail
scalar InclnVar=r(Var)

*  Wealth  *

gen lnWealth=log(Wealth)
sum lnWealth [aw=wgt], detail
scalar WlnVar=r(Var)

*  Export the results  *

putexcel B3=(ElnVar) C3=(InclnVar) D3=(WlnVar) using Concentration,modify

*  3 Gini index  *

*  Earning  *

*fastgini Earning [pw=wgt]
*scalar EGini=r(gini)

*  Income  *

*fastgini Income [pw=wgt]
*scalar IncGini=r(gini)

*  Wealth  *

*fastgini Wealth [pw=wgt]
*scalar WGini=r(gini)

*  Export the results  *

*putexcel B4=(EGini) C4=(IncGini) D4=(WGini) using Concentration,modify

*  4 Location of Mean  *

*  Earning  *

sum Earning [aw=wgt]
scalar Emean=r(mean)
local i=1
local j=0
while `j'<=0{
_pctile Earning [pw=wgt],p(`i')
local j=r(r1)-Emean
if `j'<=0{
local i=`i'+1
}
else{
scalar Elocation=`i'-1
}
}
*	
putexcel B6=(Elocation) using Concentration, modify

sum Income [aw=wgt]
scalar Incmean=r(mean)
local i=1
local j=0
while `j'<=0{
_pctile Income [pw=wgt],p(`i')
local j=r(r1)-Incmean
if `j'<=0{
local i=`i'+1
}
else{
scalar Inclocation=`i'-1
}
}
*	
putexcel C6=(Inclocation) using Concentration, modify

*  Wealth  *
summ Wealth [aw=wgt]
scalar Wmean=r(mean)
local i=1
local j=0
while `j'<=0{
_pctile Wealth [pw=wgt],p(`i')
local j=r(r1)-Wmean
if `j'<=0{
local i=`i'+1
}
else{
scalar Wlocation=`i'-1
}
}
*	
putexcel D6=(Wlocation) using Concentration, modify

*  5 Top 1%/ Lowest 40%  *

*  Earning  *

cumul Earning [aw=wgt] ,gen(Ecul)
sum Earning[aw = wgt] if Ecul >=0.99
generate E1 = r(mean)
sum Earning[aw = wgt] if Ecul <=0.4
generate E40 = r(mean)
scalar ERatio = E1/E40

*  Income  *

cumul Income [aw=wgt] ,gen(Inccul)
sum Income[aw = wgt] if Inccul >=0.99
generate Inc1 = r(mean)
sum Income[aw = wgt] if Inccul <=0.4
generate Inc40 = r(mean)
scalar IncRatio = Inc1/Inc40

*  Wealth  *

cumul Wealth [aw=wgt] ,gen(Wcul)
sum Wealth[aw = wgt] if Wcul >=0.99
generate W1 = r(mean)
sum Wealth[aw = wgt] if Wcul <=0.4
generate W40 = r(mean)
scalar WRatio = W1/W40

*  Export the results  *
putexcel B5=(ERatio) C5=(IncRatio) D5=(WRatio) using Concentration,modify



********************************
*  Part III Darw Lorenz Curve  *
********************************

*  Earning  *
lorenz estimate Earning [pw=wgt], gini
lorenz graph, aspectratio(1) xlabel(, grid)
*  Export the graph  *
graph export EarningGini.pdf

*  Update the Gini Index in the previous table  *
putexcel B4= matrix(e(G)) using Concentration,modify 


* Income  *
lorenz estimate Income [pw=wgt], gini
lorenz graph, aspectratio(1) xlabel(, grid)
*  Export the graph  *
graph export IncomeGini.pdf

*  Update the Gini Index in the previous table  *
putexcel C4=(matrix(e(G))) using Concentration,modify 


*  Wealth  *
lorenz estimate Wealth [pw=wgt], gini
lorenz graph, aspectratio(1) xlabel(, grid)
*  Export the graph  *
graph export WealthGini.pdf
*  Update the Gini Index in the previous table  *
putexcel D4= matrix(e(G)) using Concentration,modify 






