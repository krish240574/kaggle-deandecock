rgd:{[w;f;op;tl;s;counter]
        d:(2*(flip f)$((f$w)-op))+(2*l2_p*w); / L2 - ridge regression derivative
        gm:sqrt sum (d*d);
        $[(counter<niter);rgd[w-(s*d);f;op;tl;s;counter+1]; w]} ;

c:`Id`MSSubClass`MSZoning`LotFrontage`LotArea`Street`Alley`LotShape`LandContour`Utilities`LotConfig`LandSlope`Neighborhood`Condition1`Condition2`BldgType`HouseStyle`OverallQual`OverallCond`YearBuilt`YearRemodAdd`RoofStyle`RoofMatl`Exterior1st`Exterior2nd`MasVnrType`MasVnrArea`ExterQual`ExterCond`Foundation`BsmtQual`BsmtCond`BsmtExposure`BsmtFinType1`BsmtFinSF1`BsmtFinType2`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`Heating`HeatingQC`CentralAir`Electrical`1stFlrSF`2ndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`KitchenQual`TotRmsAbvGrd`Functional`Fireplaces`FireplaceQu`GarageType`GarageYrBlt`GarageFinish`GarageCars`GarageArea`GarageQual`GarageCond`PavedDrive`WoodDeckSF`OpenPorchSF`EnclosedPorch`3SsnPorch`ScreenPorch`PoolArea`PoolQC`Fence`MiscFeature`MiscVal`MoSold`YrSold`SaleType`SaleCondition`SalePrice
colStr:(count c)#"S";
.Q.fs[{`train insert flip c!(colStr;",")0:x}]`:ktrain.csv
train:train[1+til(-1+count train)]
train:delete Id from train
/ change 1stFlrSF and 2ndFlrSF to q-type variables
t:key ft:flip train
t[where t=`1stFlrSF]:`FstFlrSF
t[where t=`2ndFlrSF]:`SndFlrSF
t[where t=`3SsnPorch]:`ThreeSnPorch
train:flip t!value ft
/ garbage collect
.Q.gc[]
/ Non-categorical columns, not to be passed for one-hot encoding
remCols:`YearBuilt`YearRemodAdd`LotArea`MasVnrArea`BsmtFinSF1`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`FstFlrSF`SndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`TotRmsAbvGrd`Fireplaces`GarageYrBlt`GarageCars`GarageArea`WoodDeckSF`OpenPorchSF`EnclosedPorch`ThreeSnPorch`ScreenPorch`PoolArea`MiscVal`MoSold`YrSold`SalePrice

tmp:cols train
{tmp::tmp[where tmp <> x]}each remCols

/ Find all categorical columns with NAs, remove NAs and create a dict with distinct values in each column
e:(tmp)[wk] ! {kna[x;tk[x]]} each til count tk:where each (`NA <>/: kna:k[wk:where  (`NA in/: k:distinct each train[tmp])])
k[wk]:e@(tmp)[wk]
k:(tmp) ! k
i::0
/ Create the one-hot encoded array and append to original dataset
{s::((count train),(count r:where each (value k)[x] =\: train[key k][x]))#0;s[r[x];x]:1;train::train,'(({`$((string (key k)[i]),string x)}each (value k)[i])!)each s;i::i+1}each til count k
/ Delete original non-one-hot categorical columns
train:![train;();0b;tmp]
/ Re-append non-categorical columns
train:train,'flip remCols ! train[remCols]
////////// Not needed
{train[x]:"I"$string train[x]}each remCols
.Q.gc[]
train:([]intercept:(count train)#1.0),'train
cls:cols train
/f:flip "f"$train[cls[where cls<>`SalePrice]]
/f:flip "f"$train[cls[where (cls<>`SalePrice) and (cls <>`YearBuilt) and (cls <> `YearRemodAdd) and (cls <> `GarageYrBuilt) and (cls <> `YrSold)]]
f:"f"$train[cls[where (cls<>`SalePrice)]];
f:{0^f[;x]}each til count f[0];
w:"f"$((count f[0]),1)#-1000.0,(-1+count f[0])#1.0
op:"f"$train[`SalePrice];
tl:"f"$1.0e+009;
s:"f"$1.0e-12;
l2_p:0.0;
counter:0;
niter:1000;
show "Calling rgd";
kw:rgd[w;f;op;tl;s;counter];
show kw;
