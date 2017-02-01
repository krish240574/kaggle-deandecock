rgd:{[w;f;op;tl;s;counter]
   d:(2*(flip f)$((f$w)-op))+(2*l2_p*w); / L2 - ridge regression derivative
   gm:sqrt sum (d*d);
   $[(counter<niter);rgd[w-(s*d);f;op;tl;s;counter+1]; w]} ;
/ Process data - for both train and test
pd:{[tf]
        ds::ds[1+til(-1+count ds)];
        ds::delete Id from ds;
        / change 1stFlrSF and 2ndFlrSF to q-type variables
        t:key ft:flip ds;
        t[where t=`1stFlrSF]:`FstFlrSF;
        t[where t=`2ndFlrSF]:`SndFlrSF;
        t[where t=`3SsnPorch]:`ThreeSnPorch;
        ds::flip t!value ft;
        / garbage collect
        .Q.gc[];
        / Non-categorical columns, not to be passed for one-hot encoding
        remCols:`YearBuilt`YearRemodAdd`LotArea`MasVnrArea`BsmtFinSF1`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`FstFlrSF`SndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`TotRmsAbvGrd`Fireplaces`GarageYrBlt`GarageCars`GarageArea`WoodDeckSF`OpenPorchSF`EnclosedPorch`ThreeSnPorch`ScreenPorch`PoolArea`MiscVal`MoSold`YrSold`SalePrice;
        if[tf like "test";remCols:remCols[where remCols<>`SalePrice]];
/ Remove all non-cat columns from list, so we're sending only
/ non-cats to one-hot
        tmp:cols ds;
        tmp:tmp[ where not  tmp in\: remCols];
/       {tmp::tmp[where tmp <> x]}each remCols;

        / Find all categorical columns with NAs, remove NAs and create a dict with distinct values in each column
        kna:k[wk:where  (`NA in/: k:distinct each ds[tmp])];
        t:0,(+\)t[til(-1+count t:count each where each `NA <> kna)];
        rkna:raze kna;
        e:(tmp)[wk] ! t cut rkna:rkna[where rkna <> `NA];
        k[wk]:e@(tmp)[wk];
        k:(tmp) ! k;
        i:0;
        while[i<count k;
                ds::ds,'((`$( string (key k)[i]) ,/: string (value k)[i])!)each s:((count ds),(count r:where each (value k)[i] =\: ds[key k][i]))#0;s[r[i];i]:1;i:i+1;
        ]; / end stinking loop
///// commented
/        Create the one-hot encoded array and append to original dataset
/       {s::((count ds),(count r:where each (value k)[x] =\: ds[key k][x]))#0;s[r[x];x]:1;ds::ds,'(({`$((string (key k)[i]),string x)}each (value k)[i])!)each s;i::i+1}each til count k;
///// commented
/        Delete original non-one-hot categorical columns
        ds::![ds;();0b;tmp];
        / Re-append non-categorical columns
        ds::ds,'flip remCols ! ds[remCols];
        ////////// Not needed
        {ds[x]::"I"$string ds[x]}each remCols ;
        $[tf like "train";train::ds;test::ds]}; / end function
/ -----------------------------------train data set
c:`Id`MSSubClass`MSZoning`LotFrontage`LotArea`Street`Alley`LotShape`LandContour`Utilities`LotConfig`LandSlope`Neighborhood`Condition1`Condition2`BldgType`HouseStyle`OverallQual`OverallCond`YearBuilt`YearRemodAdd`RoofStyle`RoofMatl`Exterior1st`Exterior2nd`MasVnrType`MasVnrArea`ExterQual`ExterCond`Foundation`BsmtQual`BsmtCond`BsmtExposure`BsmtFinType1`BsmtFinSF1`BsmtFinType2`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`Heating`HeatingQC`CentralAir`Electrical`1stFlrSF`2ndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`KitchenQual`TotRmsAbvGrd`Functional`Fireplaces`FireplaceQu`GarageType`GarageYrBlt`GarageFinish`GarageCars`GarageArea`GarageQual`GarageCond`PavedDrive`WoodDeckSF`OpenPorchSF`EnclosedPorch`3SsnPorch`ScreenPorch`PoolArea`PoolQC`Fence`MiscFeature`MiscVal`MoSold`YrSold`SaleType`SaleCondition`SalePrice;
colStr:(count c)#"S";
.Q.fs[{`train insert flip c!(colStr;",")0:x}]`:ktrain.csv;
tf:"train";
ds:train;
pd[tf];
/ ------------------------------------------------------
.Q.gc[]
/ Start training regression model
train:([]intercept:(count train)#1.0),'train
cls:cols train;
f:"f"$train[cls[where (cls<>`SalePrice)]];
f:{0^f[;x]}each til count f[0];
w:"f"$((count f[0]),1)#-1000.0,(-1+count f[0])#1.0
op:"f"$train[`SalePrice];
tl:"f"$1.0e+009;
s:"f"$1.0e-12;
l2_p:0.0;
counter:0;
niter:100;
show "Calling rgd";
kw:rgd[w;f;op;tl;s;counter];

/ ---------------Process test data -----------------------------------------
tf:"test";
c:c[where c<>`SalePrice];
colStr:(count c)#"S";
.Q.fs[{`test insert flip c!(colStr;",")0:x}]`:test.csv;
ds:test;
testId:test[`Id];
testId:testId[1+til (-1+count testId)];
pd[tf];

/ ---------------Run regression model with trained weights on test data
test:([]intercept:(count test)#1.0),'test
cls:cols test;
l:cols train;
wts:kw[{where x=l}each cls]
h:((count cls),1)# raze over (cls!wts)@cls
f:"f"$test[cls]
f:{0^f[;x]}each til count f[0];
o:f$h;
show "Outputs :";
show op:([]Id:testId;SalePrice:o);
