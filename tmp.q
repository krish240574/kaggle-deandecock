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
        rkna:rkna[where rkna <> `NA];
        e:(tmp)[wk] ! t cut rkna;
        k[wk]:e@(tmp)[wk];
        k:(tmp) ! k;
        i:0;
/        Create the one-hot encoded array and append to original dataset
        {s::((count ds),(count r:where each (value k)[x] =\: ds[key k][x]))#0;s[r[x];x]:1;ds::ds,'(({`$((string (key k)[i]),string x)}each (value k)[i])!)each s;i::i+1}each til count k;
/        Delete original non-one-hot categorical columns
        ds::![ds;();0b;tmp];
        / Re-append non-categorical columns
        ds::ds,'flip remCols ! ds[remCols];
        ////////// Not needed
        {ds[a]::"I"$string ds[a]}each remCols ;
        $[tf like "train";train::ds;test::ds]};
        
c:`Id`MSSubClass`MSZoning`LotFrontage`LotArea`Street`Alley`LotShape`LandContour`Utilities`LotConfig`LandSlope`Neighborhood`Condition1`Condition2`BldgType`HouseStyle`OverallQual`OverallCond`YearBuilt`YearRemodAdd`RoofStyle`RoofMatl`Exterior1st`Exterior2nd`MasVnrType`MasVnrArea`ExterQual`ExterCond`Foundation`BsmtQual`BsmtCond`BsmtExposure`BsmtFinType1`BsmtFinSF1`BsmtFinType2`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`Heating`HeatingQC`CentralAir`Electrical`1stFlrSF`2ndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`KitchenQual`TotRmsAbvGrd`Functional`Fireplaces`FireplaceQu`GarageType`GarageYrBlt`GarageFinish`GarageCars`GarageArea`GarageQual`GarageCond`PavedDrive`WoodDeckSF`OpenPorchSF`EnclosedPorch`3SsnPorch`ScreenPorch`PoolArea`PoolQC`Fence`MiscFeature`MiscVal`MoSold`YrSold`SaleType`SaleCondition`SalePrice;
colStr:(count c)#"S";
.Q.fs[{`train insert flip c!(colStr;",")0:x}]`:ktrain.csv;
tf:"train";
ds:train;
pd[tf];
if[kds=`test;c:c[where c<>`SalePrice];ds::train];

