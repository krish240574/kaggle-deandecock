c:`Id`MSSubClass`MSZoning`LotFrontage`LotArea`Street`Alley`LotShape`LandContour`Utilities`LotConfig`LandSlope`Neighborhood`Condition1`Condition2`BldgType`HouseStyle`OverallQual`OverallCond`YearBuilt`YearRemodAdd`RoofStyle`RoofMatl`Exterior1st`Exterior2nd`MasVnrType`MasVnrArea`ExterQual`ExterCond`Foundation`BsmtQual`BsmtCond`BsmtExposure`BsmtFinType1`BsmtFinSF1`BsmtFinType2`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`Heating`HeatingQC`CentralAir`Electrical`1stFlrSF`2ndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`KitchenQual`TotRmsAbvGrd`Functional`Fireplaces`FireplaceQu`GarageType`GarageYrBlt`GarageFinish`GarageCars`GarageArea`GarageQual`GarageCond`PavedDrive`WoodDeckSF`OpenPorchSF`EnclosedPorch`3SsnPorch`ScreenPorch`PoolArea`PoolQC`Fence`MiscFeature`MiscVal`MoSold`YrSold`SaleType`SaleCondition`SalePrice
colStr:(count c)#"S";
.Q.fs[{`train insert flip c!(colStr;",")0:x}]`:ktrain.csv
train:train[1+til(-1+count train)]
train:delete Id from train
/////////
///////// TODO - Need to remove the int column - LotArea, to keep as is. 
////////
/ Find all columns with NAs, remove NAs and create a dict with distinct values
/ in each column
e:(cols train)[wk] ! {kna[x;tk[x]]} each til count tk:where each (`NA <>/: kna:k[wk:where  (`NA in/: k:distinct each train[cols train])])
/ e contains all columns with NAs removed - distinct values
/ overwrite values in k, the master dict of all column distincts
k[wk]:e@(cols train)[wk]
k:(cols train) ! k
i::0
/ Create the one-hot encoded array and append to original dataset
{s::((count train),(count r:where each (value k)[x] =\: train[key k][x]))#0;s[r[x];x]:1;train::train,'(({`$((string (key k)[i]),string x)}each (value k)[i])!)each s;i::i+1}each til count k
/ delete original rows from dataset
train:![train;();0b;tmp]
