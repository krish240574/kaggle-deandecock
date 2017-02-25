/ Lasso regression, using co-ordinate descent. 
lasso:{[i]
      / Cycle through each weight
      / calculate ro, for each weight
      / update weight by lambda rule
      oldwt:sum w[i];      
      pred:0^raze f$w;
      temp:(((count f),1)#f[;i])$(1,1)#w[i];
      roi:sum over(f[;i]*(op-pred+temp));
      nwi:0.0;
      if[i=0;nwi:roi];
      if[i>0;if[roi<(-1*l1p)%2.0;nwi:roi+(l1p%2.0)]; if[roi>(l1p%2.0);nwi:roi-(l1p%2.0)];if[(roi>=(-1*l1p)%2.0) and (roi<=(l1p%2.0));nwi:0.0]] ;
      w[i;]::nwi;
      if[(sum oldwt-w[i])<tolerance;ctr::ctr+1];
      if[i<(-1+count w);lasso[i+1]];
      };
ctr:0;
lassodriver:{show "lasso";iter:0;while[(ctr<count w) and (iter<10);ctr::0;lasso[0];show raze w;iter:iter+1]};

/ Ridge, L2 regression
rgd:{[f;op;tl;s;counter]niter {w::w-s*((2*(flip f)$((f$w)-op))+(2*l2_p*w))}\counter;};

pd:{[tf]
        ds::delete Id from ds;
        / change 1stFlrSF and 2ndFlrSF to q-type variables
        t:key ft:flip ds;
        t[where t=`1stFlrSF]:`FstFlrSF;
        t[where t=`2ndFlrSF]:`SndFlrSF;
        t[where t=`3SsnPorch]:`ThreeSnPorch;
        ds::flip t!value ft;

        / Non-categorical columns, not to be passed for one-hot encoding
        remCols:`YearBuilt`YearRemodAdd`LotArea`MasVnrArea`BsmtFinSF1`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`FstFlrSF`SndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`TotRmsAbvGrd`Fireplaces`GarageYrBlt`GarageCars`GarageArea`WoodDeckSF`OpenPorchSF`EnclosedPorch`ThreeSnPorch`ScreenPorch`PoolArea`MiscVal`MoSold`YrSold`SalePrice;
        if[tf like "test";remCols:remCols[where remCols<>`SalePrice]];
        / Remove all non-cat columns from list, so we're sending only
        / cats to one-hot
        tmp:cols ds;
        tmp:tmp[where not  tmp in\: remCols];

        / Find all categorical columns with NAs, remove NAs and create a dict with distinct values in each column
        kna:k[wk:where  (`NA in/: k:distinct each ds[tmp])];
        t:0,(+\)t[til(-1+count t:count each where each `NA <> kna)];
        rkna:raze kna;
        e:(tmp)[wk] ! t cut rkna[where rkna <> `NA];
        k[wk]:e@(tmp)[wk];
        k:(tmp) ! k;

        i:0;
        while[i<count k;
          ds::ds,'((`$( string (key k)[i]) ,/: string (value k)[i])!)each s:((count ds),(count r:where each (value k)[i] =\: ds[key k][i]))#0;
          s[r[i];i]:1;
          i:i+1;
        ]; / end while loop
        / Delete original non-one-hot categorical columns
        ds::![ds;();0b;tmp];

        / Re-append non-categorical columns
        ds::ds,'flip remCols ! ds[remCols];
        $[tf like "train";train::ds;test::ds]}; / end function

/ -----------------------------------read train data set from disk
c:`Id`MSSubClass`MSZoning`LotFrontage`LotArea`Street`Alley`LotShape`LandContour`Utilities`LotConfig`LandSlope`Neighborhood`Condition1`Condition2`BldgType`HouseStyle`OverallQual`OverallCond`YearBuilt`YearRemodAdd`RoofStyle`RoofMatl`Exterior1st`Exterior2nd`MasVnrType`MasVnrArea`ExterQual`ExterCond`Foundation`BsmtQual`BsmtCond`BsmtExposure`BsmtFinType1`BsmtFinSF1`BsmtFinType2`BsmtFinSF2`BsmtUnfSF`TotalBsmtSF`Heating`HeatingQC`CentralAir`Electrical`1stFlrSF`2ndFlrSF`LowQualFinSF`GrLivArea`BsmtFullBath`BsmtHalfBath`FullBath`HalfBath`BedroomAbvGr`KitchenAbvGr`KitchenQual`TotRmsAbvGrd`Functional`Fireplaces`FireplaceQu`GarageType`GarageYrBlt`GarageFinish`GarageCars`GarageArea`GarageQual`GarageCond`PavedDrive`WoodDeckSF`OpenPorchSF`EnclosedPorch`3SsnPorch`ScreenPorch`PoolArea`PoolQC`Fence`MiscFeature`MiscVal`MoSold`YrSold`SaleType`SaleCondition`SalePrice;
colStr:"SSSSISSSSSSSSSSSSSSIISSSSSISSSSSSSISIIISSSSIIIIIIIIIISISISSISIISSSIIIIIISSSIIISSI";
.Q.fs[{`train insert flip c!(colStr;",")0:x}]`:train.csv;
tf:"train";
ds:train[1+til (-1+count train)]; / skip header row
pd[tf];
/ ------------------------------------------------------
/ Start training regression model
train:([]intercept:(count train)#1.0),'train
cls:cols train;
f:flip 0^"f"$train[cls[where (cls<>`SalePrice)]];

/w:"f"$((count f[0]),1)#(count f[0])?100.0;
/w:"f"$((count f[0]),1)#0.0;

w:"f"$((count f[0]),1)#(10000),(-1+count f[0])#0.0;

op:0^"f"$train[`SalePrice];

trainmodel:{[flag]
  if[flag=2;tl:"f"$1.0e+009;s::"f"$1.0e-12;l2_p::0.0;counter:0;niter::100;show "Calling rgd - L2";rgd[f;op;tl;s;counter]];
  if[flag=1;w::"f"$((count f[0]),1)#(-1000000000.0%5),(-1+count f[0])#0.0;f::f%\:sqrt sum f*f;l1p::1e+7;tolerance::1.0;lassodriver[]];
  };
 

/ ---------------Process test data -----------------------------------------
tf:"test";
c:c[where c<>`SalePrice];
colStr:"SSSSISSSSSSSSSSSSSSIISSSSSISSSSSSSISIIISSSSIIIIIIIIIISISISSISIISSSIIIIIISSSIIISS";
.Q.fs[{`test insert flip c!(colStr;",")0:x}]`:test.csv;
ds:test[1+til(-1+count test)]; / skip hearder row
testId:test[1+til(-1+count test)][`Id];
pd[tf];

/ ---------------Run regression model with trained weights on test data
finaloutput:{[flag]
  test::([]intercept:(count test)#1.0),'test;
  l:cols train;k:raze where each l =/: cols test;
  cls:l[k];
  wts:w[k];
  h:((count cls),1)# raze over (cls!wts)@cls;
  f:flip 0f^/:"f"$test[cls];
  / Normalize values for lasso
  if[flag=1;f:0f^f%\:sum sqrt f*f];
  o:f$h;
  show "Outputs :";
  show op:([]Id:testId;SalePrice:o);
  };

/ Parameter [1] - L1 - lasso
/ Parameter [2] - L2 - ridge
trainmodel[2]; 
finaloutput[2];
