libname nls 'C:\Users\55484\OneDrive - ICF\Documents\ADIA\Data';

*reading in mother interview date and renaming motherid (merge field) to prepare for merge;
data mint1;
set nls.intdate;
rename R0000100=motherid;
run;
*reading in mother education data and renaming motherid (merge field) to prepare for merge;
data med;
set nls.medu;
rename R0000100=motherid;
run;
*sorting monther interview data to prepare for merge of interview and education;
proc sort data=mint1; by motherid; run;
proc sort data=med; by motherid;run;
data mint;
merge mint1 med;
by motherid;
run;
*sorting mother data to prepare to merge with ya data;
proc sort data=mint; by motherid; run;
*reading in YA sample to prepare for merge;
data sample;
	set nls.nls_d;
run;
*checking YA sample to confirm single C0000100 (childIDs) for multiple motherids; 
proc print data=sample (obs=50);
var motherid C0000100;
run;
*sorting YA sample to prepare for merge;
proc sort data=sample; by motherid; run;
*merging mother interview and YA datasets by motherid;
data intsamp;
merge mint(in=f) sample(in=d);
by motherid;
if d;
run;
*sorting new merged dataset by childid (merge field);
proc sort data=intsamp; by C0000100; run;
*one of the 2006 depression scale items was missed in inital data compilation so reading it in;
data depr;
set nls.depress06;
run;
*sorting on childid to prepare for merge;
proc sort data=depr;by C0000100; run;
*merging in missed depression item with full dataset;
data intsamp1;
merge intsamp depr;
by C0000100; 
run;
*reading over intsamp to make all missing valules comprable;
data intsamp1;
set intsamp1;
array nvarlist _numeric_;    /* Consolidating missing values between mother and CYA datasets */
do over nvarlist;
if nvarlist = -1 then nvarlist = .;  /* Refused */
  if nvarlist = -2 then nvarlist = .;  /* Dont know */
  if nvarlist = -3 then nvarlist = .;  /* Invalid missing */
  if nvarlist = -5 then nvarlist = .;  /* Non-interview */
  if nvarlist = -7 then nvarlist = .;  /* Missing */
end; 
run;


*renaming all variables to _year to prepare for breaking up each year into its own dataset;
data sample1;
set intsamp1(rename =(C0000100=childid
Y2946900=physabu_2012
Y3315500=physabu_2014
Y3659300=physabu_2016
Y4267700=physabu_2018
Y2946700=mentill_2012
Y3315300=mentill_2014
Y3659100=mentill_2016
Y4267500=mentill_2018
Y1901900=incarce_2006
Y2217500=incarce_2008
Y2560400=incarce_2010
Y2907900=incarce_2012
Y3274500=incarce_2014
Y3614600=incarce_2016
Y4222900=incarce_2018
Y2946800=subsuse_2012
Y3315400=subsuse_2014
Y3659200=subsuse_2016
Y4267600=subsuse_2018
R0217501=divorce_1979
R0405601=divorce_1980
R0618601=divorce_1981
R0898401=divorce_1982
R1144901=divorce_1983
R1520101=divorce_1984
R1890801=divorce_1985
R2257901=divorce_1986
R2445301=divorce_1987
R2871000=divorce_1988
R3074700=divorce_1989
R3401400=divorce_1990
R3656800=divorce_1991
R4007300=divorce_1992
R4418400=divorce_1993
R5081400=divorce_1994
R5166700=divorce_1996
R6479300=divorce_1998
R7007000=divorce_2000
R7704300=divorce_2002
R8496700=divorce_2004
T0988500=divorce_2006
T2210500=divorce_2008
T3108400=divorce_2010
T4112900=divorce_2012
T5023300=divorce_2014
T5771200=divorce_2016
T8219300=divorce_2018
Y2947000=loveaff_2012
Y3315600=loveaff_2014
Y3659400=loveaff_2016
Y4267800=loveaff_2018
Y2939000=dcourte_2012
Y3309400=dcourte_2014
Y3653100=dcourte_2016
Y4260600=dcourte_2018
Y2939100=drespec_2012
Y3309500=drespec_2014
Y3653200=drespec_2016
Y4260700=drespec_2018
Y2939200=dservic_2012
Y3309600=dservic_2014
Y3653300=dservic_2016
Y4260800=dservic_2018
Y2939300=dnsmart_2012
Y3309700=dnsmart_2014
Y3653400=dnsmart_2016
Y4260900=dnsmart_2018
Y2939400=dafraid_2012
Y3309800=dafraid_2014
Y3653500=dafraid_2016
Y4261000=dafraid_2018
Y2939500=ddishon_2012
Y3309900=ddishon_2014
Y3653600=ddishon_2016
Y4261100=ddishon_2018
Y2939600=dbetter_2012
Y3310000=dbetter_2014
Y3653700=dbetter_2016
Y4261200=dbetter_2018
Y2939700=dinsult_2012
Y3310100=dinsult_2014
Y3653800=dinsult_2016
Y4261300=dinsult_2018
Y2939800=dthreat_2012
Y3310200=dthreat_2014
Y3653900=dthreat_2016
Y4261400=dthreat_2018
Y2940000=dreason_2012
Y3310400=dreason_2014
Y3654100=dreason_2016
Y4261600=dreason_2018
Y2947100=foodins_2012
Y3315700=foodins_2014
Y3659500=foodins_2016
Y4267900=foodins_2018
Y0314700=tanfas_1994
Y0617900=tanfas_1996
Y0910300=tanfas_1998
Y1155400=tanfas_2000
Y1390400=tanfas_2002
Y1642100=tanfas_2004
Y1912600=tanfas_2006
Y2227900=tanfas_2008
Y2577700=tanfas_2010
Y2925400=tanfas_2012
Y3295300=tanfas_2014
Y3635600=tanfas_2016
Y4243000=tanfas_2018
R0160900=foodstp_1979
R0317900=foodstp_1980
R0488400=foodstp_1981
R0788600=foodstp_1982
R1030500=foodstp_1983
R1417200=foodstp_1984
R1785000=foodstp_1985
R2148100=foodstp_1986
R2356800=foodstp_1987
R2729000=foodstp_1988
R2977400=foodstp_1989
R3285400=foodstp_1990
R3565000=foodstp_1991
R3903100=foodstp_1992
R4370600=foodstp_1993
R5034900=foodstp_1994
R5705900=foodstp_1996
R6416100=foodstp_1998
R6931000=foodstp_2000
Y0374100=rullaws_1994
Y0667800=rullaws_1996
Y0965200=rullaws_1998
Y1174900=rullaws_2000
Y1414600=rullaws_2002
Y1665800=rullaws_2004
Y1939300=rullaws_2006
Y2255300=rullaws_2008
Y2606900=rullaws_2010
Y2957000=rullaws_2012
Y3325200=rullaws_2014
Y3670300=rullaws_2016
Y4275100=rullaws_2018
Y0374200=crimevi_1994
Y0667900=crimevi_1996
Y0965300=crimevi_1998
Y1175000=crimevi_2000
Y1414700=crimevi_2002
Y1665900=crimevi_2004
Y1939400=crimevi_2006
Y2255400=crimevi_2008
Y2607000=crimevi_2010
Y2957100=crimevi_2012
Y3325201=crimevi_2014
Y3670301=crimevi_2016
Y4275101=crimevi_2018
Y0374300=abandon_1994
Y0668000=abandon_1996
Y0965400=abandon_1998
Y1175100=abandon_2000
Y1414800=abandon_2002
Y1666000=abandon_2004
Y1939500=abandon_2006
Y2255500=abandon_2008
Y2607100=abandon_2010
Y2957200=abandon_2012
Y3325202=abandon_2014
Y3670302=abandon_2016
Y4275102=abandon_2018
Y0374400=policep_1994
Y0668100=policep_1996
Y0965500=policep_1998
Y1175200=policep_2000
Y1414900=policep_2002
Y1666100=policep_2004
Y1939600=policep_2006
Y2255600=policep_2008
Y2607200=policep_2010
Y2957300=policep_2012
Y3325203=policep_2014
Y3670303=policep_2016
Y4275103=policep_2018
Y0374500=pubtran_1994
Y0668200=pubtran_1996
Y0965600=pubtran_1998
Y1175300=pubtran_2000
Y1415000=pubtran_2002
Y1666200=pubtran_2004
Y1939700=pubtran_2006
Y2255700=pubtran_2008
Y2607300=pubtran_2010
Y2957400=pubtran_2012
Y3325204=pubtran_2014
Y3670304=pubtran_2016
Y4275104=pubtran_2018
Y0374600=supervi_1994
Y0668300=supervi_1996
Y0965700=supervi_1998
Y1175400=supervi_2000
Y1415100=supervi_2002
Y1666300=supervi_2004
Y1939800=supervi_2006
Y2255800=supervi_2008
Y2607400=supervi_2010
Y2957500=supervi_2012
Y3325205=supervi_2014
Y3670305=supervi_2016
Y4275105=supervi_2018
Y0374700=dontcar_1994
Y0668400=dontcar_1996
Y0965800=dontcar_1998
Y1175500=dontcar_2000
Y1415200=dontcar_2002
Y1666400=dontcar_2004
Y1939900=dontcar_2006
Y2255900=dontcar_2008
Y2607500=dontcar_2010
Y2957600=dontcar_2012
Y3325206=dontcar_2014
Y3670306=dontcar_2016
Y4275106=dontcar_2018
Y0374800=notjobs_1994
Y0668500=notjobs_1996
Y0965900=notjobs_1998
Y1175600=notjobs_2000
Y1415300=notjobs_2002
Y1666500=notjobs_2004
Y1940000=notjobs_2006
Y2256000=notjobs_2008
Y2607600=notjobs_2010
Y2957700=notjobs_2012
Y3325207=notjobs_2014
Y3670307=notjobs_2016
Y4275107=notjobs_2018
R0406511=memploy_1980
R0645680=memploy_1981
R0896711=memploy_1982
R1145111=memploy_1983
R1520311=memploy_1984
R1891011=memploy_1985
R2258111=memploy_1986
R2445511=memploy_1987
R2871301=memploy_1988
R3075001=memploy_1989
R3401701=memploy_1990
R3657101=memploy_1991
R4007601=memploy_1992
R4418701=memploy_1993
R5081701=memploy_1994
R5167001=memploy_1996
R6479801=memploy_1998
R7007501=memploy_2000
R7704801=memploy_2002
R8497201=memploy_2004
T0989001=memploy_2006
T2210801=memploy_2008
T3108701=memploy_2010
T4113201=memploy_2012
T5023700=memploy_2014
T5771600=memploy_2016
T8219500=memploy_2018
R0217910=mpovsta_1979
R0406100=mpovsta_1980
R0618500=mpovsta_1981
R0898700=mpovsta_1982
R1144600=mpovsta_1983
R1519800=mpovsta_1984
R1890500=mpovsta_1985
R2257600=mpovsta_1986
R2444900=mpovsta_1987
R2870400=mpovsta_1988
R3074100=mpovsta_1989
R3400800=mpovsta_1990
R3656200=mpovsta_1991
R4006700=mpovsta_1992
R4417800=mpovsta_1993
R5080800=mpovsta_1994
R5166100=mpovsta_1996
R6478800=mpovsta_1998
R7006600=mpovsta_2000
R7703900=mpovsta_2002
R8496300=mpovsta_2004
T0987900=mpovsta_2006
T2210100=mpovsta_2008
T3108000=mpovsta_2010
T4112500=mpovsta_2012
T5022800=mpovsta_2014
T5770900=mpovsta_2016
T8218900=mpovsta_2018
T3974100=mmental_2012
T4912400=mmental_2014
T5616100=mmental_2016
T3974200=msubstu_2012
T4912500=msubstu_2014
T5616200=msubstu_2016
T3974300=mphysab_2012
T4912600=mphysab_2014
T5616300=mphysab_2016
T3974400=mloveaf_2012
T4912700=mloveaf_2014
T5616400=mloveaf_2016
Y0009800=raceeth_1994
Y0396800=raceeth_1996
Y0686600=raceeth_1998
Y0992300=raceeth_1_2000
Y0992301=raceeth_2_2000
Y0992302=raceeth_3_2000
Y0992303=raceeth_4_2000
Y0992304=raceeth_5_2000
Y0992305=raceeth_6_2000
Y1232900=raceeth_1_2002
Y1232901=raceeth_2_2002
Y1232902=raceeth_3_2002
Y1232903=raceeth_4_2002
Y1232904=raceeth_5_2002
Y1232905=raceeth_6_2002
Y1462700=raceeth_1_2004
Y1462701=raceeth_2_2004
Y1462702=raceeth_3_2004
Y1462703=raceeth_4_2004
Y1462704=raceeth_5_2004
Y1462705=raceeth_6_2004
Y1709900=raceeth_1_2006
Y1709901=raceeth_2_2006
Y1709902=raceeth_3_2006
Y1709903=raceeth_4_2006
Y1709904=raceeth_5_2006
Y1709905=raceeth_6_2006
Y1995500=raceeth_1_2008
Y1995501=raceeth_2_2008
Y1995502=raceeth_3_2008
Y1995503=raceeth_4_2008
Y1995504=raceeth_5_2008
Y1995505=raceeth_6_2008
Y2314300=raceeth_1_2010
Y2314301=raceeth_2_2010
Y2314302=raceeth_3_2010
Y2314303=raceeth_4_2010
Y2314304=raceeth_5_2010
Y2314305=raceeth_6_2010
Y2314306=raceeth_7_2010
Y2647000=raceeth_1_2012
Y2647001=raceeth_2_2012
Y2647002=raceeth_3_2012
Y2647003=raceeth_4_2012
Y2647004=raceeth_5_2012
Y2647005=raceeth_6_2012
Y2647006=raceeth_7_2012
Y3004200=raceeth_1_2014
Y3004201=raceeth_2_2014
Y3004202=raceeth_3_2014
Y3004203=raceeth_4_2014
Y3004204=raceeth_5_2014
Y3004205=raceeth_6_2014
Y3004206=raceeth_7_2014
Y3373000=raceeth_1_2016
Y3373001=raceeth_2_2016
Y3373002=raceeth_3_2016
Y3373003=raceeth_4_2016
Y3373004=raceeth_5_2016
Y3373005=raceeth_6_2016
Y3373006=raceeth_7_2016
Y4014100=raceeth_1_2018
Y4014101=raceeth_2_2018
Y4014102=raceeth_3_2018
Y4014103=raceeth_4_2018
Y4014104=raceeth_5_2018
Y4014105=raceeth_6_2018
Y4014106=raceeth_7_2018
Y0686500=hisplat_1998
Y0992200=hisplat_2000
Y1232800=hisplat_2002
Y1462600=hisplat_2004
Y1709800=hisplat_2006
Y1995400=hisplat_2008
Y2314200=hisplat_2010
Y2646900=hisplat_2012
Y3004100=hisplat_2014
Y3372900=hisplat_2016
Y4014000=hisplat_2018
C0005500=bimonth_xrnd
C0005700=biryear_xrnd
Y0383802=ageinty_1994
Y0677600=ageinty_1996
Y0974800=ageinty_1998
Y1192400=ageinty_2000
Y1434300=ageinty_2002
Y1672700=ageinty_2004
Y1948500=ageinty_2006
Y2267100=ageinty_2008
Y2616000=ageinty_2010
Y2966400=ageinty_2012
Y3331900=ageinty_2014
Y3675700=ageinty_2016
Y4281700=ageinty_2018
C0005400=ygender_xrnd
Y0384012=urbnrur_1994
Y0679808=urbnrur_1996
Y0975708=urbnrur_1998
Y1204600=urbnrur_2000
Y1447600=urbnrur_2002
Y1676300=urbnrur_2004
Y1972300=urbnrur_2006
Y2267900=urbnrur_2008
Y2616800=urbnrur_2010
Y2967300=urbnrur_2012
Y3332800=urbnrur_2014
Y3676600=urbnrur_2016
Y4282600=urbnrur_2018
Y1211300=lastgrd_xrnd
Y1672900=lastgrd_2004
Y1948700=lastgrd_2006
Y2267300=lastgrd_2008
Y2616200=lastgrd_2010
Y2966600=lastgrd_2012
Y3332100=lastgrd_2014
Y3675900=lastgrd_2016
Y4281900=lastgrd_2018
T9900000=mhighgd_xrnd
R0217900=mhhinco_1979
R0406010=mhhinco_1980
R0618410=mhhinco_1981
R0898600=mhhinco_1982
R1144500=mhhinco_1983
R1519700=mhhinco_1984
R1890400=mhhinco_1985
R2257500=mhhinco_1986
R2444700=mhhinco_1987
R2870200=mhhinco_1988
R3074000=mhhinco_1989
R3400700=mhhinco_1990
R3656100=mhhinco_1991
R4006600=mhhinco_1992
R4417700=mhhinco_1993
R5080700=mhhinco_1994
R5166000=mhhinco_1996
R6478700=mhhinco_1998
R7006500=mhhinco_2000
R7703700=mhhinco_2002
R8496100=mhhinco_2004
T0987800=mhhinco_2006
T2210000=mhhinco_2008
T3107800=mhhinco_2010
T4112300=mhhinco_2012
T5022600=mhhinco_2014
T5770800=mhhinco_2016
T8218700=mhhinco_2018
Y4257000=anxiety_1_2018
Y4257001=anxiety_2_2018
Y4257002=anxiety_3_2018
Y4257003=anxiety_4_2018
Y4257004=anxiety_5_2018
Y4257005=anxiety_6_2018
Y4257006=anxiety_7_2018
Y0336000=depress_1_1994
Y0336100=depress_2_1994
Y0336200=depress_3_1994
Y0336300=depress_4_1994
Y0336400=depress_5_1994
Y0336500=depress_6_1994
Y0336600=depress_7_1994
Y0636400=depress_1_1996
Y0636500=depress_2_1996
Y0636600=depress_3_1996
Y0636700=depress_4_1996
Y0636800=depress_5_1996
Y0636900=depress_6_1996
Y0637000=depress_7_1996
Y0930900=depress_1_1998
Y0931000=depress_2_1998
Y0931100=depress_3_1998
Y0931200=depress_4_1998
Y0931300=depress_5_1998
Y0931400=depress_6_1998
Y0931500=depress_7_1998
Y1161500=depress_1_2000
Y1161600=depress_2_2000
Y1161700=depress_3_2000
Y1161800=depress_4_2000
Y1161900=depress_5_2000
Y1162000=depress_6_2000
Y1162100=depress_7_2000
Y1396600=depress_1_2002
Y1396700=depress_2_2002
Y1396800=depress_3_2002
Y1396900=depress_4_2002
Y1397000=depress_5_2002
Y1397100=depress_6_2002
Y1397200=depress_7_2002
Y1648100=depress_1_2004
Y1648200=depress_2_2004
Y1648300=depress_3_2004
Y1648400=depress_4_2004
Y1648500=depress_5_2004
Y1648600=depress_6_2004
Y1648700=depress_7_2004
Y1919900=depress_1_2006
Y1920000=depress_2_2006
Y1920100=depress_3_2006
Y1920200=depress_4_2006
Y1920300=depress_5_2006
Y1920400=depress_6_2006
Y1920500=depress_7_2006
Y2235100=depress_1_2008
Y2235200=depress_2_2008
Y2235300=depress_3_2008
Y2235400=depress_4_2008
Y2235500=depress_5_2008
Y2235600=depress_6_2008
Y2235700=depress_7_2008
Y2586700=depress_1_2010
Y2586800=depress_2_2010
Y2586900=depress_3_2010
Y2587000=depress_4_2010
Y2587100=depress_5_2010
Y2587200=depress_6_2010
Y2587300=depress_7_2010
Y2587400=depress_8_2010
Y2587500=depress_9_2010
Y2587600=depress_10_2010
Y2587700=depress_11_2010
Y2934700=depress_1_2012
Y2934800=depress_2_2012
Y2934900=depress_3_2012
Y2935000=depress_4_2012
Y2935100=depress_5_2012
Y2935200=depress_6_2012
Y2935300=depress_7_2012
Y2935400=depress_8_2012
Y2935500=depress_9_2012
Y2935600=depress_10_2012
Y2935700=depress_11_2012
Y3304800=depress_1_2014
Y3304900=depress_2_2014
Y3305000=depress_3_2014
Y3305100=depress_4_2014
Y3305200=depress_5_2014
Y3305300=depress_6_2014
Y3305400=depress_7_2014
Y3305500=depress_8_2014
Y3305600=depress_9_2014
Y3305700=depress_10_2014
Y3305800=depress_11_2014
Y3647800=depress_1_2016
Y3647900=depress_2_2016
Y3648000=depress_3_2016
Y3648100=depress_4_2016
Y3648200=depress_5_2016
Y3648300=depress_6_2016
Y3648400=depress_7_2016
Y3648500=depress_8_2016
Y3648600=depress_9_2016
Y3648700=depress_10_2016
Y3648800=depress_11_2016
Y4255300=depress_1_2018
Y4255400=depress_2_2018
Y4255500=depress_3_2018
Y4255600=depress_4_2018
Y4255700=depress_5_2018
Y4255800=depress_6_2018
Y4255900=depress_7_2018
Y4256000=depress_8_2018
Y4256100=depress_9_2018
Y4256200=depress_10_2018
Y4256300=depress_11_2018
Y1901400=evrvicr_2006
Y2217000=evrvicr_2008
Y2559900=evrvicr_2010
Y2907400=evrvicr_2012
Y3274000=evrvicr_2014
Y3614100=evrvicr_2016
Y4222400=evrvicr_2018
Y1901600=agevicr_2006
Y2217200=agevicr_2008
Y2560100=agevicr_2010
Y2907600=agevicr_2012
Y3274200=agevicr_2014
Y3614300=agevicr_2016
Y4222600=agevicr_2018
Y1901700=agerecvic_2006
Y2217300=agerecvic_2008
Y2560200=agerecvic_2010
Y2907700=agerecvic_2012
Y3274300=agerecvic_2014
Y3614400=agerecvic_2016
Y4222700=agerecvic_2018
Y1901800=agelastvic_2006
Y2217400=agelastvic_2008
Y2560300=agelastvic_2010
Y2907800=agelastvic_2012
Y3274400=agelastvic_2014
Y3614500=agelastvic_2016
Y4222800=agelastvic_2018
Y0359200=drinkdy_1994
Y0653300=drinkdy_1996
Y0949400=drinkdy_1998
Y1165600=drinkdy_2000
Y1404700=drinkdy_2002
Y1655900=drinkdy_2004
Y1928700=drinkdy_2006
Y2245600=drinkdy_2008
Y2597700=drinkdy_2010
Y2948000=drinkdy_2012
Y3316600=drinkdy_2014
Y3660400=drinkdy_2016
Y4268800=drinkdy_2018
Y0301800=accinju_1994
Y0605400=accinju_1996
Y0898200=accinju_1998
Y1147900=accinju_2000
Y1382300=accinju_2002
Y1633200=accinju_2004
Y1887300=accinju_2006
Y2201700=accinju_2008
Y2537700=accinju_2010
Y2885800=accinju_2012
Y3255800=accinju_2014
Y3595800=accinju_2016
Y0301700=preshlt_1994
Y0605300=preshlt_1996
Y0898100=preshlt_1998
Y1147800=preshlt_2000
Y1382200=preshlt_2002
Y1632600=preshlt_2004
Y1886700=preshlt_2006
Y2200800=preshlt_2008
Y2535800=preshlt_2010
Y2883900=preshlt_2012
Y3254100=preshlt_2014
Y3594700=preshlt_2016
Y4214800=preshlt_2018
R0172500=mintmon_1979
R0329200=mintmon_1980
R0530700=mintmon_1981
R0809900=mintmon_1982
R1045700=mintmon_1983
R1427500=mintmon_1984
R1794600=mintmon_1985
R2156200=mintmon_1986
R2365700=mintmon_1987
R2742500=mintmon_1988
R2986100=mintmon_1989
R3302500=mintmon_1990
R3573400=mintmon_1991
R3917600=mintmon_1992
R4100200=mintmon_1993
R4100202=mintyer_1993
R4500201=mintmon_1994
R4500202=mintyer_1994
R5200201=mintmon_1996
R5200202=mintyer_1996
R6435301=mintmon_1998
R6435302=mintyer_1998
R6963301=mintmon_2000
R6963302=mintyer_2000
R7656301=mintmon_2002
R7656302=mintyer_2002
R7800501=mintmon_2004
R7800502=mintyer_2004
T0000901=mintmon_2006
T0000902=mintyer_2006
T1200701=mintmon_2008
T1200702=mintyer_2008
T2260601=mintmon_2010
T2260602=mintyer_2010
T3195601=mintmon_2012
T3195602=mintyer_2012
T4181101=mintmon_2014
T4181102=mintyer_2014
T5150001=mintmon_2016
T5150002=mintyer_2016
T7720001=mintmon_2018
T7720002=mintyer_2018));
keep w motherid childid
bimonth_xrnd
biryear_xrnd
mintmon_:
mintyer_:
foodstp_:
divorce_:
mhhinco_:
mpovsta_:
memploy_: 
tanfas_: 
rullaws_: 
crimevi_:												
abandon_: policep_: pubtran_: supervi_: dontcar_: notjobs_: urbnrur_: incarce_: dcourte_:											
drespec_: dservic_: dnsmart_: dafraid_: ddishon_: dbetter_: dinsult_: dthreat_: dreason_:											
preshlt_: 
accinju_: 
depress_: 
drinkdy_: 
anxiety_:
raceeth:
hisplat_:
agevicr_:
evrvicr_:
ageinty_:
mhhinco_:
physabu_:
mentill_:
subsuse_:
loveaff_:
foodins_:
mmental_:
msubstu_:
mphysab_:
mloveaf_:
evrvicr_:
agevicr_:
agerecvic_:
agelastvic_:
ageinty_:
lastgrd_:
ygender_xrnd
lastgrd_xrnd
mhighgd_xrnd
;					
run;
*variables to keep that do not need to be reshaped;
data sample0;
set sample1;
keep w motherid childid 
bimonth_xrnd
biryear_xrnd
ygender_xrnd
lastgrd_xrnd
mhighgd_xrnd
raceeth_1994 raceeth_1996 raceeth_1998
physabu_2012
physabu_2014
physabu_2016
physabu_2018
mentill_2012
mentill_2014
mentill_2016
mentill_2018
subsuse_2012
subsuse_2014
subsuse_2016
subsuse_2018
loveaff_2012
loveaff_2014
loveaff_2016
loveaff_2018
foodins_2012
foodins_2014
foodins_2016
foodins_2018
mmental_2012
mmental_2014
mmental_2016
msubstu_2012
msubstu_2014
msubstu_2016
mphysab_2012
mphysab_2014
mphysab_2016
mloveaf_2012
mloveaf_2014
mloveaf_2016
evrvicr_2006
evrvicr_2008
evrvicr_2010
evrvicr_2012
evrvicr_2014
evrvicr_2016
evrvicr_2018
agevicr_2006
agevicr_2008
agevicr_2010
agevicr_2012
agevicr_2014
agevicr_2016
agevicr_2018
agerecvic_2006
agerecvic_2008
agerecvic_2010
agerecvic_2012
agerecvic_2014
agerecvic_2016
agerecvic_2018
agelastvic_2006
agelastvic_2008
agelastvic_2010
agelastvic_2012
agelastvic_2014
agelastvic_2016
agelastvic_2018
ageinty_1994
ageinty_1996
ageinty_1998
ageinty_2000
ageinty_2002
ageinty_2004
ageinty_2006
ageinty_2008
ageinty_2010
ageinty_2012
ageinty_2014
ageinty_2016
ageinty_2018
;
run;
proc sort data=sample0; by childid; run;
*reshape to long, null part of the sampele that mother interview date beyond child DOB;
%macro wtol(dsn,var);
proc transpose data=sample1 out=&dsn(drop=_label_) prefix=&var;
by childid;
var &var._:;
run;
Data &dsn;
set &dsn;
year=substrn(_name_,length(_name_)-3,4);
drop _name_;
run;
proc sort data=&dsn(rename=(&var.1=&var)); by childid year; run;
%mend;
%wtol(mintm, mintmon);
%wtol(minty, mintyer);
%wtol(foodstp, foodstp);
%wtol(divorce, divorce);
%wtol(mhhinco, mhhinco);
%wtol(mpovsta, mpovsta);
%wtol(memploy, memploy); 
%wtol(tanfas,tanfas);
%wtol(rullaws,rullaws);
%wtol(crimevi,crimevi);												
%wtol(abandon,abandon);
%wtol(policep,policep);
%wtol(pubtran,pubtran);
%wtol(supervi,supervi);
%wtol(dontcar,dontcar);
%wtol(notjobs,notjobs);
%wtol(urbnrur,urbnrur);
%wtol(incarce,incarce);
%wtol(dcourte,dcourte);									
%wtol(drespec,drespec);
%wtol(dservic,dservic);
%wtol(dnsmart,dnsmart);
%wtol(dafraid,dafraid);
%wtol(ddishon,ddishon);
%wtol(dbetter,dbetter);
%wtol(dinsult,dinsult);
%wtol(dthreat,dthreat);
%wtol(dreason,dreason);										
%wtol(preshlt,preshlt);
%wtol(accinju,accinju);
%wtol(drinkdy,drinkdy);
%wtol(depress_1,depress_1);%wtol(depress_2,depress_2);%wtol(depress_3,depress_3);%wtol(depress_4,depress_4);%wtol(depress_5,depress_5);%wtol(depress_6,depress_6);%wtol(depress_7,depress_7);%wtol(depress_8,depress_8);%wtol(depress_9,depress_9);%wtol(depress_10,depress_10);%wtol(depress_11,depress_11);
%wtol(anxiety_1,anxiety_1);%wtol(anxiety_2,anxiety_2);%wtol(anxiety_3,anxiety_3);%wtol(anxiety_4,anxiety_4);%wtol(anxiety_5,anxiety_5);%wtol(anxiety_6,anxiety_6);%wtol(anxiety_7,anxiety_7);
%wtol(raceeth_1,raceeth_1);%wtol(raceeth_2,raceeth_2);%wtol(raceeth_3,raceeth_3);%wtol(raceeth_4,raceeth_4);%wtol(raceeth_5,raceeth_5);%wtol(raceeth_6,raceeth_6);
%wtol(hisplat,hisplat); 
%wtol(agevicr,agevicr);
%wtol(evrvicr,evrvicr);
%wtol(ageinty,ageinty);
%wtol(lastgrd,lastgrd);
%wtol(mhhinco,mhhinco);



data sample_l;
merge mintm
minty
foodstp
divorce
mhhinco
mpovsta
memploy
tanfas
rullaws
crimevi
abandon
policep
pubtran
supervi
dontcar
notjobs
incarce
dcourte
drespec
dservic
dnsmart
dafraid
ddishon
dbetter
dinsult
dthreat
dreason
preshlt
accinju
depress_1 depress_2 depress_3 depress_4 depress_5 depress_6 depress_7 depress_8 depress_9 depress_10 depress_11
drinkdy
anxiety_1 anxiety_2 anxiety_3 anxiety_4 anxiety_5 anxiety_6 anxiety_7
raceeth_1 raceeth_2 raceeth_3 raceeth_4 raceeth_5 raceeth_6
hisplat
urbnrur
agevicr
evrvicr
ageinty
lastgrd
mhhinco;
by childid year;
run;
*ye to check age done correctly here;
Data sample_l1;
merge sample0 sample_l;
by childid;
idate = mdy (mintmon, 1, mintyer);
dob= mdy (bimonth_xrnd, 1, biryear_xrnd);
format idate dob date9.;
if  mintmon>=0 & mintyer>=0 then do;
intage=INT(YRDIF(dob, idate, "AGE"));
*if index(intage, '-') then delete; /* Deleting where child not born yet */
if intage<0 then delete;
end;
label intage="difference in year between mother interview date and DOB";
run;
proc freq data=sample_l1;
table intage;
run;
Data sample_l2;
set sample_l1;
if intage>=0; /*12-07-2022 add restriction, sample need to have child age after mother interview date*/
if ageinty ne . then intage=ageinty; /* where we have child self-reported age (ageinty)*/
array var1 {26} foodstp divorce mhhinco mpovsta memploy tanfas rullaws crimevi
	abandon policep pubtran supervi dontcar notjobs urbnrur incarce dcourte
	drespec dservic dnsmart dafraid ddishon dbetter dinsult dthreat dreason;
	do i=1 to 26;
  if intage => 18 then var1(i) = .E;  /* Child over 18 */
  end;
array var2 {22} preshlt accinju depress_1 depress_2 depress_3 depress_4 depress_5
	depress_6 depress_7 depress_8 depress_9 depress_10 depress_11 drinkdy anxiety_1 
	anxiety_2 anxiety_3 anxiety_4 anxiety_5 anxiety_6 anxiety_7 lastgrd_;
		do i=1 to 22;
if intage < 18 then var2(i) = .O;  /* Child under 18 */
  end;
drop i;
run;
proc print data=sample_l1 (obs=25);
where ageinty ne .;
var ageinty intage;
run;
proc print data=sample_l2 (obs=25);
where ageinty ne .;
var ageinty intage;
run;
proc print data=sample_l1 (obs=50);
var ageinty intage;
run;
proc print data=sample_l2 (obs=50);
var ageinty intage;
run;
proc freq data=sample_l2;
tables intage ageinty;
run;
proc freq data=sample_l1;
tables intage ageinty;
run;
*reshape to flat file;
proc transpose data=sample_l2 out=temp;
  by childid year ;
  var /*mintmon
mintyer*/
foodstp
divorce
mpovsta
memploy
tanfas
rullaws
crimevi
abandon
policep
pubtran
supervi
dontcar
notjobs
incarce
dcourte
drespec
dservic
dnsmart
dafraid
ddishon
dbetter
dinsult
dthreat
dreason
preshlt
accinju
depress_1 depress_2 depress_3 depress_4 depress_5 depress_6 depress_7 depress_8 depress_9 depress_10 depress_11
drinkdy
anxiety_1 anxiety_2 anxiety_3 anxiety_4 anxiety_5 anxiety_6 anxiety_7
agevicr
lastgrd
evrvicr
ageinty
urbnrur
mhhinco
hisplat
raceeth_1 raceeth_2 raceeth_3 raceeth_4 raceeth_5 raceeth_6
idate intage;
run;

proc transpose data=temp out=sample_f1(drop=_name_) delim=_;
  by childid ;
  id _name_  year ;
  var col1;
run;
*flat file;
Data sample_f;
merge sample_f1 sample0;
by childid;
run;

Data NLS.sample_f;
set sample_f;
run;
*long file;
Data sample_long;
merge sample_l2 sample0;
by childid;
run;
Data NLS.sample_long;
set sample_long;
run;
/*
data nls.hhincome;
set NLS.sample_f;
keep childid mhhinco_1979 mhhinco_1980 mhhinco_1981 mhhinco_1982 mhhinco_1983
mhhinco_1984 mhhinco_1985 mhhinco_1986 mhhinco_1987 mhhinco_1988 mhhinco_1989
mhhinco_1990 mhhinco_1991 mhhinco_1992 mhhinco_1993 mhhinco_1994 mhhinco_1996
mhhinco_1998 mhhinco_2000 mhhinco_2002 mhhinco_2004 mhhinco_2006 mhhinco_2008
mhhinco_2010 mhhinco_2012 mhhinco_2014 mhhinco_2016 mhhinco_2018;
run;

proc export data=nls.hhincome
    outfile='C:/Users/55484/OneDrive - ICF/Documents/ADIA/Data/hhincome_01.08.2023.csv'
    dbms=csv
	replace;
	putnames=yes;
run;*/
proc contents data=nls.sample_f;
run;
proc freq data=nls.sample_f;
tables raceeth_:;
run; 

%_eg_conditional_dropds(WORK.QUERY_FOR_SAMPLE_F);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SAMPLE_F AS 
   SELECT t1.intage_1992, 
          t1.intage_1993, 
          t1.intage_1994, 
          t1.ageinty_1994, 
          t1.ageinty_1996, 
          t1.intage_1996, 
          t1.intage_1998, 
          t1.ageinty_1998, 
          t1.ageinty_2000, 
          t1.intage_2000, 
          t1.agevicr_2002, 
          t1.intage_2002, 
          t1.ageinty_2004, 
          t1.intage_2004, 
          t1.ageinty_2006, 
          t1.intage_2006, 
          t1.ageinty_2008, 
          t1.intage_2008, 
          t1.ageinty_2010, 
          t1.intage_2010, 
          t1.ageinty_2012, 
          t1.intage_2012, 
          t1.ageinty_2014, 
          t1.intage_2014, 
          t1.ageinty_2016, 
          t1.intage_2016, 
          t1.ageinty_2018, 
          t1.intage_2018
      FROM NLS.SAMPLE_F t1;
QUIT;

*some check;
/*
Proc means data=sample_f;
var _numeric_;
run;
proc freq data=sample_f;
table foodstp_1979
divorce_1979 intage_1979;
run;

proc import datafile="D:\nlscya_all_1979-2018\NLS_detangled_11.19.22.xlsx" 
out=NLS dbms=xlsx;
run;

proc means data=NLS;
var _numeric_;
run;
proc freq data=NLS;
table foodstp_1979
divorce_1979;
run;

proc freq data=nls.nls_d;
table R0160900
R0217501;
run;
*/
