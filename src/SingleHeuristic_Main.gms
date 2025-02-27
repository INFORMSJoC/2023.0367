*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*                                             Flow Hub Location Problems
*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

$set ORGSET %ORGSET%
$set DSTSET %DSTSET%
$set PRODSET %PRODSET%
$set HUBSET %HUBSET%
$set CACapR %CACapR%
$set CPCapR %CPCapR%
$set clusternum k15
$set maxtime %maxtime%
$set uncapf %uncapf%
$set execseed %execseed%
$set ID %ID%
$set probthreshold 0.5

$set orgdstnum %orgdstnum%
$set EOC %EOC%
$set dmd %dmd%
$set asscost %asscost%
$set hubsetcost %hubsetcost%
$set transcost %transcost%
Option MIP = cplex;

****************************************** AP network data *****************************************
set TNhubs                 "AP network hub location set"      /1*200/;
alias (TNhubs, TNhubs1, TNhubs2);
set aaa /1*2/;
*=== Import from Excel using GDX utilities
parameter HubCordinate(TNhubs, aaa);
$CALL GDXXRW.EXE APData.xlsx par=HubCordinate rng=A1:C201
$GDXIN APData.gdx
$LOAD HubCordinate
$GDXIN
parameter HubDistanceData(TNhubs1, TNhubs2);
execseed = %execseed%;

HubDistanceData(TNhubs1, TNhubs2) = sqrt((HubCordinate(TNhubs1, '1') - HubCordinate(TNhubs2, '1'))*(HubCordinate(TNhubs1, '1') - HubCordinate(TNhubs2, '1')) + (HubCordinate(TNhubs1, '2') - HubCordinate(TNhubs2, '2'))*(HubCordinate(TNhubs1, '2') - HubCordinate(TNhubs2, '2')));

set origins              "Origins Set"           /1*%ORGSET%/;
set destinations         "destinations Set"      /1*%DSTSET%/;
set products             "Product Type Set"      /1*%PRODSET%/;
set hubs                 "hub location set"      /1*%HUBSET%/;
parameter maxtime;
maxtime = %maxtime%;

alias (origins, originsi, originsj);
alias (destinations, destinationsi, destinationsj);
alias (hubs, hubs1, hubs2);
parameter numorigins(products) 'Number of origin choices for products';
numorigins(products) = ceil(uniform(1.00000000000001, %orgdstnum%)) ;
parameter numdestinations(products) 'Number of destination choices for products';
numdestinations(products) = ceil(uniform(1.00000000000001, %orgdstnum%)) ;
set ProdOriginsset(products, origins);
ProdOriginsset(products, origins) = no;
set ProdDestinationsset(products, destinations);
ProdDestinationsset(products, destinations) = no;

************************************* BELOW: SAMPLE THE ORIGINS AND DESTINATIONS FOR ALL PRODUCTS *******************
* SEQUANTIALLY ASSIGN ORIGINS AND DESTINATIONS TO PRODUCTS UNTIL THE FULL SETS OF ORIGINS AND DESTINATIONS ARE USED.
* THEN RANDOMLY ASSIGN ORIGINS AND DESTINATIONS TO PRODUCTS
parameter tempprodorgsize;
parameter OrderAssignFlag, RandomAssignFlag, randnum;
OrderAssignFlag = 0 ;
parameter AlreadyAssigned;
AlreadyAssigned = 0;
set randomsamp /1*1000/;

loop(products,
          tempprodorgsize = 0;
          if(tempprodorgsize lt numorigins(products) and OrderAssignFlag eq 0,
                loop(origins$(ord(origins) gt AlreadyAssigned and numorigins(products) gt tempprodorgsize),
                           ProdOriginsset(products, origins) = yes;
                           AlreadyAssigned = AlreadyAssigned + 1;
                           tempprodorgsize = tempprodorgsize + 1;
                           if(ord(origins) eq card(origins) and tempprodorgsize le numorigins(products),
                                    OrderAssignFlag = 1;
                                    loop(originsi$(numorigins(products) gt tempprodorgsize),
                                              ProdOriginsset(products, originsi) = yes;
                                              tempprodorgsize = tempprodorgsize + 1;
                                         );
                              );
                     );
               );
          if(OrderAssignFlag eq 1,
                  loop(randomsamp$(numorigins(products) gt tempprodorgsize),
                           randnum = ceil(uniform(0.00000000000001, card(origins))) ;
                           loop(origins$(ord(origins) eq randnum and not ProdOriginsset(products, origins)),
                                    ProdOriginsset(products, origins) = yes;
                                    tempprodorgsize = tempprodorgsize + 1;
                                );
                       );
             );
     );
parameter samporiginresult(products);
samporiginresult(products) = 0;
loop(products, loop(origins,
          if(ProdOriginsset(products, origins),
                  samporiginresult(products) = samporiginresult(products) + 1;
             );
     ););

OrderAssignFlag = 0 ;
AlreadyAssigned = 0;
loop(products,
          tempprodorgsize = 0;
          if(tempprodorgsize lt numdestinations(products) and OrderAssignFlag eq 0,
                loop(destinations$(ord(destinations) gt AlreadyAssigned and numdestinations(products) gt tempprodorgsize),
                           ProdDestinationsset(products, destinations) = yes;
                           AlreadyAssigned = AlreadyAssigned + 1;
                           tempprodorgsize = tempprodorgsize + 1;
                           if(ord(destinations) eq card(destinations) and tempprodorgsize le numdestinations(products),
                                    OrderAssignFlag = 1;
                                    loop(destinationsi$(numdestinations(products) gt tempprodorgsize),
                                              ProdDestinationsset(products, destinationsi) = yes;
                                              tempprodorgsize = tempprodorgsize + 1;
                                         );
                              );
                     );
               );
          if(OrderAssignFlag eq 1,
                  loop(randomsamp$(numdestinations(products) gt tempprodorgsize),
                           randnum = ceil(uniform(0.00000000000001, card(destinations))) ;
                           loop(destinations$(ord(destinations) eq randnum and not ProdDestinationsset(products, destinations)),
                                    ProdDestinationsset(products, destinations) = yes;
                                    tempprodorgsize = tempprodorgsize + 1;
                                );
                       );
             );
     );
parameter sampodestinationresult(products);
sampodestinationresult(products) = 0;
loop(products, loop(destinations,
          if(ProdDestinationsset(products, destinations),
                  sampodestinationresult(products) = sampodestinationresult(products) + 1;
             );
     ););
*display numdestinations, samporiginresult, numdestinations, sampodestinationresult;
************************************* ABOVE: SAMPLE THE ORIGINS AND DESTINATIONS FOR ALL PRODUCTS *******************
display ProdOriginsset ;
display ProdDestinationsset ;

**** Calculate distance
parameter PHdistance(origins, hubs);
loop(TNhubs1$(ord(TNhubs1) le card(origins)),
      loop(TNhubs2$(ord(TNhubs2) gt card(origins) and ord(TNhubs2) le card(origins) + card(hubs)),
                PHdistance(origins, hubs)$(ord(origins) eq ord(TNhubs1) and ord(hubs) eq ord(TNhubs2) - card(origins)) = HubDistanceData(TNhubs1, TNhubs2);
           );
     );

parameter HHdistance(hubs1, hubs2);
loop(TNhubs1$(ord(TNhubs1) gt card(origins) and ord(TNhubs1) le card(origins) + card(hubs)),
      loop(TNhubs2$(ord(TNhubs2) gt card(origins) and ord(TNhubs2) le card(origins) + card(hubs)),
                HHdistance(hubs1, hubs2)$(ord(hubs1) eq ord(TNhubs1) - card(origins) and ord(hubs2) eq ord(TNhubs2) - card(origins)) = HubDistanceData(TNhubs1, TNhubs2);
           );
     );

parameter HPDdistance(hubs, destinations);
loop(TNhubs1$(ord(TNhubs1) gt card(origins) and ord(TNhubs1) le card(origins) + card(hubs)),
      loop(TNhubs2$(ord(TNhubs2) gt card(origins) + card(hubs) and ord(TNhubs2) le card(origins) + card(hubs) + card(destinations)),
                HPDdistance(hubs, destinations)$(ord(hubs) eq ord(TNhubs1) - card(origins) and ord(destinations) eq ord(TNhubs2) - card(origins) - card(hubs)) = HubDistanceData(TNhubs1, TNhubs2);
           );
     );

parameter discoutf1, discoutf2, discoutf3 ;
discoutf1 = %transcost%*0.0001;
discoutf2 = %transcost%*%EOC%;
discoutf3 = %transcost%*0.0001;

parameter values(products);
values(products) = ceil(uniform(0.1, 2)*1000)/1000;

parameter demands(products);
demands(products) = %dmd%*ceil(uniform(25, 80));

parameter ocCost(origins, products) 'origin cost';
ocCost(origins, products) = %asscost%*1*ceil(uniform(20, 40));

parameter dcCost(destinations, products) 'origin cost';
dcCost(destinations, products) = %asscost%*1*ceil(uniform(20, 40));

parameter HubCostcoef;
HubCostcoef = %hubsetcost%;
parameter HubCost(hubs);
HubCost(hubs) = HubCostcoef*ceil(uniform(200, 1000));
parameter avaerageHubCost;
avaerageHubCost = sum(hubs, HubCost(hubs))/card(hubs);

parameter transcost(origins, destinations, hubs1, hubs2, products);
set hubspp(origins, destinations, products, hubs1, hubs2)  ;
transcost(origins, destinations, hubs1, hubs2, products)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)) = 1*demands(products)*values(products)*(discoutf1*PHdistance(origins, hubs1) + discoutf2*HHdistance(hubs1, hubs2) + discoutf3*HPDdistance(hubs2, destinations)) ;

hubspp(origins, destinations, products, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)) = yes;
hubspp(origins, destinations, products, hubs1, hubs2)$(transcost(origins, destinations, hubs1, hubs2, products) gt transcost(origins, destinations, hubs2, hubs1, products)) = no;
hubspp(origins, destinations, products, hubs1, hubs2)$((ord(hubs1) ne ord(hubs2)) and (transcost(origins, destinations, hubs1, hubs2, products) ge transcost(origins, destinations, hubs1, hubs1, products) or transcost(origins, destinations, hubs1, hubs2, products) ge transcost(origins, destinations, hubs2, hubs2, products))) = no;


*** Compute average demand by origin and product = demand of product divided by the number of origin nodes the product can source from
*** Define capacity by origin and product family
parameter CACapCoef, CPcapCoef;
CACapCoef = %CACapR%;
CPcapCoef = %CPCapR%;

parameter CAcap(origins, products);
CAcap(origins, products)$(ProdOriginsset(products, origins)) = CACapCoef*ceil(uniform(0.95, 1.15)*100)/100*demands(products);
parameter CPcap(destinations, products);
CPcap(destinations, products)$(ProdDestinationsset(products, destinations)) = CPcapCoef*ceil(uniform(0.95, 1.15)*100)/100*demands(products);

parameter CAcap2(origins, products), CPcap2(destinations, products) ;
CAcap2(origins, products) = min(CAcap(origins, products)/demands(products), 1);
CPcap2(destinations, products) = min(CPcap(destinations, products)/demands(products), 1);

*********** Get cost ranking
parameter rankhubcost(hubs);
parameter findlimitnum, foundflag, minhubnum;
findlimitnum = 0;
set foundhubs(hubs);
foundhubs(hubs) = no;
set findlimit /1*500/;
parameter findlimitnum, findmaxi;

loop(findlimit$(findlimitnum le card(hubs)),
         minhubnum = smin(hubs$(not foundhubs(hubs)), HubCost(hubs));
         foundflag = 0;
         loop(hubs$(not foundhubs(hubs) and minhubnum eq HubCost(hubs) and foundflag eq 0),
                   findlimitnum = findlimitnum + 1 ;
                   rankhubcost(hubs) = findlimitnum ;
                   foundhubs(hubs) = yes ;
                   foundflag = 1;
              );
     );

parameter rankhubcstpct(hubs);
rankhubcstpct(hubs) = rankhubcost(hubs)/card(hubs);
rankhubcstpct(hubs) = round(rankhubcstpct(hubs), 1);
rankhubcstpct(hubs)$(rankhubcstpct(hubs) ge 0.5) = 0.5 ;

parameter rankorgcost(products, origins);
parameter minorgnum;
set foundorg(products, origins);
foundorg(products, origins) = no;

loop(products,
         findlimitnum = 0 ;
         loop(findlimit$(findlimitnum le numorigins(products)),
                  minorgnum = smin(origins$(not foundorg(products, origins) and ProdOriginsset(products, origins)), ocCost(origins, products));
                  foundflag = 0;
                  loop(origins$(not foundorg(products, origins) and minorgnum eq ocCost(origins, products) and foundflag eq 0 and ProdOriginsset(products, origins)),
                            findlimitnum = findlimitnum + 1 ;
                            rankorgcost(products, origins) = findlimitnum ;
                            foundorg(products, origins) = yes ;
                            foundflag = 1;
                       );
              );
     );

* display rankorgcost, ocCost ;

parameter rankdstcost(products, destinations);
parameter mindstnum;
set founddst(products, destinations);
founddst(products, destinations) = no;

loop(products,
         findlimitnum = 0 ;
         loop(findlimit$(findlimitnum le numdestinations(products)),
                  mindstnum = smin(destinations$(not founddst(products, destinations) and ProdDestinationsset(products, destinations)), dcCost(destinations, products));
                  foundflag = 0;
                  loop(destinations$(not founddst(products, destinations) and mindstnum eq dcCost(destinations, products) and foundflag eq 0 and ProdDestinationsset(products, destinations)),
                            findlimitnum = findlimitnum + 1 ;
                            rankdstcost(products, destinations) = findlimitnum ;
                            founddst(products, destinations) = yes ;
                            foundflag = 1;
                       );
              );
     );

********************************* Mathematical Models ******************************************
positive variables x(origins, destinations, products, hubs1, hubs2);
binary variables   z(hubs),
                   y(origins, products),
                   u(destinations, products);
variable totalcost;

equations objective,
          singleroute(products),
          flowhubrel(products, hubs),
          originscap(origins, products),
          destinationscap(destinations, products),
          Oflowrouterel(origins, products),
          Dflowrouterel(destinations, products) ;

objective..
       totalcost =e= sum((hubs), HubCost(hubs)*z(hubs))
                         + sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost(origins, destinations, hubs1, hubs2, products)*x(origins, destinations, products, hubs1, hubs2))
                         + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                         + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products))  ;

singleroute(products)..
      sum((origins, destinations, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =e= 1 ;

flowhubrel(products, hubs)..
      sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs, hubs1) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs, hubs1) )
         + sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ord(hubs1) ne ord(hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs) ) =l= z(hubs);

originscap(origins, products)$(ProdOriginsset(products, origins))..
      sum((destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =l= CAcap2(origins, products)*y(origins, products) ;

destinationscap(destinations, products)$(ProdDestinationsset(products, destinations))..
      sum((origins, hubs1, hubs2)$(ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins)), x(origins, destinations, products, hubs1, hubs2)) =l= CPcap2(destinations, products)*u(destinations, products) ;

Oflowrouterel(origins, products)$(ProdOriginsset(products, origins))..
      sum((destinations, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =l= y(origins, products) ;

Dflowrouterel(destinations, products)$(ProdDestinationsset(products, destinations))..
      sum((origins, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins)), x(origins, destinations, products, hubs1, hubs2)) =l= u(destinations, products) ;

model HubLocationODA /objective, singleroute, flowhubrel, originscap, destinationscap/;
Option MIP = cplex;
HubLocationODA.optfile =1;
HubLocationODA.solprint = 2;
HubLocationODA.reslim = 1000;
HubLocationODA.optca = 0.000;
HubLocationODA.optcr = 0.000;
HubLocationODA.iterlim = 4000000;
*solve HubLocationODA minimizing totalcost using mip;
*display z.l, totalcost.l ;


parameter orgxproduct(products), orgyproduct(products), dstxproduct(products), dstyproduct(products);
parameter xvalueorigins(origins), yvalueorigins(origins), xvaluehubs(hubs), yvaluehubs(hubs), xvaluedst(destinations), yvaluedst(destinations);
loop(origins, loop(TNhubs,
         if(ord(origins) eq ord(TNhubs),
                  xvalueorigins(origins) = HubCordinate(TNhubs, '1');
                  yvalueorigins(origins) = HubCordinate(TNhubs, '2');
             );
     ););
loop(hubs, loop(TNhubs,
         if(ord(hubs) eq (ord(TNhubs) - card(origins)),
                  xvaluehubs(hubs) = HubCordinate(TNhubs, '1');
                  yvaluehubs(hubs) = HubCordinate(TNhubs, '2');
             );
     ););
loop(destinations, loop(TNhubs,
          if(ord(destinations) eq (ord(TNhubs) - card(origins) - card(hubs)),
                  xvaluedst(destinations) = HubCordinate(TNhubs, '1');
                  yvaluedst(destinations) = HubCordinate(TNhubs, '2');
             );
     ););

display xvalueorigins, yvalueorigins, xvaluedst, yvaluedst;
parameter totalxvalue, totalyvalue, tempnum;
loop(products,
        tempnum = 0;
        totalxvalue = 0;
        totalyvalue = 0;
        loop(origins$ProdOriginsset(products, origins),
                  totalxvalue = totalxvalue + xvalueorigins(origins);
                  totalyvalue = totalyvalue + yvalueorigins(origins);
                  tempnum = tempnum + 1;
             );
        orgxproduct(products) = totalxvalue/tempnum;
        orgyproduct(products) = totalyvalue/tempnum;

        tempnum = 0;
        totalxvalue = 0;
        totalyvalue = 0;
        loop(destinations$ProdDestinationsset(products, destinations),
                  totalxvalue = totalxvalue + xvaluedst(destinations);
                  totalyvalue = totalyvalue + yvaluedst(destinations);
                  tempnum = tempnum + 1;
             );
        dstxproduct(products) = totalxvalue/tempnum;
        dstyproduct(products) = totalyvalue/tempnum
     );

**** Calculate distance
parameter prodorghubdistance(products, hubs);
prodorghubdistance(products, hubs) = sqrt((orgxproduct(products) - xvaluehubs(hubs))*(orgxproduct(products) - xvaluehubs(hubs)) + (orgyproduct(products) - yvaluehubs(hubs))*(orgyproduct(products) - yvaluehubs(hubs)));

parameter proddsthubdistance(hubs, products);
proddsthubdistance(hubs, products) = sqrt((dstxproduct(products) - xvaluehubs(hubs))*(dstxproduct(products) - xvaluehubs(hubs)) + (dstyproduct(products) - yvaluehubs(hubs))*(dstyproduct(products) - yvaluehubs(hubs)));

parameter transcostsingle(hubs1, hubs2, products);
set hubsppsingle(products, hubs1, hubs2);
transcostsingle(hubs1, hubs2, products) = demands(products)*values(products)*(discoutf1*prodorghubdistance(products, hubs1) + discoutf2*HHdistance(hubs1, hubs2) + discoutf3*proddsthubdistance(hubs2, products)) ;

hubsppsingle(products, hubs1, hubs2) = yes;
hubsppsingle(products, hubs1, hubs2)$(transcostsingle(hubs1, hubs2, products) gt transcostsingle(hubs2, hubs1, products)) = no;
hubsppsingle(products, hubs1, hubs2)$((ord(hubs1) ne ord(hubs2)) and (transcostsingle(hubs1, hubs2, products) ge transcostsingle(hubs1, hubs1, products) or transcostsingle(hubs1, hubs2, products) ge transcostsingle(hubs2, hubs2, products))) = no;

********************************* Single Heuristic Mathematical Models ******************************************
positive variables xsingle(products, hubs1, hubs2);
binary variables   zsingle(hubs) ;
variable totalcostsingle;

equations objectivesingle,
          singleroutesingle(products),
          flowshubrelsingle(products, hubs)  ;

objectivesingle..
       totalcostsingle =e= sum((hubs), HubCost(hubs)*zsingle(hubs))
                         + sum((hubs1, hubs2, products)$(hubsppsingle(products, hubs1, hubs2)), transcostsingle(hubs1, hubs2, products)*xsingle(products, hubs1, hubs2)) ;

singleroutesingle(products)..
      sum((hubs1, hubs2)$(hubsppsingle(products, hubs1, hubs2)), xsingle(products, hubs1, hubs2)) =e= 1 ;

flowshubrelsingle(products, hubs)..
      sum((hubs1)$(hubsppsingle(products, hubs, hubs1)), xsingle(products, hubs, hubs1) )
         + sum((hubs1)$(hubsppsingle(products, hubs1, hubs) and ord(hubs1) ne ord(hubs)), xsingle(products, hubs1, hubs) ) =l= zsingle(hubs);

model HubLocationSingle /objectivesingle, singleroutesingle, flowshubrelsingle/;
HubLocationSingle.optfile =1;
HubLocationSingle.solprint = 2;
HubLocationSingle.reslim = 1000;
HubLocationSingle.optca = 0.000;
HubLocationSingle.optcr = 0.000;
HubLocationSingle.iterlim = 4000000;
solve HubLocationSingle minimizing totalcostsingle using mip;

z.fx(hubs) = zsingle.l(hubs);
solve HubLocationODA minimizing totalcost using mip;

file SingleHeuristic /SingleHeuristic.dat/;
put SingleHeuristic;
SingleHeuristic.ap = 1;

parameter UB;
UB = totalcost.l;

parameter totalopenhubs;
totalopenhubs = 0;
loop(hubs$(z.l(hubs) eq 1),
         totalopenhubs = totalopenhubs + 1;
      );
parameter totaltime;
totaltime = HubLocationSingle.etSolve + HubLocationODA.etSolve;

parameter totalhubcost, totalIassCost, totalDassCost, totaltransportcost;
totalhubcost = sum((hubs), HubCost(hubs)*z.l(hubs))  ;
totalIassCost = sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y.l(origins, products))  ;
totalDassCost = sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u.l(destinations, products))  ;
totaltransportcost = sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost(origins, destinations, hubs1, hubs2, products)*x.l(origins, destinations, products, hubs1, hubs2))  ;
put  %ID%, " ", totaltime, " ", totalopenhubs, " ", totalhubcost, " ", totalIassCost, " ", totalDassCost, " ", totaltransportcost, " ", UB  ///
display  totaltime, " ", totalopenhubs, " ", totalhubcost, " ", totalIassCost, " ", totalDassCost, " ", totaltransportcost, " ", UB  ;
