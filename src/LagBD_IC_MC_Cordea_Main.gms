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
$set probthreshold 0.1

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
HubLocationODA.solprint = 2;
HubLocationODA.reslim = 1800;
HubLocationODA.optca = 0.000;
HubLocationODA.optcr = 0.000;
HubLocationODA.iterlim = 4000000;
solve HubLocationODA minimizing totalcost using rmip;
*parameter optobj;
*optobj = totalcost.l;
*display optobj ;
parameter modelsolvetime;
modelsolvetime = HubLocationODA.etSolve;
parameter modelsovstatus;
*modelsovstatus = HubLocationODA.modelstat ;
*parameter LowerB;
*LowerB = HubLocationODA.ObjEst;
*display z.l, x.l, y.l, u.l;
parameter zrlxsol(hubs), yrlxsol(origins, products), urlxsol(destinations, products);
zrlxsol(hubs) = z.l(hubs);
yrlxsol(origins, products) = y.l(origins, products);
urlxsol(destinations, products) = u.l(destinations, products);
display z.l ;


**********************************************************Lagrangian Relaxation Heuristics******************************************
parameter phi(products, hubs);
parameter xi1(origins, products);
parameter xi2(destinations, products);

parameter Gradient_prev(products, hubs);
parameter Gradient_xi1_prev(origins, products);
parameter Gradient_xi2_prev(destinations, products);
parameter LB, UB;
parameter totaltime;
parameter omega;
parameter stepnoIMP;
parameter stepnoIMPallowed;
parameter LagDone ;
parameter TotalcostLag;
parameter ThetaValue;
parameter Gradient_curr(products, hubs);
parameter Gradient_xi1_curr(origins, products);
parameter Gradient_xi2_curr(destinations, products);
parameter multiplication1, multiplication2, multiplication3;
parameter multiplication1xi1, multiplication2xi1, multiplication3xi1;
parameter multiplication1xi2, multiplication2xi2, multiplication3xi2;
parameter steplength;
parameter omega_decrease_rate;

set iterations /1*500/;
parameter iternumber;

phi(products, hubs) = abs(flowhubrel.m(products, hubs));;
xi1(origins, products) = abs(originscap.m(origins, products));
xi2(destinations, products) = abs(destinationscap.m(destinations, products));

Gradient_prev(products, hubs) = 0;
Gradient_xi1_prev(origins, products) = 0;
Gradient_xi2_prev(destinations, products) = 0;

LB = -inf;
UB = inf;
parameter tempUB;
totaltime = 0 ;
omega = 2 ;
stepnoIMP = 0 ;
stepnoIMPallowed = 20 ;
LagDone = 0 ;
omega_decrease_rate = 1.001;

equations Lagrangianobjectives, Lagrangianobjectives1 ;
variables Lagtotalcost, Lagtotalcost1;
parameter fc_bar(hubs);
parameter tc_bar(origins, destinations, hubs1, hubs2, products);


Lagrangianobjectives..
       Lagtotalcost =e= sum((hubs), fc_bar(hubs)*z(hubs))
                         + sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), tc_bar(origins, destinations, hubs1, hubs2, products)*x(origins, destinations, products, hubs1, hubs2))
                         + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                         + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products))
                         - sum((origins, products)$(ProdOriginsset(products, origins)), CAcap2(origins, products)*y(origins, products)*xi1(origins, products))
                         - sum((destinations, products)$(ProdDestinationsset(products, destinations)), CPcap2(destinations, products)*u(destinations, products)*xi2(destinations, products)) ;


model HL_LagSub /Lagrangianobjectives, singleroute, Oflowrouterel, Dflowrouterel/;
HL_LagSub.solprint = 2;
HL_LagSub.reslim = 3600;
HL_LagSub.optca = 0.000;
HL_LagSub.optcr = 0.000;
HL_LagSub.iterlim = 4000000;

parameter foundsoldone;
parameter LagCost1, LagCost2, LagCost2T(products), LagCost1B;
parameter tc_bar_para(origins, destinations, hubs1, hubs2);
set minSet(origins, destinations, hubs1, hubs2);
set hubused(hubs);

parameter x_feasol(origins, destinations, products, hubs1, hubs2);
parameter z_feasol(hubs), y_feasol(origins, products), u_feasol(destinations, products);
parameter x_Lagsol(origins, destinations, products, hubs1, hubs2), zLagsol(hubs), yLagsol(origins, products), uLagsol(destinations, products) ;
parameter x_feasol_total(hubs), y_feasol_total(origins, products), u_feasol_total(destinations, products) ;
parameter useratio_org(products), useratio_dst(products), useratio(products);
set  close_org_node(origins, products), close_dst_node(destinations, products);
parameter demands_new(products), CAcap_new(origins, products), CPcap_new(destinations, products), ocCost_new(origins, products), dcCost_new(destinations, products)  ;
parameter routeAVGcost;
set newRoute(origins, destinations, hubs1, hubs2), neworigin(origins), newdst(destinations);
parameter totalIassCost, totalDassCost, totalhubcost, totaltransportcost;
parameter totalopenhubs;
parameter totalIassCostTEMP, totalDassCostTEMP, totalhubcostTEMP, totaltransportcostTEMP;
parameter totalopenhubsTEMP;

parameter smoothdone;
set smoothloop /1*100/;
parameter minhubcost;
scalar starttime;
starttime = jnow;
scalar elapsed;
elapsed = (jnow - starttime)*24*360;
parameter lagstep;
lagstep = 5;
set LagSS /1*1/;
parameter LagZSol(LagSS, hubs);
parameter LagYSol(LagSS, origins, products);
parameter LagUSol(LagSS, destinations, products);
parameter LagScount;
LagScount = 0;
LagZSol(LagSS, hubs) = 0;
LagYSol(LagSS, origins, products) = 0;
LagUSol(LagSS, destinations, products) = 0;
parameter LagZSolTlt(hubs), LagZS(hubs);
parameter LagYSolTlt(origins, products), LagYS(origins, products);
parameter LagUSolTlt(destinations, products), LagUS(destinations, products);
LagZSolTlt(hubs) = 0 ;
LagYSolTlt(origins, products) = 0 ;
LagUSolTlt(destinations, products) = 0 ;

parameter lagHubopennum;
if(HubCostcoef eq 10,
          lagHubopennum = 1;
        else
          lagHubopennum = 3;
   );
set pickhubs /1*10/;
parameter pickedhubs(hubs), pickednum, pickedone;
parameter lowestcost, pickedlowestcost(products);

parameter pickbyzrelax(hubs);
pickbyzrelax(hubs) = no;
parameter pickbound, pickrelnum;
pickbound =0.3;
pickrelnum = 0;
loop(pickhubs$(pickrelnum eq 0),
         loop(hubs$(zrlxsol(hubs) ge pickbound),
                   pickbyzrelax(hubs) = yes;
                   pickrelnum = pickrelnum + 1;
              );
         pickbound = pickbound - 0.05;
     );
set hubspp2(origins, destinations, products, hubs1, hubs2)  ;

loop(iterations$(LagDone eq 0 and stepnoIMP le 20000 and elapsed le maxtime and ord(iterations) le lagstep),
         iternumber = ord(iterations) ;
         fc_bar(hubs) = HubCost(hubs) - sum(products, phi(products, hubs));
         tc_bar(origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and (ord(hubs1) ne ord(hubs2))) =  transcost(origins, destinations, hubs1, hubs2, products) + phi(products, hubs1) + phi(products, hubs2) + (xi1(origins, products) + xi2(destinations, products));
         tc_bar(origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and ord(hubs1) eq ord(hubs2)) =  transcost(origins, destinations, hubs1, hubs2, products) + phi(products, hubs1) + (xi1(origins, products) + xi2(destinations, products)) ;

         LagCost1 = 0;
         loop(hubs,
                    if(fc_bar(hubs) le 0,
                          zLagsol(hubs) = 1;
                          LagCost1 = LagCost1 + fc_bar(hubs);
                            else
                              zLagsol(hubs) = 0;
                       );
             );
         if(ord(iterations) eq 1 and sum(hubs, zLagsol(hubs)) eq 0,
                   minhubcost = smin(hubs, fc_bar(hubs));
                   zLagsol(hubs)$(fc_bar(hubs) eq minhubcost) = 1;
            );
         LagCost1B = 0;
         loop(products, loop(origins$ProdOriginsset(products, origins),
                    if(ocCost(origins, products) - CAcap2(origins, products)*xi1(origins, products) le 0,
                          yLagsol(origins, products) = 1;
                          LagCost1B = LagCost1B + ocCost(origins, products) - CAcap2(origins, products)*xi1(origins, products);
                            else
                              yLagsol(origins, products) = 0;
                       );
             ););
         loop(products, loop(destinations$ProdDestinationsset(products, destinations),
                    if(dcCost(destinations, products) - CPcap2(destinations, products)*xi2(destinations, products) le 0,
                          uLagsol(destinations, products) = 1;
                          LagCost1B = LagCost1B + dcCost(destinations, products) - CPcap2(destinations, products)*xi2(destinations, products);
                            else
                              uLagsol(destinations, products) = 0;
                       );
             ););

         LagCost2 = 0;
         loop(products,
                   minSet(origins, destinations, hubs1, hubs2) = no;
                   tc_bar_para(origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and (ord(hubs1) ne ord(hubs2)) and hubspp(origins, destinations, products, hubs1, hubs2)) =  transcost(origins, destinations, hubs1, hubs2, products) + phi(products, hubs1) + phi(products, hubs2) + (xi1(origins, products) + xi2(destinations, products))
                                                                                                                                                                                                                                                     + max(0, ocCost(origins, products) - CAcap2(origins, products)*xi1(origins, products)) + max(0, dcCost(destinations, products) - CPcap2(destinations, products)*xi2(destinations, products))  ;
                   tc_bar_para(origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and (ord(hubs1) eq ord(hubs2)) and hubspp(origins, destinations, products, hubs1, hubs2)) =  transcost(origins, destinations, hubs1, hubs2, products) + phi(products, hubs1) + (xi1(origins, products) + xi2(destinations, products))
                                                                                                                                                                                                                                                     + max(0, ocCost(origins, products) - CAcap2(origins, products)*xi1(origins, products)) + max(0, dcCost(destinations, products) - CPcap2(destinations, products)*xi2(destinations, products))  ;
                   LagCost2T(products) = smin((origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2)), tc_bar_para(origins, destinations, hubs1, hubs2));

                   foundsoldone = 0;
                   loop((origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and tc_bar_para(origins, destinations, hubs1, hubs2) eq LagCost2T(products) and foundsoldone eq 0 and hubspp(origins, destinations, products, hubs1, hubs2)),
                              minSet(origins, destinations, hubs1, hubs2) = yes;
                              yLagsol(origins, products) = 1;
                              uLagsol(destinations, products) = 1;
                              foundsoldone = 1;
                        );
                   LagCost2 = LagCost2 + LagCost2T(products);

                   Gradient_curr(products, hubs) = sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and minSet(origins, destinations, hubs1, hubs)), 1)
                                                            + sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs, hubs1) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and ord(hubs) ne ord(hubs1) and minSet(origins, destinations, hubs, hubs1)), 1)
                                                            - zLagsol(hubs);
                   Gradient_xi1_curr(origins, products) = sum((destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdDestinationsset(products, destinations) and minSet(origins, destinations, hubs1, hubs2)), 0.5*demands(products)*1) - 0.5*demands(products)*CAcap2(origins, products)*yLagsol(origins, products) ;
                   Gradient_xi2_curr(destinations, products) = sum((origins, hubs1, hubs2)$(ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and minSet(origins, destinations, hubs1, hubs2)), 0.5*demands(products)*1) - 0.5*demands(products)*CPcap2(destinations, products)*uLagsol(destinations, products);
                   x_feasol(origins, destinations, products, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and minSet(origins, destinations, hubs1, hubs2)) = 1;
                   x_feasol(origins, destinations, products, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and not minSet(origins, destinations, hubs1, hubs2)) = 0;
             );

          TotalcostLag = LagCost1 + LagCost1B + LagCost2 ;

* Below adjust x_feasol solutions
          x_feasol_total(hubs) = sum((origins, destinations, products, hubs2)$(hubspp(origins, destinations, products, hubs, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x_feasol(origins, destinations, products, hubs, hubs2))
                                                     + sum((origins, destinations, products, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x_feasol(origins, destinations, products, hubs1, hubs));
          z_feasol(hubs)$(x_feasol_total(hubs) ge 1) = 1;
          z_feasol(hubs)$(x_feasol_total(hubs) lt 1) = 0;
          pickedhubs(hubs) = no;
          hubspp2(origins, destinations, products, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2)) = no;
          if(iternumber le 3,
                  pickedhubs(hubs) = pickbyzrelax(hubs);
                       else
                            pickednum = 0;
                            loop(pickhubs$(pickednum lt lagHubopennum),
                                      lowestcost = smin(hubs$(not pickedhubs(hubs) and z_feasol(hubs) eq 1), HubCost(hubs));
                                      pickedone = 0;
                                      loop(hubs$(not pickedhubs(hubs) and z_feasol(hubs) eq 1 and pickedone eq 0 and HubCost(hubs) eq lowestcost),
                                               pickedhubs(hubs) = yes;
                                               pickedone = 1;
                                               pickednum = pickednum + 1;
                                           );
                                 );
              );
          hubspp2(origins, destinations, products, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and pickedhubs(hubs1) and pickedhubs(hubs2)) = hubspp(origins, destinations, products, hubs1, hubs2);
          x_feasol(origins, destinations, products, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)) = 0;
          loop(products,
                   minSet(origins, destinations, hubs1, hubs2) = no;
                   pickedlowestcost(products) = smin((origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp2(origins, destinations, products, hubs1, hubs2)), tc_bar_para(origins, destinations, hubs1, hubs2));
                   foundsoldone = 0;
                   loop((origins, destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and tc_bar_para(origins, destinations, hubs1, hubs2) eq pickedlowestcost(products) and foundsoldone eq 0 and hubspp2(origins, destinations, products, hubs1, hubs2)),
                              minSet(origins, destinations, hubs1, hubs2) = yes;
                              foundsoldone = 1;
                        );
                   x_feasol(origins, destinations, products, hubs1, hubs2)$(hubspp2(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and minSet(origins, destinations, hubs1, hubs2)) = 1;
                   x_feasol(origins, destinations, products, hubs1, hubs2)$(hubspp2(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and not minSet(origins, destinations, hubs1, hubs2)) = 0;
             );
* Above adjust x_feasol solutions

          if(mod(iternumber, 1) eq 0 or iternumber eq 1,
                   x_feasol_total(hubs) = sum((origins, destinations, products, hubs2)$(hubspp(origins, destinations, products, hubs, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x_feasol(origins, destinations, products, hubs, hubs2))
                                                     + sum((origins, destinations, products, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x_feasol(origins, destinations, products, hubs1, hubs));
                   z_feasol(hubs)$(x_feasol_total(hubs) ge 1) = 1;
                   z_feasol(hubs)$(x_feasol_total(hubs) lt 1) = 0;
                   y_feasol_total(origins, products)$ProdOriginsset(products, origins) = sum((destinations, hubs1, hubs2)$ProdDestinationsset(products, destinations), x_feasol(origins, destinations, products, hubs1, hubs2));
                   u_feasol_total(destinations, products)$ProdDestinationsset(products, destinations) = sum((origins, hubs1, hubs2)$ProdOriginsset(products, origins), x_feasol(origins, destinations, products, hubs1, hubs2));
                   y_feasol(origins, products)$(y_feasol_total(origins, products) ge 1) = 1;
                   y_feasol(origins, products)$(y_feasol_total(origins, products) lt 1) = 0;
                   u_feasol(destinations, products)$(u_feasol_total(destinations, products) ge 1) = 1;
                   u_feasol(destinations, products)$(u_feasol_total(destinations, products) lt 1) = 0;

                   close_org_node(origins, products) = no;
                   close_dst_node(destinations, products) = no;
                   ocCost_new(origins, products) = ocCost(origins, products);
                   dcCost_new(destinations, products) = dcCost(destinations, products);
                   CAcap_new(origins, products) = CAcap2(origins, products);
                   CPcap_new(destinations, products) = CPcap2(destinations, products);

                   loop(products,
                               smoothdone = 0;
                               loop(origins$(ProdOriginsset(products, origins) and y_feasol(origins, products) eq 1),
                                        loop(destinations$(ProdDestinationsset(products, destinations) and u_feasol(destinations, products) eq 1),
                                                 ocCost_new(origins, products) = 0;
                                                 dcCost_new(destinations, products) = 0;
                                                 useratio_org(products) = 1/CAcap2(origins, products) ;
                                                 useratio_dst(products) = 1/CPcap2(destinations, products);
                                                 useratio(products) = max(useratio_org(products), useratio_dst(products));
                                                 if(useratio(products) > 1,
                                                      demands_new(products) = 1 - 1/useratio(products);
                                                      CAcap_new(origins, products) = max(0, CAcap2(origins, products) - CAcap2(origins, products)/useratio(products)) ;
                                                      CPcap_new(destinations, products) = max(0, CPcap2(destinations, products) - CPcap2(destinations, products)/useratio(products)) ;
                                                      x_feasol(origins, destinations, products, hubs1, hubs2)$(x_feasol(origins, destinations, products, hubs1, hubs2) eq 1) = 1/useratio(products);
*                                                      display useratio, demands, demands_new, CAcap_new, CAcap, CPcap_new, CPcap, x_feasol;
                                                      if(useratio_org(products) > 1,
                                                            close_org_node(origins, products) = yes;
                                                         );
                                                      if(useratio_dst(products) > 1,
                                                            close_dst_node(destinations, products) = yes;
                                                         );
                                                      else
                                                         smoothdone = 1;
                                                   );

                                             );
                                    );

                               loop(smoothloop$(smoothdone eq 0),
                                         newRoute(origins, destinations, hubs1, hubs2) = no;
                                         neworigin(origins) = no;
                                         newdst(destinations) = no;
                                         routeAVGcost = smin((origins, destinations, hubs1, hubs2)$(z_feasol(hubs1) eq 1 and z_feasol(hubs2) eq 1 and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and not close_org_node(origins, products) and not close_dst_node(destinations, products)),
                                                              (ocCost_new(origins, products) + dcCost_new(destinations, products))/demands_new(products) + transcost(origins, destinations, hubs1, hubs2, products));
                                         foundsoldone = 0;
                                         loop((origins, destinations, hubs1, hubs2)$(z_feasol(hubs1) eq 1 and z_feasol(hubs2) eq 1 and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and not close_org_node(origins, products) and not close_dst_node(destinations, products)
                                                                                        and (routeAVGcost eq (ocCost_new(origins, products) + dcCost_new(destinations, products))/demands_new(products) + transcost(origins, destinations, hubs1, hubs2, products)) and (foundsoldone eq 0)),
                                                         newRoute(origins, destinations, hubs1, hubs2) = yes;
                                                         neworigin(origins) = yes;
                                                         newdst(destinations) = yes;
                                                         foundsoldone = 1;
                                                   );

                                         loop(origins$(ProdOriginsset(products, origins) and neworigin(origins)),
                                                 loop(destinations$(ProdDestinationsset(products, destinations) and newdst(destinations)),
                                                          ocCost_new(origins, products) = 0;
                                                          dcCost_new(destinations, products) = 0;
                                                          useratio_org(products) = demands_new(products)/CAcap_new(origins, products) ;
                                                          useratio_dst(products) = demands_new(products)/CPcap_new(destinations, products);
                                                          useratio(products) = max(useratio_org(products), useratio_dst(products));
                                                          if(useratio(products) > 1,
                                                               demands_new(products) = demands_new(products) - demands_new(products)/useratio(products);
                                                               CAcap_new(origins, products) = max(0, CAcap_new(origins, products) - CAcap_new(origins, products)/useratio(products)) ;
                                                               CPcap_new(destinations, products) = max(0, CPcap_new(destinations, products) - CPcap_new(destinations, products)/useratio(products)) ;
                                                               x_feasol(origins, destinations, products, hubs1, hubs2)$(newRoute(origins, destinations, hubs1, hubs2)) = 1/useratio(products)*(demands_new(products)/1);
                                                               y_feasol(origins, products) = 1;
                                                               u_feasol(destinations, products) = 1;
                                                               if(useratio_org(products) > 1,
                                                                     close_org_node(origins, products) = yes;
                                                                  );
                                                               if(useratio_dst(products) > 1,
                                                                     close_dst_node(destinations, products) = yes;
                                                                  );
                                                               else
                                                                  smoothdone = 1;
                                                                  x_feasol(origins, destinations, products, hubs1, hubs2)$(newRoute(origins, destinations, hubs1, hubs2)) = demands_new(products)/1;
                                                                  y_feasol(origins, products) = 1;
                                                                  u_feasol(destinations, products) = 1;
                                                            );

                                                      );
                                             );

                                   );
                       );

                   totalIassCostTEMP = sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y_feasol(origins, products)) ;
                   totalDassCostTEMP = sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u_feasol(destinations, products)) ;
                   totalhubcostTEMP = sum((hubs), HubCost(hubs)*z_feasol(hubs)) ;
                   totaltransportcostTEMP = sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost(origins, destinations, hubs1, hubs2, products)*x_feasol(origins, destinations, products, hubs1, hubs2));
                   totalopenhubsTEMP = sum((hubs), z_feasol(hubs)) ;
                   tempUB = totalIassCostTEMP + totalDassCostTEMP + totalhubcostTEMP + totaltransportcostTEMP;
             );

         if (TotalcostLag gt LB,
                    LB = TotalcostLag ;
                    stepnoIMP = 0;
             );

         if (tempUB le UB,
                UB = tempUB;
                totalIassCost = totalIassCostTEMP ;
                totalDassCost = totalDassCostTEMP ;
                totalhubcost = totalhubcostTEMP ;
                totaltransportcost = totaltransportcostTEMP ;
                totalopenhubs = totalopenhubsTEMP ;
                if(LagScount lt card(LagSS),
                            LagZSol(LagSS, hubs)$(ord(LagSS) eq LagScount + 1) = z_feasol(hubs);
                            LagYSol(LagSS, origins, products)$(ord(LagSS) eq LagScount + 1) = y_feasol(origins, products);
                            LagUSol(LagSS, destinations, products)$(ord(LagSS) eq LagScount + 1) = u_feasol(destinations, products);
                            LagScount = LagScount + 1;
                      else
                            LagZSol(LagSS, hubs) = LagZSol(LagSS + 1, hubs);
                            LagYSol(LagSS, origins, products) = LagYSol(LagSS + 1, origins, products);
                            LagUSol(LagSS, destinations, products) = LagUSol(LagSS + 1, destinations, products);
                            LagZSol(LagSS, hubs)$(ord(LagSS) eq card(LagSS)) = z_feasol(hubs);
                            LagYSol(LagSS, origins, products)$(ord(LagSS) eq card(LagSS)) = y_feasol(origins, products);
                            LagUSol(LagSS, destinations, products)$(ord(LagSS) eq card(LagSS)) = u_feasol(destinations, products);
                   );
             );
         display LB, UB ;
         if(UB - LB lt 0.01,
               LagDone = 1 ;
           );

         multiplication1 =  sum((products, hubs), Gradient_curr(products, hubs)*Gradient_prev(products, hubs))
                              + sum((origins, products)$(ProdOriginsset(products, origins)), Gradient_xi1_curr(origins, products)*Gradient_xi1_prev(origins, products))
                              + sum((destinations, products)$(ProdDestinationsset(products, destinations)), Gradient_xi2_curr(destinations, products)*Gradient_xi2_prev(destinations, products));

         multiplication2 =  sum((products, hubs), Gradient_curr(products, hubs)*Gradient_curr(products, hubs))
                              + sum((origins, products)$(ProdOriginsset(products, origins)), Gradient_xi1_curr(origins, products)*Gradient_xi1_curr(origins, products))
                              + sum((destinations, products)$(ProdDestinationsset(products, destinations)), Gradient_xi2_curr(destinations, products)*Gradient_xi2_curr(destinations, products));

         multiplication3 =  sum((products, hubs), Gradient_prev(products, hubs)*Gradient_prev(products, hubs))
                              + sum((origins, products)$(ProdOriginsset(products, origins)), Gradient_xi1_prev(origins, products)*Gradient_xi1_prev(origins, products))
                              + sum((destinations, products)$(ProdDestinationsset(products, destinations)), Gradient_xi2_prev(destinations, products)*Gradient_xi2_prev(destinations, products)) ;

         if(multiplication2 eq 0,
                 multiplication2 = 0.00001;
            );
         if(multiplication3 eq 0,
                 multiplication3 = 0.00001;
            );
         if(multiplication1 lt 0,
              ThetaValue = multiplication2/multiplication3;
                 else
                     ThetaValue = 0;
            );

         if(iternumber gt 1,
              steplength = omega*(UB - LB)/multiplication2;
            );
         if(iternumber eq 1,
              steplength = omega*(LB)/multiplication2;
            );

         Gradient_curr(products, hubs) = Gradient_curr(products, hubs) + ThetaValue*Gradient_prev(products, hubs);
         phi(products, hubs) = phi(products, hubs) + steplength*Gradient_curr(products, hubs);
         phi(products, hubs) = max(0, phi(products, hubs));
         Gradient_prev(products, hubs) = Gradient_curr(products, hubs) ;

         Gradient_xi1_curr(origins, products) = Gradient_xi1_curr(origins, products) + ThetaValue*Gradient_xi1_prev(origins, products);
         xi1(origins, products) = xi1(origins, products) + steplength*Gradient_xi1_curr(origins, products);
         xi1(origins, products) = max(0, xi1(origins, products));
         Gradient_xi1_prev(origins, products) = Gradient_xi1_curr(origins, products) ;

         Gradient_xi2_curr(destinations, products) = Gradient_xi2_curr(destinations, products) + ThetaValue*Gradient_xi2_prev(destinations, products);
         xi2(destinations, products) = xi2(destinations, products) + steplength*Gradient_xi2_curr(destinations, products);
         xi2(destinations, products) = max(0, xi2(destinations, products));
         Gradient_xi2_prev(destinations, products) = Gradient_xi2_curr(destinations, products) ;

         stepnoIMP = stepnoIMP + 1;
         elapsed = (jnow - starttime)*24*360;
         if (stepnoIMP gt stepnoIMPallowed,
                    omega = omega/omega_decrease_rate ;
             );
    );

totaltime = elapsed ;

parameter LagTime, LagLB, LagUB;
LagTime = totaltime;
LagLB = LB;
LagUB = UB;

LagZS(hubs) = LagZSolTlt(hubs)/lagstep ;
LagYS(origins, products) = LagYSolTlt(origins, products)/lagstep ;
LagUS(destinations, products) = LagUSolTlt(destinations, products)/lagstep ;


********************************************************** Uncapacitated Model ****************************************************
parameter transcost_u(origins, destinations, hubs1, hubs2, products);
transcost_u(origins, destinations, hubs1, hubs2, products)$(ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2)) = transcost(origins, destinations, hubs1, hubs2, products) + ocCost(origins, products) + dcCost(destinations, products) ;

variable totalcost_u;
equations objective_u  ;

objective_u..
       totalcost_u =e= sum((hubs), HubCost(hubs)*z(hubs))
                         + sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost_u(origins, destinations, hubs1, hubs2, products)*x(origins, destinations, products, hubs1, hubs2));

model FHLP_Uncap /objective_u, singleroute, flowhubrel/;
FHLP_Uncap.solprint = 2;
FHLP_Uncap.reslim = 1800;
FHLP_Uncap.optca = 0.000;
FHLP_Uncap.optcr = 0.000;
FHLP_Uncap.iterlim = 4000000;


************************************************************************************************************************
************************************************************************************************************************
**************************************** Benders Decomposition **************************************************
************************************************************************************************************************
************************************************************************************************************************
parameter zsol(hubs), usol(destinations, products), ysol(origins, products);
zsol(hubs) = 0;
usol(destinations, products) = 0;
ysol(origins, products) = 0 ;
set removedhubs(hubs);
removedhubs(hubs) = no;

*---------------------------------------------------------------------
* Benders Decomposition Initialization
*---------------------------------------------------------------------
display "------------------ BENDERS ALGORITHM -----------------------";

*---------------------------------------------------------------------
* Benders Primal Subproblem
*---------------------------------------------------------------------
equations objectiveprimal,
          singlerouteprimal(products),
          flowhubrelprimal(products, hubs),
          originscapprimal(origins, products),
          destinationscapprimal(destinations, products) ;

objectiveprimal..
       totalcost =e= sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost(origins, destinations, hubs1, hubs2, products)*x(origins, destinations, products, hubs1, hubs2));

singlerouteprimal(products)..
      sum((origins, destinations, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =e= 1 ;

flowhubrelprimal(products, hubs)..
      sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs, hubs1) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs, hubs1) )
         + sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ord(hubs1) ne ord(hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs) ) =l= zsol(hubs);

originscapprimal(origins, products)$(ProdOriginsset(products, origins))..
      sum((destinations, hubs1, hubs2)$(ProdOriginsset(products, origins) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =l= CAcap2(origins, products)*ysol(origins, products) ;

destinationscapprimal(destinations, products)$(ProdDestinationsset(products, destinations))..
      sum((origins, hubs1, hubs2)$(ProdDestinationsset(products, destinations) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins)), x(origins, destinations, products, hubs1, hubs2)) =l= CPcap2(destinations, products)*usol(destinations, products) ;

model FHLprimal /objectiveprimal, singlerouteprimal, flowhubrelprimal, originscapprimal, destinationscapprimal/;
FHLprimal.solprint = 2;
FHLprimal.reslim = 3600;
FHLprimal.optca = 0.000;
FHLprimal.optcr = 0.000;
FHLprimal.iterlim = 4000000;
*solve FHLprimal using lp minimizing totalcost;


*---------------------------------------------------------------------
* Benders Dual Subproblem
*---------------------------------------------------------------------
variable dualTC 'objective variable' ;
variables AlphaVar(products) ;
positive variables
ThetaVar(hubs, products)
GammaVar(origins, products)
TauVar(destinations, products)

equations
dualsubprob_obj 'objective'
dualsubprob_constr1(origins, destinations, hubs1, hubs2, products) 'dual constraint 1'
dualsubprob_constr2(origins, destinations, hubs, hubs, products) 'dual constraint 2'
;

dualsubprob_obj.. dualTC =e= sum((products), AlphaVar(products))
                                 - sum((hubs, products)$(not removedhubs(hubs)), zsol(hubs)*ThetaVar(hubs, products))
                                 - sum((origins, products)$ProdOriginsset(products, origins), CAcap2(origins, products)*ysol(origins, products)*GammaVar(origins, products))
                                 - sum((destinations, products)$ProdDestinationsset(products, destinations), CPcap2(destinations, products)*usol(destinations, products)*TauVar(destinations, products));

dualsubprob_constr1(origins, destinations, hubs1, hubs2, products)$(not removedhubs(hubs1) and not removedhubs(hubs2) and hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and ord(hubs1) ne ord(hubs2))..
AlphaVar(products) - ThetaVar(hubs1, products) - ThetaVar(hubs2, products) - GammaVar(origins, products) - TauVar(destinations, products) =l= transcost(origins, destinations, hubs1, hubs2, products) ;

dualsubprob_constr2(origins, destinations, hubs, hubs, products)$(not removedhubs(hubs) and hubspp(origins, destinations, products, hubs, hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations))..
AlphaVar(products) - ThetaVar(hubs, products) - GammaVar(origins, products) - TauVar(destinations, products) =l= transcost(origins, destinations, hubs, hubs, products) ;

model dualsubproblem /dualsubprob_obj, dualsubprob_constr1, dualsubprob_constr2/;
dualsubproblem.solprint=2;
*Keep GAMS in memory
dualsubproblem.solvelink=2;
dualsubproblem.reslim = 3600;
dualsubproblem.optca = 0.000;
dualsubproblem.optcr = 0.000;
dualsubproblem.iterlim = 40000;


***************************************************************************************************************
********************************************** Generate K-means Clusters **************************************
***************************************************************************************************************
parameter prodorimatch(products, origins), proddstmatch(products, destinations);
prodorimatch(products, origins) = 0 ;
proddstmatch(products, destinations) = 0 ;
prodorimatch(products, origins)$ProdOriginsset(products, origins) = 1 ;
proddstmatch(products, destinations)$ProdDestinationsset(products, destinations) = 1 ;

sets
   k  'clusters' /k1*k200/
   product_k(products, k) 'assignment of products to clusters'
   product_k_final(products, k) 'final assignment of products to clusters';
parameter
   numk    'number of clusters'
   cluster(products)    'initial cluster number for each product';

parameter increasenum, stopavgdis;
numk = ceil(card(products)/10);
increasenum = ceil(card(products)/10);
stopavgdis = 0.5 ;

alias(products, productsi);

sets
  product_k_prev(products, k) 'previous assignment of products to clusters'
  product_k_diff
  trial   'number of trials' /trial1*trial1/
  iterclus    'max number of iterations' /iter1*iter50/
;
parameters
  n(k)             'number of points assigned to cluster'
  c(k,origins)     'centroids'
  c2(k,destinations)     'centroids'
  notconverged     '0=converged,else not converged'
  d(products, k)     'squared distance'
  dclose(products)         'distance closest cluster'
  trace(trial) 'reporting'
  distancecap
;
parameter totaldisclus(k), avgdisclus(k), avgdis;
distancecap =inf;
set iterclusdis /1*10/;
parameter avgdis, iterdisnum;
avgdis = 100;
parameter clustermatchdone;
clustermatchdone = 0;
*------------------------------------------------
* Step 1: Random assignment
*------------------------------------------------
loop(iterclusdis$(avgdis ge stopavgdis),
     iterdisnum = ord(iterclusdis);
     distancecap = inf;
     numk = min(numk + increasenum, card(products));
     product_k(products, k) = no;
     product_k_final(products, k) = no;
loop(trial,
         cluster(products)$(ord(products) le numk) = ord(products);
         cluster(products)$(ord(products) gt numk) = uniformint(1,numk);
         product_k(products, k) = cluster(products) = ord(k);
         notConverged = 1;
         loop(iterclus$notConverged,
*------------------------------------------------
* Step 2: Calculate centroids
*------------------------------------------------
              n(k)$(ord(k) le numk) = sum(product_k(products, k), 1);
              c(k,origins)$(n(k) and ord(k) le numk) = sum(product_k(products, k), prodorimatch(products, origins))/n(k);
              c2(k,destinations)$(n(k) and ord(k) le numk) = sum(product_k(products, k), proddstmatch(products, destinations))/n(k);

*------------------------------------------------
* Step 3: Re-assign points
*------------------------------------------------
              product_k_prev(products, k)$(ord(k) le numk) = product_k(products, k);
              d(products, k)$(ord(k) le numk) = sum(origins, sqr(prodorimatch(products, origins) - c(k,origins))) + sum(destinations, sqr(proddstmatch(products, destinations) - c2(k,destinations)));
              dclose(products) = smin(k$(ord(k) le numk), d(products,k));
              loop(products,
                    clustermatchdone = 0;
                    loop(k$(ord(k) le numk and clustermatchdone eq 0),
                         if(dclose(products) eq d(products, k),
                                 product_k(products, k)$(ord(k) le numk) = yes;
                                 clustermatchdone = 1 ;
                            );
                   ););
*------------------------------------------------
* Step 4: Check convergence            )
*------------------------------------------------
              product_k_diff(products, k)$(ord(k) le numk) = product_k(products, k) xor product_k_prev(products, k);
              notConverged = card(product_k_diff);
              trace(trial) = sum(product_k(products, k), d(products, k));
           );
     if(trace(trial) lt distancecap,
              distancecap = trace(trial) ;
              product_k_final(products, k) = product_k(products, k) ;
              totaldisclus(k) = sum(products$product_k(products, k), d(products, k));
              loop(k,
                       if(n(k) gt 1,
                             avgdisclus(k) = sum(products$product_k(products, k), d(products, k))/(n(k)-1);
                           else
                             avgdisclus(k) = 0 ;
                          );
                   );
              avgdis = distancecap/card(products);
        );
    );
);
display product_k_final, numk;
******************************************************************************************************************
***************************************** Generate K-means Clusters : Above **************************************
******************************************************************************************************************

*---------------------------------------------------------------------
* Benders Restricted Master Problem
*---------------------------------------------------------------------
set iter /iter1*iter5000/;
set cutset(iter) 'dynamic set';
cutset(iter)=no;

variables zetavar(k),
          MasterCost 'relaxed master objective variable';

equations
master_obj
cut(iter, k) 'Benders cut for optimal subproblem'
origincon(products)
destincon(products)
HubOpenMstr
;
parameters
cutconst1(iter, products)
cutcoeff1(iter, hubs, products)
cutcoeff2(iter, origins, products)
cutcoeff3(iter, destinations, products)
;
cutconst1(iter, products) = 0;
cutcoeff1(iter, hubs, products) = 0;
cutcoeff2(iter, origins, products) = 0;
cutcoeff3(iter, destinations, products) = 0;


master_obj..  MasterCost =e= sum((k), zetavar(k)) + sum((hubs)$(not removedhubs(hubs)), HubCost(hubs)*z(hubs))
                                 + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                                 + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products)) ;

cut(cutset, k)..
              zetavar(k) =g= sum(products$(product_k_final(products, k)), cutconst1(cutset, products))
                                 - sum((hubs, products)$(product_k_final(products, k) and not removedhubs(hubs)), z(hubs)*cutcoeff1(cutset, hubs, products))
                                 - sum((origins, products)$(ProdOriginsset(products, origins) and product_k_final(products, k)), y(origins, products)*cutcoeff2(cutset, origins, products))
                                 - sum((destinations, products)$(ProdDestinationsset(products, destinations) and product_k_final(products, k)), u(destinations, products)*cutcoeff3(cutset, destinations, products)) ;

origincon(products)..
      sum(origins$(ProdOriginsset(products, origins)), CAcap(origins, products)*y(origins, products)) =g= demands(products) ;

destincon(products)..
      sum(destinations$(ProdDestinationsset(products, destinations)), CPcap(destinations, products)*u(destinations, products)) =g= demands(products) ;

HubOpenMstr..
         sum(hubs$(not removedhubs(hubs)), z(hubs)) =g= 1 ;

model master /master_obj, cut, origincon, destincon, HubOpenMstr/;
* reduce output to listing file:
master.solprint=2;
* speed up by keeping GAMS in memory:
master.solvelink=2;
* solve to optimality
master.optcr=0;
master.reslim = 60;
master.optca = 0.000;
master.optcr = 0.000;
master.iterlim = 4000000;


set selectedtesthubs(hubs);
equations  HubOpenMstr2 ;
set Qhubs(hubs);
Qhubs(hubs) = no;

HubOpenMstr2..
         sum(hubs$(Qhubs(hubs)), z(hubs)) =g= 1 ;

model mastertest /master_obj, cut, origincon, destincon, HubOpenMstr2/;
* reduce output to listing file:
mastertest.solprint=2;
* speed up by keeping GAMS in memory:
mastertest.solvelink=2;
* solve to optimality
mastertest.optcr=0;
mastertest.reslim = 3600;
mastertest.optca = 0.000;
mastertest.optcr = 0.000;
mastertest.iterlim = 4000000;

equations
master_obj_iter1 ;
master_obj_iter1..  MasterCost =e= sum((hubs), HubCost(hubs)*z(hubs))
                                 + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                                 + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products)) ;
model master_iter1 /master_obj_iter1, origincon, destincon, HubOpenMstr/;
* reduce output to listing file:
master_iter1.solprint=2;
* speed up by keeping GAMS in memory:
master_iter1.solvelink=2;
* solve to optimality
master_iter1.optcr=0;
master_iter1.reslim = 60;
master_iter1.optca = 0.000;
master_iter1.optcr = 0.000;
master_iter1.iterlim = 4000000;


*********************************************************************************************************************************************************************************************************
************************************************************* Benders decomposition for Uncapacitated Problem *******************************************************************************************
*********************************************************************************************************************************************************************************************************
*---------------------------------------------------------------------
* Benders Primal Subproblem
*---------------------------------------------------------------------
equations objectiveprimal_Uncap,
          singlerouteprimal_Uncap(products),
          flowhubrelprimal_Uncap(products, hubs),
          Oflowrouterelpriuncap(origins, products),
          Dflowroutereldstuncap(destinations, products) ;

objectiveprimal_Uncap..
       totalcost =e= sum((origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), transcost(origins, destinations, hubs1, hubs2, products)*x(origins, destinations, products, hubs1, hubs2));

singlerouteprimal_Uncap(products)..
      sum((origins, destinations, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =e= 1 ;

flowhubrelprimal_Uncap(products, hubs)..
      sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs, hubs1) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs, hubs1) )
         + sum((origins, destinations, hubs1)$(hubspp(origins, destinations, products, hubs1, hubs) and ord(hubs1) ne ord(hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs) ) =l= zsol(hubs);

Oflowrouterelpriuncap(origins, products)$(ProdOriginsset(products, origins))..
      sum((destinations, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdDestinationsset(products, destinations)), x(origins, destinations, products, hubs1, hubs2)) =l= ysol(origins, products) ;

Dflowroutereldstuncap(destinations, products)$(ProdDestinationsset(products, destinations))..
      sum((origins, hubs1, hubs2)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins)), x(origins, destinations, products, hubs1, hubs2)) =l= usol(destinations, products) ;

model FHLprimal_Uncap /objectiveprimal_Uncap, singlerouteprimal_Uncap, flowhubrelprimal_Uncap, Oflowrouterelpriuncap, Dflowroutereldstuncap/;
FHLprimal_Uncap.solprint = 2;
FHLprimal_Uncap.reslim = 3600;
FHLprimal_Uncap.optca = 0.000;
FHLprimal_Uncap.optcr = 0.000;
FHLprimal_Uncap.iterlim = 4000000;

*---------------------------------------------------------------------
* Benders Dual Subproblem
*---------------------------------------------------------------------
equations
dualsubprob_obj_Uncap 'objective'
dualsubprob_constr1_Uncap(origins, destinations, hubs1, hubs2, products) 'dual constraint 1'
dualsubprob_constr2_Uncap(origins, destinations, hubs, hubs, products) 'dual constraint 2'
;

dualsubprob_obj_Uncap.. dualTC =e= sum((products), AlphaVar(products))
                                 - sum((hubs, products), zsol(hubs)*ThetaVar(hubs, products))
                                 - sum((origins, products)$ProdOriginsset(products, origins), ysol(origins, products)*GammaVar(origins, products))
                                 - sum((destinations, products)$ProdDestinationsset(products, destinations), usol(destinations, products)*TauVar(destinations, products));

dualsubprob_constr1_Uncap(origins, destinations, hubs1, hubs2, products)$(hubspp(origins, destinations, products, hubs1, hubs2) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations) and ord(hubs1) ne ord(hubs2))..
AlphaVar(products) - ThetaVar(hubs1, products) - ThetaVar(hubs2, products) - GammaVar(origins, products) - TauVar(destinations, products) =l= transcost(origins, destinations, hubs1, hubs2, products) ;

dualsubprob_constr2_Uncap(origins, destinations, hubs, hubs, products)$(hubspp(origins, destinations, products, hubs, hubs) and ProdOriginsset(products, origins) and ProdDestinationsset(products, destinations))..
AlphaVar(products) - ThetaVar(hubs, products) - GammaVar(origins, products) - TauVar(destinations, products) =l= transcost(origins, destinations, hubs, hubs, products) ;

model dualsubproblem_Uncap /dualsubprob_obj_Uncap, dualsubprob_constr1_Uncap, dualsubprob_constr2_Uncap/;
dualsubproblem_Uncap.solprint=2;
dualsubproblem_Uncap.solvelink=2;
dualsubproblem_Uncap.reslim = 3600;
dualsubproblem_Uncap.optca = 0.000;
dualsubproblem_Uncap.optcr = 0.000;
dualsubproblem_Uncap.iterlim = 40000;

*---------------------------------------------------------------------
* Benders Restricted Master Problem
*---------------------------------------------------------------------
variables zetavar_uncap(products);

equations
master_obj_uncap
cut_uncap(iter, products)
;

master_obj_uncap..  MasterCost =e= sum((products), zetavar_uncap(products)) + sum((hubs), HubCost(hubs)*z(hubs))+ sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                                 + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products)) ;

cut_uncap(cutset, products)..
              zetavar_uncap(products) =g= cutconst1(cutset, products)
                                 - sum((hubs), z(hubs)*cutcoeff1(cutset, hubs, products))
                                 - sum((origins)$ProdOriginsset(products, origins), y(origins, products)*cutcoeff2(cutset, origins, products))
                                 - sum((destinations)$ProdDestinationsset(products, destinations), u(destinations, products)*cutcoeff3(cutset, destinations, products)) ;

model master_uncap /master_obj_uncap, cut_uncap, origincon, destincon, HubOpenMstr/;
master_uncap.solprint=2;
master_uncap.solvelink=2;
master_uncap.optcr=0;
master_uncap.reslim = 60;
master_uncap.optca = 0.000;
master_uncap.optcr = 0.000;
master_uncap.iterlim = 4000000;

equations
master_obj_uncap_iter1 ;
master_obj_uncap_iter1..  MasterCost =e= sum((hubs), HubCost(hubs)*z(hubs)) + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*y(origins, products))
                                 + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*u(destinations, products)) ;
model master_uncap_iter1 /master_obj_uncap_iter1, origincon, destincon, HubOpenMstr/;
master_uncap_iter1.solprint=2;
master_uncap_iter1.solvelink=2;
master_uncap_iter1.optcr=0;
master_uncap_iter1.reslim = 60;
master_uncap_iter1.optca = 0.000;
master_uncap_iter1.optcr = 0.000;
master_uncap_iter1.iterlim = 4000000;
*********************************************************************************************************************************************************************************************************
************************************************************* Benders decomposition for Uncapacitated Problem *******************************************************************************************
*********************************************************************************************************************************************************************************************************


*---------------------------------------------------------------------
* Benders Algorithm
*---------------------------------------------------------------------
scalar converged /0/;
scalar iteration;
scalar bound;
parameter totalIassCost, totalDassCost, totalhubcost, totaltransportcost;
parameter totalopenhubs;
parameter tempbound;
*parameter select_best(flows);
*parameter hubsopen_best(hubs);
parameter log(iter,*) 'logging info';
parameter iternum;
parameter initstate;
initstate = 0;
parameter mastersoltime, testingtime, uncaptime, subprobtime, Bendcutnum;
mastersoltime = 0;
testingtime = 0;
uncaptime = 0;
subprobtime = 0;
Bendcutnum = 0;

parameter uncap_phase;
uncap_phase = %uncapf%;
parameter LB_uncap, UB_uncap;
LB_uncap = -inf;
UB_uncap = inf;
parameter LPsolz(hubs), LPobj, LPRcstz(hubs);
parameter rankhubRDcstpct(hubs),  rankhubRDcost(hubs) ;
parameter dependvalue(hubs), probequalone(hubs) ;
parameter testdone;
testdone = 0;
parameter zsollogistic(hubs);
parameter probthrehold, decayfac, totaltest, UBLBGap;
probthrehold = 0.001;
UBLBGap = 100;
decayfac = 10;
totaltest = 3;
parameter UBLBGap2;
set testloop /1*100/;
parameter availablehub;
availablehub = 0;
parameter testprobLB;
parameter TestLPRcstz(hubs);
parameter maxTestLPRcstz ;
parameter removedhubsize;


if(LagDone eq 0,
loop(iter$(not converged and totaltime lt maxtime),
iternum = ord(iter);
Bendcutnum = ord(iter);
*
* solve Relaxed Master Problem
*

if(uncap_phase eq 0,
         option optcr=0;
         if(ord(iter) eq 1,
                  solve master_uncap_iter1 minimizing MasterCost using mip;
                  LB_uncap = master_uncap_iter1.ObjEst;
                  totaltime = totaltime + master_uncap_iter1.etSolve;
                  uncaptime = uncaptime + master_uncap_iter1.etSolve;
                  abort$(master_uncap_iter1.modelstat=4) "Relaxed Master is infeasible";
                else
                  solve master_uncap minimizing MasterCost using mip;
                  LB_uncap = master_uncap.ObjEst;
                  totaltime = totaltime + master_uncap.etSolve;
                  uncaptime = uncaptime + master_uncap.etSolve;
                  abort$(master_uncap.modelstat=4) "Relaxed Master is infeasible";
            );
         zsol(hubs) = z.l(hubs) ;
         ysol(origins, products) = y.l(origins, products);
         usol(destinations, products) = u.l(destinations, products);

         if (LB_uncap > LB,
               LB = LB_uncap;
            );

         solve dualsubproblem_Uncap maximizing dualTC using lp;
         display dualTC.l;
         totaltime = totaltime + dualsubproblem_Uncap.etSolve;
         uncaptime = uncaptime + dualsubproblem_Uncap.etSolve;
         abort$(dualsubproblem_Uncap.modelstat>=2) "Dualsubproblem not solved to optimality";
         bound = sum((hubs), HubCost(hubs)*zsol(hubs))
                    + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*ysol(origins, products))
                    + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*usol(destinations, products))
                    + dualTC.l  ;
         if (bound < UB_uncap,
                        UB_uncap = bound;
              );
         cutset(iter) = yes;
         cutconst1(iter, products) = AlphaVar.l(products);
         cutcoeff1(iter, hubs, products) = ThetaVar.l(hubs, products);
         cutcoeff2(iter, origins, products) = GammaVar.l(origins, products);
         cutcoeff3(iter, destinations, products) = TauVar.l(destinations, products);
         if(UB_uncap - LB_uncap lt 0.01,
                uncap_phase = 1;
            );
         display uncap_phase, LB_uncap, UB_uncap;

else
if(ord(iter) le LagScount,
         loop(LagSS$(ord(iter) eq ord(LagSS)),
                  zsol(hubs) = LagZSol(LagSS, hubs) ;
                  ysol(origins, products) = LagYSol(LagSS, origins, products) ;
                  usol(destinations, products) = LagUSol(LagSS, destinations, products) ;
              );
     else
option optcr=0;
if(ord(iter) eq 1,
      solve master_iter1 minimizing MasterCost using mip;
      LB = master_iter1.ObjEst;
      totaltime = totaltime + master_iter1.etSolve;
      mastersoltime = mastersoltime + master_iter1.etSolve;
      abort$(master_iter1.modelstat=4) "Relaxed Master is infeasible";
    else
                   solve master minimizing MasterCost using rmip;
                  totaltime = totaltime + master.etSolve;
                  mastersoltime = mastersoltime + master.etSolve;
                  LPsolz(hubs) = z.l(hubs) ;
                  LPobj = MasterCost.l;
                  LPRcstz(hubs) = z.m(hubs) ;
                  display z.m;
                  loop(hubs$(not removedhubs(hubs) and LPsolz(hubs) eq 0),
                       if(LPobj + LPRcstz(hubs) gt UB,
                               removedhubs(hubs) = yes;
                               z.fx(hubs)$removedhubs(hubs) = 0;
                          );
                      );
                  testdone = 0 ;
                  Qhubs(hubs) = no;
                  if(UB - LB le avaerageHubCost,
                           Qhubs(hubs)$(not removedhubs(hubs)) = yes;
                           availablehub = 0;
                           loop(hubs$(Qhubs(hubs)),
                                    availablehub = 1;
                                );
                           loop(testloop$(testdone eq 0 and availablehub eq 1 and totaltime lt maxtime and testingtime lt maxtime/2),
                                    solve mastertest minimizing MasterCost using rmip;
                                    totaltime = totaltime + mastertest.etSolve;
                                    testingtime = testingtime + mastertest.etSolve;
                                    testprobLB = mastertest.ObjEst;
                                    TestLPRcstz(hubs) = z.m(hubs);
                                    if((UB - testprobLB)/UB gt 0.002,
                                            maxTestLPRcstz = smax(hubs$(Qhubs(hubs)), TestLPRcstz(hubs));
                                            Qhubs(hubs)$(TestLPRcstz(hubs) lt 0.25*maxTestLPRcstz) = no;
                                       );
                                    availablehub = 0;
                                    loop(hubs$(Qhubs(hubs)),
                                            availablehub = 1;
                                         );
                                    if(availablehub eq 1 and testingtime lt maxtime/2,
                                            solve mastertest minimizing MasterCost using mip;
                                            totaltime = totaltime + mastertest.etSolve;
                                            testingtime = testingtime + mastertest.etSolve;
                                            if(mastertest.ObjEst gt UB,
                                                    testdone = 1 ;
                                                    removedhubs(hubs)$Qhubs(hubs) = yes;
                                                    z.fx(hubs)$removedhubs(hubs) = 0;
                                                    else
                                                        Qhubs(hubs)$(Qhubs(hubs) and z.l(hubs) eq 1) = no;
                                                        availablehub = 0;
                                                        loop(hubs$(Qhubs(hubs)),
                                                                availablehub = 1;
                                                             );
                                               );
                                        );
                                );
                      );
                  solve master minimizing MasterCost using mip;
                  LB = master.ObjEst;
                  totaltime = totaltime + master.etSolve;
                  mastersoltime = mastersoltime + master.etSolve;
                  abort$(master.modelstat=4) "Relaxed Master is infeasible";
            );

         zsol(hubs) = z.l(hubs) ;
         ysol(origins, products) = y.l(origins, products);
         usol(destinations, products) = u.l(destinations, products);
       );
*
* solve Benders subproblem
*
solve dualsubproblem maximizing dualTC using lp;
totaltime = totaltime + dualsubproblem.etSolve;
subprobtime = subprobtime + dualsubproblem.etSolve;
abort$(dualsubproblem.modelstat>=2) "Dualsubproblem not solved to optimality";
bound = sum((hubs), HubCost(hubs)*zsol(hubs))
           + sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*ysol(origins, products))
           + sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*usol(destinations, products))
           + dualTC.l  ;

if (bound < UB,
        UB = bound;
        totalhubcost = sum((hubs), HubCost(hubs)*zsol(hubs));
        totalIassCost = sum((origins, products)$ProdOriginsset(products, origins), ocCost(origins, products)*ysol(origins, products));
        totalDassCost = sum((destinations, products)$ProdDestinationsset(products, destinations), dcCost(destinations, products)*usol(destinations, products));
        totaltransportcost = dualTC.l;
        totalopenhubs = sum((hubs), zsol(hubs)) ;
    );
cutset(iter) = yes;

cutconst1(iter, products) = AlphaVar.l(products);
cutcoeff1(iter, hubs, products) = ThetaVar.l(hubs, products);
cutcoeff2(iter, origins, products) = CAcap2(origins, products)*GammaVar.l(origins, products);
cutcoeff3(iter, destinations, products) = CPcap2(destinations, products)*TauVar.l(destinations, products);
);
log(iter,'LB') = LB;
log(iter,'UB') = UB;
iteration = ord(iter);
display iteration,LB,UB;
converged$( (UB-LB) < 0.001 ) = 1;
display$converged "Converged";
);   );

file LagBD_IC_MC_Cordea /LagBD_IC_MC_Cordea.dat/;
put LagBD_IC_MC_Cordea;
LagBD_IC_MC_Cordea.ap = 1;

parameter removedhubnum;
removedhubnum = 0;
loop(hubs$(removedhubs(hubs)),
         removedhubnum = removedhubnum + 1;
      );

put  %ID%, " ", removedhubnum, " ", LagTime, " ", LagLB, " ", LagUB, " ", subprobtime, " ", mastersoltime, " ", testingtime, " ", uncaptime, " ", totaltime, " ", Bendcutnum, " ", totalopenhubs, " ", totalhubcost, " ", totalIassCost, " ", totalDassCost, " ", totaltransportcost, " ", LB, " ", UB  ///


display  removedhubnum, " ", LagTime, " ", LagLB, " ", LagUB, " ", subprobtime, " ", mastersoltime, " ", testingtime, " ", uncaptime, " ", totaltime, " ", Bendcutnum, " ", totalopenhubs, " ", totalhubcost, " ", totalIassCost, " ", totalDassCost, " ", totaltransportcost, " ", LB, " ", UB ;
