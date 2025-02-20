****************************************************************************************************************************************
**************** This description is for the SET2 data set generated for the computational tests of the paper titled ******************
*************** "Machine Learning-empowered Benders Decomposition for Flow Hub Location in E-Commerce"    ******************
****************************************************************************************************************************************

This data set contains 25 test instances. The parameter settings of the 15 test instances with C_f = 1 are given as below. Besides the below presented ones, all other parameters, including (C_o, C_d), C_e, C_w, C_a, and C_t, are set to the default values.
______________________________

ID	      |O|	|D|	|K|	|H|	C_c
______________________________
1001	40	40	40	20	0.7
1002	50	50	50	25	0.7
1003	60	60	60	30	0.7
1004	70	70	70	35	0.7
1005	80	80	80	40	0.7
1006	90	90	90	45	0.7
1007	100	100	100	50	0.7
1008	110	110	110	55	0.7
1009	120	120	120	60	0.7
1010	130	130	130	65	0.7
1011	140	140	140	70	0.7
1012	150	150	150	75	0.7
1013	160	160	160	80	0.7
1014	170	170	170	85	0.7
1015	180	180	180	90	0.7
______________________________

The other 10 test instances have the same parameter values as the test instances with IDs from 1006 to 1015, but with a difference by changing C_f from 1 to 10.

In each data file, the parameter values are given in the below sequence:

1. distance(origins, hubs) - The distance between origins and hubs." 
2. distance(hubs1, hubs2) - The distance between different hubs.
3. distance(hubs, destinations) - The distance between hubs and destinations.
4. Origin Coordinate(origins, X-or-Y) - The origin coordinates 1=X, 2=Y.
5. Hub Coordinate(hubs, X-or-Y) - The hub coordinates 1=X, 2=Y.
6. Destination Coordinate(destinations, X-or-Y) - The destination coordinates 1=X, 2=Y.
7. values(products) - Transportation weights for products.
8. demands(products) - Product demand.
9. Mapping between products and their origin nodes (products, origins).
10. Mapping between products and their destination nodes (products, destinations).
11. ocCost(origins, products) - Origin assignment costs by origins and products.
12. dcCost(destinations, products) - Destination assignment costs by destinations and products.
13. HubCost(hubs) - Hub costs by hubs.
14. CAcap(origins, products) - Origin capacity by origins and products.
15. CPcap(destinations, products) - Destination capacity by destinations and products.

For each parameter, indices are given at first, and parameter values are given subsequently.

************************************************************************************************************************************************************ 
************************************************************ END of DESCRIPTION ****************************************************************************
************************************************************************************************************************************************************ 


 
