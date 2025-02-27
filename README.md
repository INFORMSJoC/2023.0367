[![INFORMS Journal on Computing Logo](https://INFORMSJoC.github.io/logos/INFORMS_Journal_on_Computing_Header.jpg)](https://pubsonline.informs.org/journal/ijoc)

# Machine Learning-empowered Benders Decomposition for Flow Hub Location in E-Commerce

This archive is distributed in association with the [INFORMS Journal on
Computing](https://pubsonline.informs.org/journal/ijoc) under the [MIT License](LICENSE).

The codes, results, and data in this repository are for the research reported on in the paper 
[Machine Learning-empowered Benders Decomposition for Flow Hub Location in E-Commerce](https://doi.org/10.1287/ijoc.2023.0367) by T. Wu, W. Chen, J. F. Cordeau, and R. Jans. 

## Cite

To cite the contents of this repository, please cite both the paper and this repo, using their respective DOIs.

https://doi.org/10.1287/ijoc.2023.0367

https://doi.org/10.1287/ijoc.2023.0367.cd

Below is the BibTex for citing this snapshot of the repository.

```
@misc{LEBDIJOC,
  author =        {T. Wu and W. Chen and J. F. Cordeau and R. Jans},
  publisher =     {INFORMS Journal on Computing},
  title =         {Machine Learning-empowered \textsc{B}enders Decomposition for Flow Hub Location in E-Commerce},
  year =          {2025},
  doi =           {https://doi.org/10.1287/ijoc.2023.0367.cd},
  url =           {https://github.com/INFORMSJoC/2023.0367},
  note =          {Available for download at https://doi.org/10.1287/ijoc.2023.0367},
}  
```

## Description

This github contains three main folders for Data, Code, and Results. The detailed explanation on them is given in the corresponding folders. Specifically,

The "src" folder contains the codes for the six compared methods for Benchmark Testing, including

Cplex_Main.gms                            ===  CPX
FHLAutomaticBD_Main.gms                   ===  ABD
LagBD_IC_MC_Cordea_Main.gms               ===  CET
LagBD_IC_MC_Gita2_Main.gms                ===  TET
LagBD_IC_MC_LEBD_ENH_6.gms                ===  LEBD
SingleHeuristic_Main.gms                  ===  GRD

To run the code/reproduce the results, for each method, one should run the call*.gms file, it then executes the correspoding algorithm code for various test instances and produce the computational results.

In addition, the "src" folder contains three subfolders. The "MachineLearningModels" subfolder contains python files where the logistic regression and random forest models are coded. The python notebooks can be directly run to get the machine learning model results used in the paper.  The "VariousLikelihoodFunctions" and "Impact of the Algorothmic Enhancement" subfolders contain the GAMS files for getting the results related to the performance comparisons of the LEBD algorithm under various learning functions (e.g., logistic regression, random, and random forest) and the results related to various enhancement strategies. The files can be run using the call files and/or using the data saved in the "data" subfolders.


With respect to the "results" folder, the computational results are saved into four subfolders, in which we clearly identified the related Table numbers with which these computational results are associated.  In these result files, the essential columns in Pivot tables are named similarly to the notations given in the paper.

With respect to "data" folder, the data needed for replicating the experiments are saved into each subfolder that corresponds to the test instances in the small-sized, default, SET1, and SET2 data sets. The detailed description of these test instances is given in these subfolders individually. For example, below is the description for the default data set generated for the computational tests of the paper.

**************************************************************************************************************************************
**************** This description is for the default data set generated for the computational tests of the paper titled ***************
*************** "Machine Learning-empowered Benders Decomposition for Flow Hub Location in E-Commerce"    ***************
***************************************************************************************************************************************

This data set contains 120 test instances. The parameter settings of all 120 test instances are given as below. Besides the below presented ones, all other parameters, including (C_o, C_d), C_e, C_w, C_a, C_f, and C_t, are set to the default values.

___________________________
ID	|O|	|D|	|K|	|H|	C_c
___________________________
1	15	15	15	10	1.1
2	15	15	15	10	1.1
3	15	15	15	10	1.1
4	15	15	15	10	1.1
5	15	15	15	10	1.1
6	15	15	15	10	1.1
7	15	15	15	10	1.1
8	15	15	15	10	1.1
9	15	15	15	10	1
10	15	15	15	10	1
11	15	15	15	10	1
12	15	15	15	10	1
13	15	15	15	10	1
14	15	15	15	10	1
15	15	15	15	10	0.7
16	15	15	15	10	0.7
17	15	15	15	10	0.7
18	15	15	15	10	0.7
19	15	15	15	10	0.7
20	15	15	15	10	0.7
21	15	15	15	10	1.1
22	15	15	15	10	1.1
23	15	15	15	10	1
24	15	15	15	10	1
25	15	15	15	10	1
26	15	15	15	10	1
27	15	15	15	10	0.7
28	15	15	15	10	0.7
29	15	15	15	10	0.7
30	15	15	15	10	0.7
31	25	25	25	15	1.1
32	25	25	25	15	1.1
33	25	25	25	15	1.1
34	25	25	25	15	1.1
35	25	25	25	15	1.1
36	25	25	25	15	1.1
37	25	25	25	15	1.1
38	25	25	25	15	1.1
39	25	25	25	15	1.1
40	25	25	25	15	1
41	25	25	25	15	1
42	25	25	25	15	1
43	25	25	25	15	1
44	25	25	25	15	1
45	25	25	25	15	1
46	25	25	25	15	0.7
47	25	25	25	15	0.7
48	25	25	25	15	0.7
49	25	25	25	15	0.7
50	25	25	25	15	0.7
51	25	25	25	15	0.7
52	25	25	25	15	0.7
53	25	25	25	15	1.1
54	25	25	25	15	1
55	25	25	25	15	1
56	25	25	25	15	1
57	25	25	25	15	1
58	25	25	25	15	0.7
59	25	25	25	15	0.7
60	25	25	25	15	0.7
61	40	40	40	25	1.1
62	40	40	40	25	1.1
63	40	40	40	25	1.1
64	40	40	40	25	1.1
65	40	40	40	25	1.1
66	40	40	40	25	1.1
67	40	40	40	25	1
68	40	40	40	25	1
69	40	40	40	25	1
70	40	40	40	25	1
71	40	40	40	25	1
72	40	40	40	25	1
73	40	40	40	25	1
74	40	40	40	25	1
75	40	40	40	25	0.7
76	40	40	40	25	0.7
77	40	40	40	25	0.7
78	40	40	40	25	0.7
79	40	40	40	25	0.7
80	40	40	40	25	0.7
81	40	40	40	25	0.7
82	40	40	40	25	1.1
83	40	40	40	25	1.1
84	40	40	40	25	1.1
85	40	40	40	25	1.1
86	40	40	40	25	1
87	40	40	40	25	1
88	40	40	40	25	0.7
89	40	40	40	25	0.7
90	40	40	40	25	0.7
91	60	60	50	50	1.1
92	60	60	50	50	1.1
93	60	60	50	50	1.1
94	60	60	50	50	1.1
95	60	60	50	50	1.1
96	60	60	50	50	1.1
97	60	60	50	50	1.1
98	60	60	50	50	1.1
99	60	60	50	50	1.1
100	60	60	50	50	1.1
101	60	60	50	50	1
102	60	60	50	50	1
103	60	60	50	50	1
104	60	60	50	50	1
105	60	60	50	50	1
106	60	60	50	50	1
107	60	60	50	50	1
108	60	60	50	50	1
109	60	60	50	50	1
110	60	60	50	50	1
111	60	60	50	50	0.7
112	60	60	50	50	0.7
113	60	60	50	50	0.7
114	60	60	50	50	0.7
115	60	60	50	50	0.7
116	60	60	50	50	0.7
117	60	60	50	50	0.7
118	60	60	50	50	0.7
119	60	60	50	50	0.7
120	60	60	50	50	0.7
___________________________

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


 
