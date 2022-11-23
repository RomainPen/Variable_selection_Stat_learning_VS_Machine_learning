/*DGP1 : générer des données indépendantes, ayant pour matrice de variance-covariance la matrice identité */
%macro simu_DGP1(outdata);
proc iml;
call randseed(1);
T=100;
V=I(50);
mean=j(1,ncol(v),0);
/*Début de la boucle ici*/
%do i=1 %to 1000;
X=randnormal(T, mean ,v);
eps=normal(j(nrow(X),1,0))*0.1;
/*Ici vous définissez votre vecteur de paramètres, je prends n'importe quoi*/
beta={1.5 0.9 1 0.1 -0.5};
/*Génération de Y, je ne prends que les 5 premières variables*/
Y=X[,1:5]*beta`+eps;
names='Y'//('X1':'X50')`;
/*exportation des données*/
resu=Y||X;
create table_DGP1&i from resu[colname=names];
append from resu;
close table_DGP1&i;
%end;
%mend;
%simu_DGP1(table_DGP1&i)

proc univariate data=table_DGP11; /*v�rifier la distribution des variables*/
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
Histogram/normal;
run;

/*methode : stat learning*/
%macro regression_DGP1(outdata); /*ici récupérer les résultats de la proc glmselect et stocker les résultats*/
%do i=1 %to 1000;
proc glmselect data=table_DGP1&i OUTDESIGN=testdgp1&i noprint ; /*noprint*/
model y=X1-X50 / selection=lasso (lscoeff choose=Cp) ;/*exporter les résultats et jouer sur les parametres de selection*/ 
%end;
%mend;
%regression_DGP1(outdata);
/* on prend une table de reference*/
data origine_1;
input y intercept x1-x50;
cards;
run;
%macro fusion(); /* on ajoute les r�sultats de "testdgp12" � "testdgp11000" dans la table "origine", afin de cumuler les variable s�lectionn�es*/
options mprint;
%do i=2 %to 1000;
data origine_1;
set origine_1 testdgp1&i;
run;
%end;
%mend;
%fusion();

data datastepwise; /*1 si il y a un nombre, 0 sinon, but est de connaire l� o� les variables sont selectionn�es*/
set origine_1(drop=Y Intercept);
array num(*) _numeric_;
do i=1 to dim(num);
if num(i)=. then
num(i)=0;
else
num(i)=1;
end;
run;



/*NOMBRE DE FOIS D'APPARITION DES VARIABLES SIMPLES*/ /*version_off*/

proc summary data=datastepwise;
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
output out=sommestepwise sum=;
run;
data stepwise;
set sommestepwise;
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

data stepwise;
set stepwise(drop= _TYPE_ _FREQ_ i);
run;

PROC TRANSPOSE DATA=stepwise OUT=Transpose_stepwise prefix=iter;
VAR x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
RUN;

/*Graph_1_off*/
proc template;
define statgraph Graph_1;
dynamic _ITER1A __NAME_;
begingraph;
entrytitle "Percentage of chance to select the unique variable";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.TRANSPOSE_stepwise template=Graph_1;
dynamic _ITER1A="ITER1" __NAME_="'_NAME_'n";
title "DGP1()"; /*title*/
run;



/*NOMBRE DE FOIS D'APPARITION DE plusieur VARIABLES à la fois*/ /*version_off*/ /*pas besoin de d�tailler comme dans le dgp3*/

/*1 à 5*/
data method12345;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15=1;
else var_select15=0;
run; 
data selectionmethod12345;
set method12345(keep=var_select15);
obs = _n_;
run;

/*1 à 6*/
data method123456;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select16=1;
else var_select16=0; 
run; 
data selectionmethod123456;
set method123456(keep=var_select16);
obs = _n_;
run;

/*1 à 7*/
data method1234567;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=0 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select17=1;
else var_select17=0;
run; 
data selectionmethod1234567;
set method1234567(keep=var_select17);
obs = _n_;
run;

/*fusion*/
data selection_method;
merge selectionmethod12345 selectionmethod123456 selectionmethod1234567;
by obs;
run;
/*somme*/
proc summary data=selection_method;
var var_select15 var_select16 var_select17;
output out=proba_method sum=;
run;

/*division par 1000 pour avoir les r�sultats en pourcentage*/
data proba_method;
set proba_method(keep=var_select15 var_select16 var_select17);
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

PROC TRANSPOSE DATA=proba_method OUT=Transpose_proba_method prefix=iter;
VAR var_select15 var_select16 var_select17;
RUN;

/*graph officiel*/
proc template;
define statgraph Graph_2;
dynamic __NAME_  _ITER1A;
begingraph;
entrytitle "Probability of selecting the good model (only X1 to X5)";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A / barlabel=true ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=Transpose_proba_method template=Graph_2 ;
dynamic __NAME_="'_NAME_'n"  _ITER1A="ITER1"; 
run;



















/*************************************************************************************************************************************************/
/*DGP2 : corréler les variables au sein de la regression,*/ 
/*k : pour contrôler la correlation, k=7, correlation maximale Pour réduire la taille de la corrélation, faire tendre k vers de grandes valeurs*/
%macro simu_DGP2(outdata);
proc iml;
call randseed(1);
k=7;
T=100;
c=(1:5)/k; /*avec (1:5)= (1 2 3 4 5)*/
cov=toeplitz(c);
do i=1 to ncol(cov);
cov[i,i]=1;
end;
meana=j(1,ncol(cov),0);
/*Début de la boucle ici*/
%do i=1 %to 1000;
Xa=randnormal(T, meana ,cov);
V=I(45);
meanb=j(1,ncol(v),0);
Xb=randnormal(T, meanb ,v);
X=Xa||Xb;
/*vérification*/
/*cov=X`*X/(nrow(x)-1);
*cor=corr(X);*/
eps=normal(j(nrow(X),1,0))*0.1;
/*Ici vous définissez votre vecteur de paramètres, je prends n'importe quoi*/
beta={1.5 0.9 1 0.1 -0.5};
/*Génération de Y, je ne prends que les 5 premières variables*/
Y=X[,1:5]*beta`+eps;
names='Y'//('X1':'X50')`;
/*exportation des données*/
resu=Y||X;
create table_DGP2&i from resu[colname=names];
append from resu;
close table_DGP2&i ;
%end;
%mend;
%simu_DGP2(table_DGP2&i);

/*methode : without criteria*/
%macro regression_DGP2(outdata); /*ici récupérer les résultats de la proc glmselect et stocker les résultats*/
%do i=1 %to 1000;
proc glmselect data=table_DGP2&i OUTDESIGN=testdgp2&i noprint;
model y=X1-X50 / selection=lar (lscoeff choose=aicc );/*exporter les résultats et jouer sur les parametres de selection*/ 
%end;
%mend;
%regression_DGP2(outdata);
/* on prend une table de reference*/
data origine_2;
input y intercept x1-x50;
cards;
run;
%macro fusion();
options mprint;
%do i=2 %to 1000;
data origine_2;
set origine_2 testdgp2&i;
run;
%end;
%mend;
%fusion();

data datastepwise; /*1 si il y a un nombre, 0 sinon, but est de connaire l� o� les variables sont selectionn�es*/
set origine_2(drop=Y Intercept);
array num(*) _numeric_;
do i=1 to dim(num);
if num(i)=. then num(i)=0;
else
num(i)=1;
end;
run;


/*NOMBRE DE FOIS D'APPARITION DES VARIABLES SIMPLES*/ /*version_off*/

proc summary data=datastepwise;
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
output out=sommestepwise sum=;
run;
data stepwise;
set sommestepwise;
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

data stepwise;
set stepwise(drop= _TYPE_ _FREQ_ i);
run;

PROC TRANSPOSE DATA=stepwise OUT=Transpose_stepwise prefix=iter;
VAR x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
RUN;


/*Graph_1_off*/
proc template;
define statgraph Graph_1;
dynamic _ITER1A __NAME_;
begingraph;
entrytitle "Percentage of chance to select the unique variable ";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.TRANSPOSE_stepwise template=Graph_1;
dynamic _ITER1A="ITER1" __NAME_="'_NAME_'n";
title "DGP2()";
run;



/*NOMBRE DE FOIS D'APPARITION DE plusieur VARIABLES � la fois*/ /*version_off*/

/*x1 to x3*/
data method123;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=1 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select13=1;
else var_select134=0;
run; 
data selectionmethod123;
set method123(keep=var_select13);
obs = _n_;
run;

/*x1 to x5 without x4*/
data method1235;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15_no4=1;
else var_select15_no4=0;
run; 
data selectionmethod1235;
set method1235(keep=var_select15_no4);
obs = _n_;
run;

/*1 � 5*/
data method12345;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15=1;
else var_select15=0;
run; 
data selectionmethod12345;
set method12345(keep=var_select15);
obs = _n_;
run;

/*1 � 6*/
data method123456;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select16=1;
else var_select16=0; 
run; 
data selectionmethod123456;
set method123456(keep=var_select16);
obs = _n_;
run;

/*1 � 7*/
data method1234567;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=0 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select17=1;
else var_select17=0;
run; 
data selectionmethod1234567;
set method1234567(keep=var_select17);
obs = _n_;
run;

/*fusion*/
data selection_method;
merge selectionmethod123 selectionmethod1235 selectionmethod12345 selectionmethod123456 selectionmethod1234567;
by obs;
run;
/*somme*/
proc summary data=selection_method;
var var_select13 var_select15_no4 var_select15 var_select16 var_select17;
output out=proba_method sum=;
run;

/*division par 100*/
data proba_method;
set proba_method(keep= var_select13 var_select15_no4 var_select15 var_select16 var_select17);
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

PROC TRANSPOSE DATA=proba_method OUT=Transpose_proba_method prefix=iter;
VAR var_select13 var_select15_no4 var_select15 var_select16 var_select17;
RUN;


/*graph off*/
proc template;
define statgraph Graph_2;
dynamic __NAME_  _ITER1A;
begingraph;
entrytitle "Probability of selecting the good model (only X1 to X5)";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A / barlabel=true ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=Transpose_proba_method template=Graph_2;
dynamic __NAME_="'_NAME_'n" _ITER1A="ITER1";
run;


















/***************************************************************************************************************************************************/
/*DGP3 : corréler les variables au sein de la base de données*/
%macro simu_DGP3(outdata);
proc iml
%do i=1 %to 1000;
call randseed(1);
v=do(1, 1/50, (-1/50)); /*correlation entre les varibles x */
cov=toeplitz(v); /* matrice de corr entre les variables explicatives x, x1 tres corr avec x1 x2 (les x du début), et faiblement corr avec x50 (les derniers x)*/ /*matrices de corrélation symétriques positives semi-définies*/
meanc=j(1, ncol(v), 0); /*moyenne des x1 d'un echantillon de 100 individus*/
X=randnormal(100, meanc ,cov); /* la matrice X suit une loi N(mean,cov), cette loi s'applique sur les 100 individus(lignes)*/
eps=normal(j(nrow(X),1,0))*0.1;
/*Ici vous définissez votre vecteur de paramètres, je prends n'importe quoi*/
beta={1.5 0.9 1 0.1 -0.5};
/*Génération de Y, je ne prends que les 5 premières variables*/
Y=X[,1:5]*beta`+eps;
names='Y'//('X1':'X50')`;
/*exportation des données*/
resu=Y||X;
create table_DGP3&i from resu[colname=names]; /* create table in work's library from matrix resu */
append from resu;
close table_DGP3&i;
%end;
%mend;
%simu_DGP3(table_DGP3&i); 

/*methode : without criteria*/
%macro regression(outdata); /*ici récupérer les résultats de la proc glmselect et stocker les résultats*/
%do i=1 %to 1000;
proc glmselect data=table_DGP3&i OUTDESIGN=testdgp3&i noprint;
model y=X1-X50 / selection=elasticnet (choose=cp enscale) ;/*exporter les résultats et jouer sur les parametres de selection*/ 
%end;
%mend;
%regression(outdata);
/* on prend une table de reference*/
data origine_3;
input y intercept x1-x50;
cards;
run;
%macro fusion();
options mprint;
%do i=2 %to 1000;
data origine_3;
set origine_3 testdgp3&i;
run;
%end;
%mend;
%fusion();

data datastepwise; /*1 si il y a un nombre, 0 sinon, but est de connaire l� o� les variables sont selectionn�es*/
set origine_3(drop=Y Intercept);
array num(*) _numeric_;
do i=1 to dim(num);
if num(i)=. then
num(i)=0;
else
num(i)=1;
end;
run;


/*NOMBRE DE FOIS D'APPARITION DES VARIABLES SIMPLES*/

proc summary data=datastepwise;
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
output out=sommestepwise sum=;
run;
data stepwise;
set sommestepwise;
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

data stepwise;
set stepwise(drop= _TYPE_ _FREQ_ i);
run;

PROC TRANSPOSE DATA=stepwise OUT=Transpose_stepwise prefix=iter;
VAR x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
RUN;


/*Graph_1_off*/
proc template;
define statgraph Graph_1;
dynamic _ITER1A __NAME_;
begingraph;
entrytitle "Percentage of chance to select the unique variable ";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.TRANSPOSE_stepwise template=Graph_1;
title "DGP3 ()";
dynamic _ITER1A="ITER1" __NAME_="'_NAME_'n";
run;





/*NOMBRE DE FOIS D'APPARITION DE plusieur VARIABLES � la fois*/ /*version_off*/

/*x1 to x3*/
data method123;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=1 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select13=1;
else var_select134=0;
run; 
data selectionmethod123;
set method123(keep=var_select13);
obs = _n_;
run;

/*x1 to x5 without x4*/
data method1235;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15_no4=1;
else var_select15_no4=0;
run; 
data selectionmethod1235;
set method1235(keep=var_select15_no4);
obs = _n_;
run;

/*1 � 5*/
data method12345;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15=1;
else var_select15=0;
run; 
data selectionmethod12345;
set method12345(keep=var_select15);
obs = _n_;
run;

/*1 � 6*/
data method123456;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select16=1;
else var_select16=0; 
run; 
data selectionmethod123456;
set method123456(keep=var_select16);
obs = _n_;
run;

/*1 � 7*/
data method1234567;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=0 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select17=1;
else var_select17=0;
run; 
data selectionmethod1234567;
set method1234567(keep=var_select17);
obs = _n_;
run;

/*fusion*/
data selection_method;
merge selectionmethod123 selectionmethod1235 selectionmethod12345 selectionmethod123456 selectionmethod1234567;
by obs;
run;
/*somme*/
proc summary data=selection_method;
var var_select13 var_select15_no4 var_select15 var_select16 var_select17;
output out=proba_method sum=;
run;

/*division par 100*/
data proba_method;
set proba_method(keep= var_select13 var_select15_no4 var_select15 var_select16 var_select17);
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

PROC TRANSPOSE DATA=proba_method OUT=Transpose_proba_method prefix=iter;
VAR var_select13 var_select15_no4 var_select15 var_select16 var_select17;
RUN;

/*graph off*/
proc template;
define statgraph Graph_2;
dynamic __NAME_  _ITER1A;
begingraph;
entrytitle "Probability of selecting the good model (only X1 to X5)";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A / barlabel=true ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=Transpose_proba_method template=Graph_2;
dynamic __NAME_="'_NAME_'n" _ITER1A="ITER1";
run;












/****************************************************************************************************************************************/
/*DGP4 : pr�sence de variable extr�me*/
%macro simu_DGP4(outdata);
proc iml;
call randseed(1);
T=100;
V=I(50);
mean=j(1,ncol(v),0);
/*Début de la boucle ici*/
%do i=1 %to 1000;
x = j(T, ncol(v));
call randgen(X, "ExtremeValue", Mean, v);
eps=normal(j(nrow(X),1,0))*0.1;
/*Ici vous définissez votre vecteur de paramètres, je prends n'importe quoi*/
beta={1.5 0.9 1 0.1 -0.5};
/*Génération de Y, je ne prends que les 5 premières variables*/
Y=X[,1:5]*beta`+eps;
names='Y'//('X1':'X50')`;
/*exportation des données*/
resu=Y||X;
create table_DGP4&i from resu[colname=names];
append from resu;
close table_DGP4&i;
%end;
%mend;
%simu_DGP4(table_DGP4&i)

proc univariate data=table_DGP41;
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
Histogram/normal;
run;

/*methode : stat learning*/
%macro regression_DGP4(outdata); /*ici récupérer les résultats de la proc glmselect et stocker les résultats*/
%do i=1 %to 1000;
proc glmselect data=table_DGP4&i OUTDESIGN=testdgp4&i noprint ; /*noprint*/
model y=X1-X50 / selection=elasticnet (choose=cp enscale) ;/*exporter les résultats et jouer sur les parametres de selection*/ 
%end;
%mend;
%regression_DGP4(outdata);
/* on prend une table de reference*/
data origine_4;
input y intercept x1-x50;
cards;
run;
%macro fusion(); /* on ajoute les r�sultats de "testdgp12" � "testdgp11000" dans la table "origine", afin de cumuler les variable s�lectionn�es*/
options mprint;
%do i=2 %to 1000;
data origine_4;
set origine_4 testdgp4&i;
run;
%end;
%mend;
%fusion();

data datastepwise; /*1 si il y a un nombre, 0 sinon, but est de connaire l� o� les variables sont selectionn�es*/
set origine_4(drop=Y Intercept);
array num(*) _numeric_;
do i=1 to dim(num);
if num(i)=. then
num(i)=0;
else
num(i)=1;
end;
run;



/*NOMBRE DE FOIS D'APPARITION DES VARIABLES SIMPLES*/

proc summary data=datastepwise;
var x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
output out=sommestepwise sum=;
run;
data stepwise;
set sommestepwise;
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

data stepwise;
set stepwise(drop= _TYPE_ _FREQ_ i);
run;

PROC TRANSPOSE DATA=stepwise OUT=Transpose_stepwise prefix=iter;
VAR x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 
	x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 
	x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50;
RUN;


/*Graph_1_off*/
proc template;
define statgraph Graph_1;
dynamic _ITER1A __NAME_;
begingraph;
entrytitle "Percentage of chance to select the unique variable ";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=WORK.TRANSPOSE_stepwise template=Graph_1;
dynamic _ITER1A="ITER1" __NAME_="'_NAME_'n";
title "DGP4()";
run;



/*NOMBRE DE FOIS D'APPARITION DE plusieur VARIABLES à la fois*/ /*version_off*/

/*x1 to x3*/
data method123;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=1 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select13=1;
else var_select134=0;
run; 
data selectionmethod123;
set method123(keep=var_select13);
obs = _n_;
run;

/*x1 to x5 without x4*/
data method1235;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=1 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15_no4=1;
else var_select15_no4=0;
run; 
data selectionmethod1235;
set method1235(keep=var_select15_no4);
obs = _n_;
run;

/*1 � 5*/
data method12345;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=1 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select15=1;
else var_select15=0;
run; 
data selectionmethod12345;
set method12345(keep=var_select15);
obs = _n_;
run;

/*1 � 6*/
data method123456;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=1 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select16=1;
else var_select16=0; 
run; 
data selectionmethod123456;
set method123456(keep=var_select16);
obs = _n_;
run;

/*1 � 7*/
data method1234567;
set datastepwise;
if x1~=0 & x2~=0 & x3~=0 & x4~=0 & x5~=0 & x6~=0 & x7~=0 & x8~=1 & x9~=1 & x10~=1 & x11~=1 & x12~=1 & x13~=1 & x14~=1 & x15~=1 &
x16~=1 & x17~=1 & x18~=1 & x19~=1 & x20~=1 & x21~=1 & x22~=1 & x23~=1 & x24~=1 & x25~=1 & x26~=1 & x27~=1 & x28~=1 & x29~=1 
& x30~=1 & x31~=1 & x32~=1 & x33~=1 & x34~=1 & x35~=1 & x36~=1 & x37~=1 & x38~=1 & x39~=1 & x40~=1 & x41~=1 & x42~=1 & 
x43~=1 & x44~=1 & x45~=1 & x46~=1 & x47~=1 & x48~=1 & x49~=1 & x50~=1 then var_select17=1;
else var_select17=0;
run; 
data selectionmethod1234567;
set method1234567(keep=var_select17);
obs = _n_;
run;

/*fusion*/
data selection_method;
merge selectionmethod123 selectionmethod1235 selectionmethod12345 selectionmethod123456 selectionmethod1234567;
by obs;
run;
/*somme*/
proc summary data=selection_method;
var var_select13 var_select15_no4 var_select15 var_select16 var_select17;
output out=proba_method sum=;
run;

/*division par 100*/
data proba_method;
set proba_method(keep= var_select13 var_select15_no4 var_select15 var_select16 var_select17);
array div(*) _numeric_;
do i = 1 to dim(div);
div[i] = div[i]/1000;
end;
run;

PROC TRANSPOSE DATA=proba_method OUT=Transpose_proba_method prefix=iter;
VAR var_select13 var_select15_no4 var_select15 var_select16 var_select17;
RUN;


/*graph_off*/
proc template;
define statgraph Graph_2;
dynamic __NAME_  _ITER1A;
begingraph;
entrytitle "Probability of selecting the good model (only X1 to X5)";
layout lattice / rows=1 rowgutter=10;
cell;
layout overlay / xaxisopts=(display=(TICKS TICKVALUES LINE LABEL) 
label="Variables" discreteopts=(tickvaluefitpolicy=splitrotate)) 
yaxisopts=(label="Number of times the variable was selected" labelattrs=(size=10pt) griddisplay=on linearopts = (tickvaluesequence=(start=0 end=100 increment=10) 
viewmin=0 viewmax=100) /*display =(label ticks line tickvalues)*/);
barchart category=__NAME_ response=_ITER1A / barlabel=true ;
endlayout;
endcell;
endlayout;
endgraph;
end;
run;

proc sgrender data=Transpose_proba_method template=Graph_2;
dynamic __NAME_="'_NAME_'n" _ITER1A="ITER1";
run;
















/************************ pour le diab�te**********************************/
proc univariate data=table_DGP41;
run;
