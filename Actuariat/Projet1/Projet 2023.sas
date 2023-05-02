libname tabs "/home/u63328955/projet 2023/tabs" ;
run;

/* IMPORTATION DE SINISTRE  */

FILENAME REFFILE '/home/u63328955/projet 2023/sinistre.csv' encoding="LATIN2";

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV 
	OUT=tabs.sinistre replace;
	GETNAMES=YES;
RUN;

proc print data=tabs.sinistre(obs=10);
run;

/* Analyse des valeurs manquants : */; 
proc means data=Tabs.sinistre NMISS;
run;

/* Élimination des doublons :  */; 
proc sql;
	create table tab_test as 
	select n_sinistre, count(*) as repetition from tabs.sinistre 
	group by n_sinistre
	order by repetition desc;
quit;

proc print data=tab_test(obs=10);
run;

/* Élimination des montants négatifs */;
proc sql;
	create table tab_test as 
	select n_sinistre, montantsinistre from tabs.sinistre 
	order by montantsinistre asc ;
quit;

/* Manipulations */

proc sql; 
	create table tabs.sinistresT as 
	select numepolice ,annee_reference as exercice, count(*) as nombre , sum(montantsinistre) as somme
	from tabs.sinistre 
	group by annee_reference, numepolice;
quit;

proc print data = tabs.sinistresT(obs=10);
run;

proc univariate data=tabs.sinistre;
var montantsinistre;
output out=quantiles pctlpre=p_ 
       pctlpts=5 25 50 75 95 97 99 pctlname=percentile;
run;

proc sql; 
	create table tabs.sinistres as 
	select numepolice ,annee_reference as exercice, count(*) as nombre , sum(montantsinistre) as somme
	from tabs.sinistre 
	where montantsinistre< 80000
	group by annee_reference, numepolice;
quit;

proc print data = tabs.sinistres(obs=10);
run;

/* IMPORTATION DE PRODUCTION */

FILENAME REFFILE '/home/u63328955/projet 2023/production.csv' encoding="LATIN2";

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV 
	OUT=tabs.production replace;
	GETNAMES=YES;
RUN;

proc print data=tabs.production(obs=10);
run; 

/* Analyse des valeurs manquants : */; 
proc means data=tabs.production NMISS;
run;

/* Élimination des doublons :  */; 
proc sql;
	create table tab_test as 
	select numepolice,exercice, count(*) as repetition from tabs.production 
	group by numepolice,exercice
	order by repetition desc;
quit;
proc print data=tab_test(obs=10);
run;

/* Manipulation */;
proc sql; 
	alter table tabs.production add ageV int; 
	alter table tabs.production add ageC int; 
	
	update tabs.production 
	set ageV = exercice - year(DMC), ageC = exercice - year(DOB);
	alter table tabs.production drop DOB;
	alter table tabs.production drop DMC;
	alter table tabs.production drop var1;
quit;

/* Netoyage  */
proc sql;
	select count(*)  as nombre_des_ages_inferieur_à_18
	from tabs.production 
	where ageV<0 or ageC<17.5 ;
quit;	
	
	
/* Merging data */
proc sql;
create table tabs.data as
SELECT  p.* , s.nombre , s.somme
FROM tabs.production AS p
LEFT JOIN tabs.sinistres AS s
ON p.numepolice = s.numepolice and p.exercice = s.exercice;

alter table tabs.data drop var1; 

update tabs.data 
	set somme=0,nombre=0
	where somme =.;
quit;

proc print data=tabs.data(obs=20);
run;

/*  severité + frequence */ 

proc sql;

alter table tabs.data add frequence float(4); 
alter table tabs.data add severity float(4); 

update tabs.data 
	set frequence = nombre/exposition,severity = somme/nombre;
	
update tabs.data 
	set severity = 0
	where severity = .;
quit;


/*  DESCRIPTIVE STATISTIC */;



/* univariate */

proc sgplot data=tabs.data;	vbar sexe / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar pf / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar comubsution / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar zone/ stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar ageC / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar ageV / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	vbar nombre / stat=percent fillattrs=(color=CX42bfa5);run;
proc sgplot data=tabs.data;	histogram somme /fillattrs=(color=CX42bfa5) ;run;


proc sgplot data=tabs.data;
	histogram frequence/   fillattrs=(color=CX42bfa5);
run;

proc sgplot data=tabs.data;
	histogram severity /  fillattrs=(color=CX42bfa5);
run;


/* MAcro to do that */;
%macro plot_stats( cat_var=, quant_var=);

proc sql;
create table amd as
select &cat_var, mean(&quant_var) as mean_sin, sqrt(var(&quant_var))/5 as var_sin
from tabs.data
group by &cat_var
order by mean_sin;
quit;

/* Print table */
proc print data=amd;
run;

/* Create bar chart */
proc sgplot data=amd;
vbar &cat_var / response=mean_sin barwidth=1 fillattrs=(color=CX42bfa5);
vbar &cat_var / response=var_sin barwidth=0.5 fillattrs=(color=CX5d6a74);
run;

%mend;
/* ===================== */

/* EFFET ZONE */
%plot_stats(cat_var=zone, quant_var=nombre);
%plot_stats(cat_var=zone, quant_var=severity);

proc sql;
	alter table tabs.data add newzone char(1000) ; 
	update tabs.data set newzone = zone; 
	update tabs.data 
	set newzone = "zone_low_risk" 
	where zone in ("Tanger-Tétouan-Hoceďma","Laâyoune-Sakia El Hamra","Dakhla-Oued Ed Dahab","Marrakech-Safi");
	
	update tabs.data 
	set newzone = "zone_high_risk" 
	where newzone <> "zone_low_risk" ;
quit;


proc sql;
	alter table tabs.data add newszone char(1000) ; 
	update tabs.data set newszone = zone; 

	update tabs.data 
	set newszone = "zone_low_risk" 
	where zone in ("Laâyoune-Sakia El Hamra","Marrakech-Safi","Dakhla-Oued Ed Dahab","Souss-Massa","Tanger-Tétouan-Hoceďma","Drâa-Tafilalet","BéniMellal-Khénifra","Casablanca-Settat");
	
	update tabs.data 
	set newszone = "zone_high_risk" 
	where newszone <> "zone_low_risk" ;
quit;


/* EFFET AGE C */
%plot_stats(cat_var=ageC, quant_var=nombre);
%plot_stats(cat_var=ageC, quant_var=severity);


proc sql;
	alter table tabs.data add ageCnew char(100) ;
	update tabs.data set ageCnew ="[18-30] U [56-oo]" where ageC between 18 and 28;
	update tabs.data set ageCnew ="[29-50]" where ageC between 29 and 55;
	update tabs.data set ageCnew ="[18-30] U [56-oo]" where ageC >= 56;
quit; 	

%plot_stats(cat_var=ageCnew, quant_var=nombre);
%plot_stats(cat_var=ageCnew, quant_var=severity);

/* EFFET AGE  V */
%plot_stats(cat_var=ageV, quant_var=nombre);
%plot_stats(cat_var=ageV, quant_var=severity);


/* EFFET COMUBSUTION */
%plot_stats(cat_var=comubsution, quant_var=nombre);
%plot_stats(cat_var=comubsution, quant_var=severity);


/* EFFET Sexe*/
%plot_stats(cat_var=sexe, quant_var=nombre);
%plot_stats(cat_var=sexe, quant_var=severity);


/* EFFET DE PF */
%plot_stats(cat_var=PF, quant_var=nombre);
%plot_stats(cat_var=PF, quant_var=severity);

proc sql;
	alter table tabs.data add newPF char(1000) ; 
	update tabs.data set newPF = PF; 
	update tabs.data 
	set newPF = "PF_low_risk" 
	where PF in ("[10-14]");	
	update tabs.data 
	set newPF = "PF_high_risk" 
	where newPF <> "PF_low_risk" ;
quit;

proc sql;
	alter table tabs.data add newsPF char(1000) ; 
	update tabs.data set newsPF = PF; 
	update tabs.data 
	set newsPF = "PF_low_risk" 
	where PF in ("[00-08]");	
	update tabs.data 
	set newsPF = "PF_high_risk" 
	where newsPF <> "PF_low_risk" ;
quit;


%plot_stats(cat_var=newPF, quant_var=nombre);
%plot_stats(cat_var=newPF, quant_var=severity);

/* EFFET DE SEXE */
%plot_stats(cat_var=sexe, quant_var=nombre);
%plot_stats(cat_var=sexe, quant_var=severity);


/* MODELISATION de nombre */

/* Poisson  */
proc sql;
	alter table tabs.data add logexp float(4) ; 
	update tabs.data set logexp = log(exposition);
quit;

proc genmod data=tabs.data;
Class sexe zone   comubsution  PF  ;
Model nombre =  sexe comubsution PF zone  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 1";
ods output modelfit = pois1;
run;

proc genmod data=tabs.data;
Class sexe zone   comubsution  newPF  ;
Model nombre =  sexe comubsution newPF zone  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 2";
ods output modelfit = pois2;
run;

proc genmod data=tabs.data;
Class sexe newzone   comubsution  newPF  ;
Model nombre =  sexe comubsution newPF newzone  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 3";
ods output modelfit = pois3;
run;


proc genmod data=tabs.data;
Class sexe newzone   comubsution  newPF  ;
Model nombre =  sexe comubsution newPF*newzone  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 4";
ods output modelfit = pois4;
run;

proc genmod data=tabs.data;
Class sexe newzone   comubsution agecnew newPF  ;
Model nombre =  sexe comubsution newPF*newzone  ageCnew ageV/
dist = pois link = log offset=logexp ;
title "Poisson 5";
ods output modelfit = pois5;
run;


proc genmod data=tabs.data;
Class newzone   comubsution  newPF  ;
Model nombre =  comubsution newzone*newpf  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 6";
ods output modelfit = pois6;
run;

title "COmparaison "; 

proc print data = pois4;run;
proc print data=pois6;run;

/* ZIP */
proc sql;
	alter table tabs.data add isnull int;
	update tabs.data set isnull = 0; 
	update tabs.data set isnull = 1 where nombre > 0; 
quit;

%plot_stats(cat_var=newPF, quant_var=isnull);
%plot_stats(cat_var=newzone, quant_var=isnull);
%plot_stats(cat_var=comubsution, quant_var=isnull);
%plot_stats(cat_var=ageC, quant_var=isnull);
%plot_stats(cat_var=ageV, quant_var=isnull);
%plot_stats(cat_var=sexe, quant_var=isnull);


proc genmod data=tabs.data;
Class newzone   comubsution  newPF  ;
Model nombre =  comubsution newzone*newpf  ageC ageV/
dist = zip link = log offset=logexp ;
zeromodel  / link = logit  ;
title "ZIP 4";
ods output modelfit = zip1;
run;


proc genmod data=tabs.data;
Class newzone   comubsution  newPF  ;
Model nombre =  comubsution newzone*newpf  ageC ageV/
dist = zip link = log offset=logexp ;
zeromodel  / link = logit  ;
title "ZIP 6";
ods output modelfit = zip1;
run;




/* Comparaison :  */













/* =====================================================================  */

/* Modélisation de séverité :  */


proc genmod data=tabs.data;
Class sexe zone Comubsution PF;
Model severity = sexe zone Comubsution PF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam1;
run;

proc genmod data=tabs.data;
Class sexe zone Comubsution newPF;
Model severity = sexe zone Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam2;
run;


proc genmod data=tabs.data;
Class sexe newszone Comubsution newPF;
Model severity = sexe newszone Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam3;
run;

proc genmod data=tabs.data;
Class sexe newszone Comubsution newPF;
Model severity = newszone sexe* Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam4;
run;

proc genmod data=tabs.data;
Class newszone Comubsution newPF;
Model severity = newszone Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam5;
run;

/* lognormal  */
proc sql;
	alter table tabs.data add logSev float(4);
	update tabs.data set logsev = log(severity);
quit;


/* Model tout variable */
proc genmod data=tabs.data;
Class sexe zone Comubsution PF;
Model logsev = sexe zone Comubsution PF ageC ageV/
dist = normal link = identity offset=logexp ;
ods output modelfit = LGN1;
run;

/* Model avec decomposition pf */
proc genmod data=tabs.data;
Class sexe zone Comubsution newsPF;
Model logsev = sexe zone Comubsution newsPF ageC ageV/
dist = normal link = identity offset=logexp ;
ods output modelfit = LGN2;
run;


/* ne se regle pas , on elimine newspf  */
proc genmod data=tabs.data;
Class sexe newzone Comubsution ;
Model logsev = sexe newzone Comubsution  ageC ageV/
dist = normal link = identity offset=logexp ;
ods output modelfit = LGN3;
run;

/* On élimine newszone */
proc genmod data=tabs.data;
Class sexe  Comubsution ;
Model logsev = sexe  Comubsution  ageC ageV/
dist = normal link = identity offset=logexp ;
ods output modelfit = LGN4;
run;

/* On elimine sexe */
proc genmod data=tabs.data;
Class newzone Comubsution ;
Model logsev =  newzone Comubsution  ageC ageV/
dist = normal link = identity offset=logexp ;
ods output modelfit = LGN3;
run;


proc print data=gam4;run;
proc print data=gam5;run;




/*  ============================================= */
/* Calcul des primes */

/* frequency models keept */; 


proc genmod data=tabs.data;
Class sexe newzone   comubsution  newPF  ;
Model nombre =  sexe comubsution newPF*newzone  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 4";
ods output modelfit = pois4;
output out=tabs.tarif4 p=fitted_frequence;
run;

proc sql;
	update tabs.tarif4 set fitted_frequence = fitted_frequence/exposition;
quit; 


proc genmod data=tabs.data;
Class newzone   comubsution  newPF  ;
Model nombre =  comubsution newzone*newpf  ageC ageV/
dist = pois link = log offset=logexp ;
title "Poisson 6";
ods output modelfit = pois6;
output out=tabs.tarif6 p=fitted_frequence; 
run;

proc sql;
	update tabs.tarif6 set fitted_frequence = fitted_frequence/exposition;
quit; 


proc sql;
CREATE TABLE amd (
  nombre_total INT ,
  nombre_tarif4 INT,
  nombre_tarif6 INT
);

INSERT INTO amd (nombre_total, nombre_tarif4, nombre_tarif6) VALUES
  (0, 0, 0); 
  
UPDATE amd set 
nombre_total = (select sum(nombre) from tabs.sinistres),
nombre_tarif4 = (select sum(fitted_frequence*exposition) from tabs.tarif4),
nombre_tarif6 = (select sum(fitted_frequence*exposition) from tabs.tarif6);
quit;

proc print data=amd;run;



/* severity Model keept */

proc genmod data=tabs.data;
Class sexe newszone Comubsution newPF;
Model severity = newszone sexe* Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam4;
output out=sev4 p=fitted_severity;
run;

proc genmod data=tabs.data;
Class newszone Comubsution newPF;
Model severity = newszone Comubsution newPF ageC ageV/
dist = gamma link = log offset=logexp ;
ods output modelfit = Gam5;
output out=sev5 p=fitted_severity;
run;


/* primum calcul */

proc sql;
	create table tarif44 as 
	select p4.*,s4.fitted_severity, s4.fitted_severity*p4.fitted_frequence as prime_stand
	from tabs.tarif4 as p4
	inner join sev4 as s4
	on p4.numepolice = s4.numepolice and p4.exercice = s4.exercice;
quit; 


proc sql;
	create table tarif45 as 
	select p4.*,s5.fitted_severity, s5.fitted_severity*p4.fitted_frequence as prime_stand
	from tabs.tarif4 as p4
	inner join sev5 as s5
	on p4.numepolice = s5.numepolice and p4.exercice = s5.exercice;
quit; 


proc sql;
	create table tarif64 as 
	select p6.*,s4.fitted_severity, s4.fitted_severity*p6.fitted_frequence as prime_stand
	from tabs.tarif6 as p6
	inner join sev4 as s4
	on p6.numepolice = s4.numepolice and p6.exercice = s4.exercice;
quit; 





proc sql;
	create table tarif65 as 
	select p6.*,s5.fitted_severity, s5.fitted_severity*p6.fitted_frequence as prime_stand
	from tabs.tarif6 as p6
	inner join sev5 as s5
	on p6.numepolice = s5.numepolice and p6.exercice = s5.exercice;
quit; 


proc print data=tarif44(obs=5);run;
proc print data=tarif45(obs=5);run;
proc print data=tarif64(obs=5);run;
proc print data=tarif65(obs=5);run;


proc sql;
CREATE TABLE cotisation (cot float(4));

INSERT INTO cotisation (cot) VALUES (0); 
  
UPDATE cotisation set 
cot = (select mean(montantsinistre)*count(*) from tabs.sinistre where montantsinistre > 80000)/(select count(*) from tabs.sinistre);
quit;

proc print data=cotisation;run;

proc sql;
alter table tarif44 add prime_pure float(4);
update tarif44 set prime_pure = prime_stand + (select cot from cotisation);
alter table tarif45 add prime_pure float(4);
update tarif45 set prime_pure = prime_stand + (select cot from cotisation) ;
alter table tarif64 add prime_pure float(4);
update tarif64 set prime_pure = prime_stand + (select cot from cotisation) ;
alter table tarif65 add prime_pure float(4);
update tarif65 set prime_pure = prime_stand + (select cot from cotisation) ;
quit;



proc sql;
CREATE TABLE amd (
charges_totaux float(5) , tarifs44  float(5), tarifs45 float(5) , tarifs64  float(5), tarifs65 float(5)
);

INSERT INTO amd (charges_totaux, tarifs44, tarifs45, tarifs64, tarifs65) VALUES
  (0, 0, 0, 0, 0); 
  
UPDATE amd set 
charges_totaux = ( select sum(montantsinistre) from tabs.sinistre),
tarifs44 = (select sum(prime_pure) from tarif44), 
tarifs45 = (select sum(prime_pure) from tarif45),
tarifs64 = (select sum(prime_pure) from tarif64),
tarifs65 = (select sum(prime_pure) from tarif65);
quit;


proc print data=amd;run;




