proc import 
datafile='/home/cwilso60/Mixed_models/test2_long.csv' out=data replace;
run;

proc print; 
run;

proc glimmix;
class Time TreatmentNew SubjectID;
model Response_Transformed = TreatmentNew Time  Time*TreatmentNew/ddfm=satterthwaite;
random Time/subject = SubjectID rside type = AR(1) vcorr group = TreatmentNew;
lsmeans TreatmentNew;
lsmeans Time*TreatmentNew;
/*Test if Vehicle is different than the Wild Type (Test A)*/
/* Averaging over all Time points*/
estimate
'Vehicle-Wild Type Avg (A)' TreatmentNew 0 0 0 0 0 0 1 -1,
/* Testing at one given Time point*/
'Vehicle-Wild Type Time Point (A)' TreatmentNew 0 0 0 0 0 0 1 -1 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1
/adjust=simulate adjdfe=row cl;
/*Test if Vehicle is different than the Wild Type (Test D)*/
/* Averaging over all Time points*/
estimate
'Dose 1-Dose 2' TreatmentNew 1 -1 0 0 0 0 0 0,
'Dose 1-Dose 2 Time Point' TreatmentNew 1 -1 0 0 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0 0,
'Dose 1-Dose 3' TreatmentNew 1 0 -1 0 0 0 0 0,
'Dose 1-Dose 3 Time Point' TreatmentNew 1 0 -1 0 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0 0,
'Dose 1-Dose 4' TreatmentNew 1 0 0 -1 0 0 0 0,
'Dose 1-Dose 4 Time Point' TreatmentNew 1 0 0 -1 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0 0,
'Dose 1-Dose 5' TreatmentNew 1 0 0 0 -1 0 0 0,
'Dose 1-Dose 5 Time Point' TreatmentNew 1 0 0 0 -1 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 -1 0 0 0,
'Dose 2-Dose 3' TreatmentNew 0 1 -1 0 0 0 0 0,
'Dose 2-Dose 3 Time Point' TreatmentNew 0 1 -1 0 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0 0,
'Dose 2-Dose 4' TreatmentNew 0 1 0 -1 0 0 0 0,
'Dose 2-Dose 4 Time Point' TreatmentNew 0 1 0 -1 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0 0,
'Dose 2-Dose 5' TreatmentNew 0 1 0 0 -1 0 0 0,
'Dose 2-Dose 5 Time Point' TreatmentNew 0 1 0 0 -1 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 -1 0 0 0,
'Dose 3-Dose 4 Time Point' TreatmentNew 0 0 1 -1 0 0 0 0 ,
'Dose 3-Dose 4 Time Point' TreatmentNew 0 0 1 -1 0 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0 0,
'Dose 3-Dose 5' TreatmentNew 0 0 1 0 -1 0 0 0,
'Dose 3-Dose 5 Time Point' TreatmentNew 0 0 1 0 -1 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 -1 0 0 0,
'Dose 4-Dose 5' TreatmentNew 0 0 0 1 -1 0 0 0,
'Dose 4-Dose 5 Time Point' TreatmentNew 0 0 0 1 -1 0 0 0 Time 0 0 0 0 0 
Time*TreatmentNew 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 0 0
/adjust=simulate adjdfe=row cl;
run;
