autoLibrary
dim xnp(7),wo_desc$*30
open #h_tmsht:=6: "Name=TMSHT"&wsid$&",KFName=TMSHT-IDX"&wsid$,internal,outIn,keyed 
fnopenprn
do 
	read #h_tmsht,using FM_TIME: mat xnp,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
	FM_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
	pr #255: xnp(1);' had (';b8;') had a time of ';xnp(3);' on ';xnp(6);' charge=';xnp(5)
loop 
TM_XIT2: ! 
fncloseprn
