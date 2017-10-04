20200   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fndat,fngethandle
20600   dim sys_name$(40)*55,g$*5,b(8),iv$*12,s(10),dat$*20,skln$*80 ! co$(4)*40,
20800   dim inv_item$(10)*55,client_addr$(3)*30,ct(10),sc(10),gl$(10)*12,cap$*128
21000   dim in1$(3)
21200   dim inp(7),wo_desc$*30
21400   open #h_tmsht:=6: "Name=TMSHT"&wsid$&",KFName=TMSHT-IDX"&wsid$,internal,outin,keyed 
21600   fnopenprn(cp,42,220,process)
21800   do 
22000     read #h_tmsht,using FM_TIME: mat inp,b6,b7,b8,sc,o_o,wo_desc$ eof TM_XIT2
22200 FM_TIME: form pos 1,g 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,x 12,pd 3,c 30
22400     pr #255: inp(1);' had (';b8;') had a time of ';inp(3);' on ';inp(6);' charge=';inp(5)
22600   loop 
22800 TM_XIT2: ! 
23000   fncloseprn
