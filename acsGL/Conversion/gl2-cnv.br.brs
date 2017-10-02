00020   library 'S:\Core\Library': fnxit,fntop,fnindex_it
00030 ! 
00040   dim d$*50,rf(6),bc(12),bp(12),bm(12),ta(2),tr(7),tr$*12,td$*30
00050   let fntop(program$,"CHANGE_ME")
00060   dim k$(3)*25,ss$*11,m(18)
00070   dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,procdat$*20
00080   dim dat$*20,ch$*20,prgl(21),co(7),acctmo$*6,vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,adr(2),revb(13)
00090   open #1: "Name=CNO.H"&wsid$,internal,outin,relative 
00100   pr newpage
00110   pr fields "8,5,C 70,HRB,N": "   WARNING THIS PROGRAM CAN ONLY BE RUN ONE TIME FOR EACH COMPANY #"
00120   pr fields "10,9,C 60": "ENTER THE COMPANY # TO BE CONVERTED OR 0 TO STOP:"
00130 L130: input fields "10,60,N 2,UE,N": cno conv L130
00140   if cno=0 then stop 
00150   pr newpage
00160   pr fields "10,5,C 60": "CONVERSION FOR COMPANY #"&str$(cno)&" IN PROCESS"
00170 ! 
00180   rewrite #1,using L190,rec=1: cno
00190 L190: form pos 1,n 2
00200   close #1: 
00210   open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
00220   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",REPLACE,RecL=338",internal,output 
00230 L230: read #2,using L240: dno,ano,sno,d$,rf(1),rf(3),rf(5),bb,cb,mat bc,mat bp,mat bm,pbp,rf(2),rf(4),rf(6) eof L280
00240 L240: form n 3,n 6,n 3,c 50,3*pd 3,39*pd 6.2,3*pd 3
00250   write #1,using L260: dno,ano,sno,d$,mat rf,bb,cb,mat bc,0,mat bp,0,mat bm,0,pbp,0,0,mat revb
00260 L260: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
00270   goto L230
00280 L280: close #1: : close #2,free: 
00290   open #2: "Name="&env$('Q')&"\GLmstr\ACGLTRAN.h"&str$(cno),internal,input,relative 
00300   open #1: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno)&",REPLACE,RecL=73",internal,outin,relative 
00310   write #1,using L320,rec=1: 0,0,0,0,0,0,0," "," ",lrec(2)
00320 L320: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00330   for j=1 to lrec(2)
00340     read #2,using L320,rec=j: mat tr,tr$,td$ norec L360,conv L360
00350     write #1,using L320: mat tr,tr$,td$,0
00360 L360: next j
00370   close #1: : close #2,free: 
00380   open #1: "Name="&env$('Q')&"\GLmstr\ACGLPGMN.h"&str$(cno)&",REPLACE,RecL=58",internal,outin,relative 
00390   for j=1 to 20
00400     write #1,using L410,rec=j: "","",0,0,0
00410 L410: form pos 1,c 20,c 35,3*n 1
00420   next j
00430   close #1: 
00440   open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&"",internal,outin,relative ioerr L530
00450   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",REPLACE,RecL=190",internal,output 
00460   for j=1 to lrec(2)
00470     read #2,using L480,rec=j: eno,mat k$,ss$,mat m,mat ta eof L520,conv L510,norec L510
00480 L480: form pos 1,n 4,3*c 25,c 11,18*pd 5.2,2*n 5
00490     if eno=0 then goto L510
00500     write #1,using L480: eno,mat k$,ss$,mat m,0,0
00510 L510: next j
00520 L520: close #2,free: 
00530 L530: if lrec(1)=0 then close #1,free: else close #1: 
00540   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno),internal,outin,relative 
00550   read #1,using L560,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$
00560 L560: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30
00570   close #1,free: 
00580   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",SIZE=0,RecL=512",internal,outin,relative 
00590   open #2: "Name="&env$('Q')&"\GLmstr\PRGLNUMB.h"&str$(cno),internal,input ioerr L630
00600   read #2,using L610: mat prgl
00610 L610: form pos 1,21*pd 4
00620   close #2,free: 
00630 L630: open #2: "Name="&env$('Q')&"\GLmstr\GLREC.h"&str$(cno)&"",internal,input ioerr L670
00640   read #2,using L650: reccode
00650 L650: form pos 1,n 1
00660   close #2,free: 
00670 L670: open #2: "Name=S:\acsGL\ACGLCODE.H"&str$(cno)&"",internal,input ioerr L710
00680   read #2,using L690: c1,c2
00690 L690: form pos 1,n 1,n 2
00700   close #2,free: 
00710 L710: open #2: "Name="&env$('Q')&"\GLmstr\ACGLDATE.h"&str$(cno)&"",internal,input 
00720   read #2,using L730: procdat$,dat$,acctmo,acctmo$,ch$,lmu
00730 L730: form pos 1,c 20,c 20,n 2,c 6,c 20,n 2
00740   close #2,free: 
00750   write #1,using L760,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$,c2,c1,procdat$,dat$,acctmo,acctmo$,ch$,lmu,mat prgl,0,reccode,nap,ficarate,ficawage,feducrat,feducwag
00760 L760: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30,n 2,n 1,2*c 20,n 2,c 6,c 20,n 2,21*pd 4,2*n 1,n 2,pd 5.3,pd 5.2,pd 5.3,pd 5.2
00770   close #1: 
00780   open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",NoShr",internal,input ioerr L800
00790   close #1,free: 
00800 L800: open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",size=0,RecL=68,Shru",internal,outin,relative 
00810   close #1: 
00820   gosub L890 ! CONVERT VENDOR FILES
00830   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&" 1 12 REPLACE DupKeys"
00840   execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 REPLACE DupKeys"
00850   fnindex_it(env$('Q')&"\GLmstr\GLBREC.h"&env$('cno'),env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno'),"1 24")
00860   execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&" 1 4 REPLACE DupKeys" ioerr L870
00870 L870: execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 REPLACE DupKeys"
00880   goto L1130
00890 L890: open #3: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed 
00900   open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno),internal,outin,relative ioerr L920
00910   close #2,free: 
00920 L920: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",SIZE=0,RecL=64,NoShr",internal,outin,relative 
00930   write #2,using L970,rec=1: "",0,0,"","",1
00940   open #1: "Name="&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed ioerr L960
00950   close #1,free: 
00960 L960: open #1: "Name="&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&",size=0,RecL=127,NoShr",internal,outin,relative 
00970 L970: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
00980 L980: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
00990 L990: read #3,using L980: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ eof L1070
01000   mat adr=(0)
01010   if ytdp=0 then goto L1050
01020   let rec2=lrec(2)+1
01030   write #2,using L970,rec=rec2: vn$,dat,ytdp,"","BEGINNING BALANCE",0
01040   mat adr=(rec2)
01050 L1050: write #1,using L980: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
01060   goto L990
01070 L1070: close #1: ioerr L1080
01080 L1080: close #2: ioerr L1090
01090 L1090: close #3: ioerr L1100
01100 L1100: execute "Copy "&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&' '&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)
01110   execute "Free "&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)
01120   return 
01130 L1130: ! REASS.CNV
01140   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
01150   open #2: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno),internal,outin,relative 
01160   pr newpage
01170   pr fields "10,15,c 60,h,n": "REASSIGN GL ADDRESSES IN PROCESS"
01180 L1180: form pos 333,2*pd 3
01190   let lr2=lrec(2)
01200   rewrite #2,using L1300,rec=1: lr2
01210   for j=1 to lr2
01220     read #2,using L1230,rec=j: k$,nta norec L1310
01230 L1230: form pos 1,c 12,pos 71,pd 3
01240     read #1,using L1180,key=k$: mat ta nokey L1310
01250     if ta(1)=0 then let ta(1)=j
01260     if ta(2)>0 then rewrite #2,using L1300,rec=ta(2): j
01270     let ta(2)=j
01280     rewrite #1,using L1180,key=k$: mat ta
01290     rewrite #2,using L1300,rec=j: 0
01300 L1300: form pos 71,pd 3
01310 L1310: next j
01320   close #1: 
01330   close #2: 
01340   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),internal,outin,keyed ioerr L1540
01350   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno),internal,outin,relative ioerr L1540
01360   pr newpage
01370   pr fields "10,15,c 60,h,n": "REASSIGN PR ADDRESSES IN PROCESS"
01380 L1380: form pos 181,2*n 5
01390   let lr2=lrec(2)
01400   rewrite #2,using L1500,rec=1: lr2
01410   for j=1 to lr2
01420     read #2,using L1430,rec=j: en$,nta norec L1510,conv L1510
01430 L1430: form pos 1,c 4,pos 68,pd 3
01440     read #1,using L1380,key=en$: mat ta nokey L1510,conv L1510
01450     if ta(1)=0 then let ta(1)=j
01460     if ta(2)>0 then rewrite #2,using L1500,rec=ta(2): j
01470     let ta(2)=j
01480     rewrite #1,using L1380,key=en$: mat ta
01490     rewrite #2,using L1500,rec=j: 0
01500 L1500: form pos 68,pd 3
01510 L1510: next j
01520   close #1: 
01530   close #2: 
01540 L1540: ! 
01550   open #1: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno),internal,input,relative ioerr L1690
01560   pr fields "14,32,C 16,BR,N": "   IN PROCESS"
01570   open #2: "Name=X,RecL=72,REPLACE",internal,output 
01580   for j=1 to lrec(1)
01590     read #1,using L1620,rec=j: mat tr,tr$,td$ conv L1640
01600     if tr(1)+tr(2)+tr(3)=0 then goto L1640
01610     actpd=int(tr(4)*.0001)
01620 L1620: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
01630     write #2,using L1620: mat tr,tr$,td$,actpd
01640 L1640: next j
01650   close #1,free: 
01660   close #2: 
01670   execute "Rename X "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)
01680   execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)&' '&env$('Q')&"\GLmstr\AcTrIdx.h"&str$(cno)&" 1/71/17/13 12/2/2/4 REPLACE DupKeys"
01690 L1690: ! S:\acsGL\PRmstr.CNV
01700   dim pr1$*90,pr1(18),pr2(36)
01710   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),internal,outin,keyed ioerr L1910
01720   open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,REPLACE",internal,output 
01730 L1730: read #1,using L1740: pr1$,mat pr1 eof END1
01740 L1740: form pos 1,c 90,18*pd 5.2,2*n 5
01750   for j=1 to 11: let pr2(j)=pr1(j): next j
01760   let pr2(13)=pr1(12)
01770   for j=13 to 18: let pr2(j+18)=pr1(j): next j
01780   write #2,using L1790: pr1$,mat pr2
01790 L1790: form pos 1,c 90,36*pd 5.2,2*n 5
01800   goto L1730
01810 END1: close #1: 
01820   close #2: 
01830   execute "COPY "&env$('Temp')&"\Work."&session$&", "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&""
01840   execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&","&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",1,4,REPLACE,DupKeys"
01850   open #1: "Name=CNO.H"&wsid$,internal,outin,relative 
01860   rewrite #1,using L1870,rec=1: cno
01870 L1870: form pos 1,n 2
01880   close #1: 
01890   open #1: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",RecL=110,REPLACE",internal,output 
01900   close #1: 
01910 L1910: open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno),internal,outin,relative ioerr L1960
01920   read #1,using L1930,rec=1: gl1$,gl2$
01930 L1930: form pos 298,2*c 12
01940   rewrite #1,using L1930,rec=1: gl2$,gl1$
01950   close #1: 
01960 L1960: let end1=0 ! 
01970   dim id$(6)*40,fil$(6),idx$(6)
01980   let id$(1)=" 1 = BALANCE SHEET FILE": let fil$(1)="ACGLFNSB": let idx$(1)="FNSBINDX"
01990   let id$(2)=" 2 = INCOME STATEMENT FILE": let fil$(2)="ACGLFNSI": let idx$(2)="FNSIINDX"
02000   let id$(3)=" 3 = FUND STMT / CASH FLOW FILE": let fil$(3)="ACGLFNSF": let idx$(3)="FNSFINDX"
02010   let id$(4)=" 4 = SECONDARY BALANCE SHEET FILE": let fil$(4)="ACGLFNSC": let idx$(4)="FNSCINDX"
02020   let id$(5)=" 5 = SECONDARY INCOME STATEMENT FILE": let fil$(5)="ACGLFNSJ": let idx$(5)="FNSJINDX"
02030   let id$(6)=" 6 = SECONDARY FUND / CASH FLOW FILE": let fil$(6)="ACGLFNSG": let idx$(6)="FNSGINDX"
02040   for j=1 to 6
02050     execute "Copy "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&" "&env$('Temp')&"\Work."&session$&" -83" ioerr L2300
02060     execute "COPY  "&env$('Temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)
02070     if j=2 or j=5 then goto L2080 else goto L2300
02080 L2080: open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(j)&".h"&str$(cno),internal,outin,keyed 
02090     let end1=st1=st2=rno=rnp=0
02100 L2100: gosub FIND1
02110     restore #1,key>=lpad$(str$(st1),5): nokey END2
02120 L2120: read #1,using L2220: rno,ic eof END2
02130     if rno<st2 then goto L2170
02140     if end1=1 then goto END2
02150     let rnp=0
02160     goto L2100
02170 L2170: rewrite #1,using L2180: rnp
02180 L2180: form pos 79,n 5
02190     goto L2120
02200 FIND1: let st1=rno : let st2=99999 : let rnp=0
02210 L2210: read #1,using L2220: rno,ic eof END21
02220 L2220: form pos 1,g 5,pos 75,n 1
02230     if ic=0 then goto L2210
02240     if ic=1 then let rnp=rno
02250     if ic=2 then let st2=rno : goto L2280
02260     goto L2210
02270 END21: let end1=1
02280 L2280: return 
02290 END2: close #1: 
02300 L2300: next j
02310   chain "S:\acsGL\Company"
