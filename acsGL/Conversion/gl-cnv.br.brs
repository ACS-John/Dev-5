00010 ! Replace S:\acsGL\Conversion\GL-CNV
00020 ! ???
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnputcno,fnerror,fnindex_it
00050   fntop(program$,"CHANGE_ME")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim d$*50,rf(6),bc(12),bp(12),bm(12),ta(2),tr(7),tr$*12,td$*30
00090   dim k$(3)*25,ss$*11,m(18)
00100   dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,procdat$*20
00110   dim dat$*20,ch$*20,prgl(21),co(7),acctmo$*6,vn$*8,nam$*35,ad1$*20
00120   dim ad2$*20,csz$*20,ss$*11,adr(2),revb(13)
00130 ! ______________________________________________________________________
00140   pr newpage
00150   pr f "8,5,C 70,HRB,N": "   WARNING THIS PROGRAM CAN ONLY BE RUN ONE TIME FOR EACH COMPANY #"
00160   pr f "10,9,C 60": "ENTER THE COMPANY # TO BE CONVERTED OR 0 TO STOP:"
00170 L170: input fields "10,60,N 5,UE,N": cno conv L170
00180   if cno=0 then goto XIT
00190   pr newpage
00200   pr f "10,5,C 60": "CONVERSION FOR COMPANY #"&str$(cno)&" IN PROCESS"
00210 ! 
00220   fnputcno(cno)
00230   open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
00240   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",Replace,RecL=338",internal,output 
00250 L250: read #2,using L260: dno,ano,sno,d$,rf(1),rf(3),rf(5),bb,cb,mat bc,mat bp,mat bm,pbp,rf(2),rf(4),rf(6) eof L300
00260 L260: form n 3,n 6,n 3,c 50,3*pd 3,39*pd 6.2,3*pd 3
00270   write #1,using L280: dno,ano,sno,d$,mat rf,bb,cb,mat bc,0,mat bp,0,mat bm,0,pbp,0,0,mat revb
00280 L280: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
00290   goto L250
00300 L300: close #1: : close #2,free: 
00310   open #2: "Name="&env$('Q')&"\GLmstr\ACGLTRAN.h"&str$(cno)&"",internal,input,relative 
00320   open #1: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno)&",Replace,RecL=73",internal,outin,relative 
00330   write #1,using L340,rec=1: 0,0,0,0,0,0,0," "," ",lrec(2)
00340 L340: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00350   for j=1 to lrec(2)
00360     read #2,using L340,rec=j: mat tr,tr$,td$ norec L380,conv L380
00370     write #1,using L340: mat tr,tr$,td$,0
00380 L380: next j
00390   close #1: : close #2,free: 
00400   open #1: "Name="&env$('Q')&"\GLmstr\ACGLPGMN.h"&str$(cno)&",Replace,RecL=58",internal,outin,relative 
00410   for j=1 to 20
00420     write #1,using L430,rec=j: "","",0,0,0
00430 L430: form pos 1,c 20,c 35,3*n 1
00440   next j
00450   close #1: 
00460   open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&"",internal,outin,relative ioerr L550
00470   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",Replace,RecL=190",internal,output 
00480   for j=1 to lrec(2)
00490     read #2,using L500,rec=j: eno,mat k$,ss$,mat m,mat ta eof L540,conv L530,norec L530
00500 L500: form pos 1,n 4,3*c 25,c 11,18*pd 5.2,2*n 5
00510     if eno=0 then goto L530
00520     write #1,using L500: eno,mat k$,ss$,mat m,0,0
00530 L530: next j
00540 L540: close #2,free: 
00550 L550: if lrec(1)=0 then close #1,free: else close #1: 
00560   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno),internal,outin,relative 
00570   read #1,using L580,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$
00580 L580: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30
00590   close #1,free: 
00600   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",SIZE=0,RecL=512",internal,outin,relative 
00610   open #2: "Name="&env$('Q')&"\GLmstr\PRGLNUMB.h"&str$(cno)&"",internal,input ioerr L650
00620   read #2,using L630: mat prgl
00630 L630: form pos 1,21*pd 4
00640   close #2,free: 
00650 L650: open #2: "Name="&env$('Q')&"\GLmstr\GLREC.h"&str$(cno)&"",internal,input ioerr L690
00660   read #2,using L670: reccode
00670 L670: form pos 1,n 1
00680   close #2,free: 
00690 L690: open #2: "Name=S:\acsGL\ACGLCODE.H"&str$(cno),internal,input ioerr L730
00700   read #2,using L710: c1,c2
00710 L710: form pos 1,n 1,n 2
00720   close #2,free: 
00730 L730: open #2: "Name="&env$('Q')&"\GLmstr\ACGLDATE.h"&str$(cno)&"",internal,input 
00740   read #2,using L750: procdat$,dat$,acctmo,acctmo$,ch$,lmu
00750 L750: form pos 1,c 20,c 20,n 2,c 6,c 20,n 2
00760   close #2,free: 
00770   write #1,using L780,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$,c2,c1,procdat$,dat$,acctmo,acctmo$,ch$,lmu,mat prgl,0,reccode,nap,ficarate,ficawage,feducrat,feducwag
00780 L780: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30,n 2,n 1,2*c 20,n 2,c 6,c 20,n 2,21*pd 4,2*n 1,n 2,pd 5.3,pd 5.2,pd 5.3,pd 5.2
00790   close #1: 
00800   open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",NoShr",internal,input ioerr L820
00810   close #1,free: 
00820 L820: open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",size=0,RecL=68,Shru",internal,outin,relative 
00830   close #1: 
00840   gosub L920 ! CONVERT VENDOR FILES
00850   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&" 1 12 Replace DupKeys -n"
00860   execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 Replace DupKeys -n"
00870   fnindex_it(env$('Q')&"\GLmstr\GLBREC.h"&env$('cno'),env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno'),"1 24")
00880   execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&" 1 4 Replace DupKeys -n" ioerr L890
00890 L890: execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 Replace DupKeys"
00900   goto L1170
00910 ! ______________________________________________________________________
00920 L920: open #3: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed 
00930   open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno),internal,outin,relative ioerr L950
00940   close #2,free: 
00950 L950: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",SIZE=0,RecL=64,NoShr",internal,outin,relative 
00960   write #2,using L1000,rec=1: "",0,0,"","",1
00970   open #1: "Name="&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed ioerr L990
00980   close #1,free: 
00990 L990: open #1: "Name="&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&",size=0,RecL=127,NoShr",internal,outin,relative 
01000 L1000: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
01010 L1010: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
01020 L1020: read #3,using L1010: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ eof L1100
01030   mat adr=(0)
01040   if ytdp=0 then goto L1080
01050   let rec2=lrec(2)+1
01060   write #2,using L1000,rec=rec2: vn$,dat,ytdp,"","BEGINNING BALANCE",0
01070   mat adr=(rec2)
01080 L1080: write #1,using L1010: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
01090   goto L1020
01100 L1100: close #1: ioerr L1110
01110 L1110: close #2: ioerr L1120
01120 L1120: close #3: ioerr L1130
01130 L1130: execute "Copy "&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)&' '&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)
01140   execute "Free "&env$('Q')&"\GLmstr\GL1099N.H"&str$(cno)
01150   return 
01160 ! ______________________________________________________________________
01170 L1170: ! REASS.CNV
01180   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
01190   open #2: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno),internal,outin,relative 
01200   pr newpage
01210   pr f "10,15,c 60,h,n": "REASSIGN GL ADDRESSES IN PROCESS"
01220 L1220: form pos 333,2*pd 3
01230   lr2=lrec(2)
01240   rewrite #2,using L1340,rec=1: lr2
01250   for j=1 to lr2
01260     read #2,using L1270,rec=j: k$,nta norec L1350
01270 L1270: form pos 1,c 12,pos 71,pd 3
01280     read #1,using L1220,key=k$: mat ta nokey L1350
01290     if ta(1)=0 then let ta(1)=j
01300     if ta(2)>0 then rewrite #2,using L1340,rec=ta(2): j
01310     let ta(2)=j
01320     rewrite #1,using L1220,key=k$: mat ta
01330     rewrite #2,using L1340,rec=j: 0
01340 L1340: form pos 71,pd 3
01350 L1350: next j
01360   close #1: 
01370   close #2: 
01380   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),internal,outin,keyed ioerr L1580
01390   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno),internal,outin,relative ioerr L1580
01400   pr newpage
01410   pr f "10,15,c 60,h,n": "REASSIGN PR ADDRESSES IN PROCESS"
01420 L1420: form pos 181,2*n 5
01430   lr2=lrec(2)
01440   rewrite #2,using L1540,rec=1: lr2
01450   for j=1 to lr2
01460     read #2,using L1470,rec=j: en$,nta norec L1550,conv L1550
01470 L1470: form pos 1,c 4,pos 68,pd 3
01480     read #1,using L1420,key=en$: mat ta nokey L1550,conv L1550
01490     if ta(1)=0 then let ta(1)=j
01500     if ta(2)>0 then rewrite #2,using L1540,rec=ta(2): j
01510     let ta(2)=j
01520     rewrite #1,using L1420,key=en$: mat ta
01530     rewrite #2,using L1540,rec=j: 0
01540 L1540: form pos 68,pd 3
01550 L1550: next j
01560   close #1: 
01570   close #2: 
01580 L1580: ! 
01590   open #1: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno),internal,input,relative ioerr L1740
01600   pr f "14,32,C 16,BR,N": "   IN PROCESS"
01610   open #2: "Name=X,RecL=72,Replace",internal,output 
01620   for j=1 to lrec(1)
01630     read #1,using L1660,rec=j: mat tr,tr$,td$ conv L1680
01640     if tr(1)+tr(2)+tr(3)=0 then goto L1680
01650     actpd=int(tr(4)*.0001)
01660 L1660: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
01670     write #2,using L1660: mat tr,tr$,td$,actpd
01680 L1680: next j
01690   close #1,free: 
01700   close #2: 
01710   execute "Rename X "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)
01720   execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)&' '&env$('Q')&"\GLmstr\AcTrIdx.h"&str$(cno)&" 1/71/17/13 12/2/2/4 Replace DupKeys"
01730 ! ___    __________   ___________   __________   ___________   _________
01740 L1740: ! S:\acsGL\PRmstr.CNV
01750   dim pr1$*90,pr1(18),pr2(36)
01760   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),internal,outin,keyed ioerr L1940
01770   open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,Replace",internal,output 
01780 L1780: read #1,using L1790: pr1$,mat pr1 eof END1
01790 L1790: form pos 1,c 90,18*pd 5.2,2*n 5
01800   for j=1 to 11: let pr2(j)=pr1(j): next j
01810   let pr2(13)=pr1(12)
01820   for j=13 to 18: let pr2(j+18)=pr1(j): next j
01830   write #2,using L1840: pr1$,mat pr2
01840 L1840: form pos 1,c 90,36*pd 5.2,2*n 5
01850   goto L1780
01860 ! ______________________________________________________________________
01870 END1: close #1: 
01880   close #2: 
01890   execute "COPY "&env$('Temp')&"\Work."&session$&", "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&""
01900   execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&","&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",1,4,Replace,DupKeys"
01910   fnputcno(cno)
01920   open #1: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",RecL=110,Replace",internal,output 
01930   close #1: 
01940 L1940: open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno),internal,outin,relative ioerr L1990
01950   read #1,using L1960,rec=1: gl1$,gl2$
01960 L1960: form pos 298,2*c 12
01970   rewrite #1,using L1960,rec=1: gl2$,gl1$
01980   close #1: 
01990 L1990: end1=0 ! 
02000   dim id$(6)*40,fil$(6),idx$(6)
02010   let id$(1)=" 1 = BALANCE SHEET FILE" : let fil$(1)="ACGLFNSB" : let idx$(1)="FNSBINDX"
02020   let id$(2)=" 2 = INCOME STATEMENT FILE" : let fil$(2)="ACGLFNSI" : let idx$(2)="FNSIINDX"
02030   let id$(3)=" 3 = FUND STMT / CASH FLOW FILE" : let fil$(3)="ACGLFNSF" : let idx$(3)="FNSFINDX"
02040   let id$(4)=" 4 = SECONDARY BALANCE SHEET FILE" : let fil$(4)="ACGLFNSC" : let idx$(4)="FNSCINDX"
02050   let id$(5)=" 5 = SECONDARY INCOME STATEMENT FILE" : let fil$(5)="ACGLFNSJ" : let idx$(5)="FNSJINDX"
02060   let id$(6)=" 6 = SECONDARY FUND / CASH FLOW FILE" : let fil$(6)="ACGLFNSG" : let idx$(6)="FNSGINDX"
02070   for j=1 to 6
02080     execute "Copy "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&" "&env$('Temp')&"\Work."&session$&" -83" ioerr L2360
02090     execute "COPY  "&env$('Temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&""
02100     if j=2 or j=5 then goto L2110 else goto L2360
02110 L2110: open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(j)&".h"&str$(cno),internal,outin,keyed 
02120     end1=st1=st2=rno=rnp=0
02130 L2130: gosub FIND1
02140     restore #1,key>=lpad$(str$(st1),5): nokey END2
02150 L2150: read #1,using L2270: rno,ic eof END2
02160     if rno<st2 then goto L2210
02170     if end1=1 then goto END2
02180     let rnp=0
02190     goto L2130
02200 ! ______________________________________________________________________
02210 L2210: rewrite #1,using L2220: rnp
02220 L2220: form pos 79,n 5
02230     goto L2150
02240 ! ______________________________________________________________________
02250 FIND1: st1=rno : st2=99999 : let rnp=0
02260 L2260: read #1,using L2270: rno,ic eof END21
02270 L2270: form pos 1,g 5,pos 75,n 1
02280     if ic=0 then goto L2260
02290     if ic=1 then let rnp=rno
02300     if ic=2 then st2=rno : goto L2330
02310     goto L2260
02320 END21: end1=1
02330 L2330: return 
02340 ! ______________________________________________________________________
02350 END2: close #1: 
02360 L2360: next j
02370   chain "S:\acsGL\Company"
02380 ! ______________________________________________________________________
02390 XIT: stop 
02400 ! ______________________________________________________________________
02410 ! <Updateable Region: ERTN>
02420 ERTN: let fnerror(program$,err,line,act$,"xit")
02430   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02460 ERTN_EXEC_ACT: execute act$ : goto ERTN
02470 ! /region
02480 ! ______________________________________________________________________
