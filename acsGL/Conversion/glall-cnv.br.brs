00010 ! REPLACE S:\acsGL\Conversion\glAll-CNV
00020   dim tr(7),tr$*12,td$*30,cnam$*40
00030   library 'S:\Core\Library': fntop
00040   pr newpage
00050   fntop(program$,"CHANGE_ME")
00070 L70: ! 
00080   pr f "10,24,C 32": "COMPANY NUMBER TO CONVERT:"
00090   pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
00100 L100: input fields "10,57,N 5,UE,N",attr "R": cno conv L100
00110   if cmdkey=5 then stop 
00120 ! 
00130   open #1: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno),internal,input ioerr L260
00140   pr f "14,32,C 16,BR,N": "   IN PROCESS"
00150   open #2: "Name=X,size=0,RecL=72,REPLACE",internal,output 
00160 L160: read #1,using L190: mat tr,tr$,td$ eof L220
00170   if tr(1)+tr(2)+tr(3)=0 then goto L160
00180   actpd=int(tr(4)*.0001)
00190 L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
00200   write #2,using L190: mat tr,tr$,td$,actpd
00210   goto L160
00220 L220: close #1,free: 
00230   close #2: 
00240   execute "Rename X "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)
00250   execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)&' '&env$('Q')&"\GLmstr\AcTrIdx.h"&str$(cno)&" 1/71/17/13 12/2/2/4 REPLACE DupKeys"
00260 L260: ! S:\acsGL\PRmstr.CNV
00270   dim pr1$*90,pr1(18),pr2(36)
00280   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),internal,outin,keyed ioerr L480
00290   open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,REPLACE",internal,output 
00300 L300: read #1,using L310: pr1$,mat pr1 eof END3
00310 L310: form pos 1,c 90,18*pd 5.2,2*n 5
00320   for j=1 to 11: let pr2(j)=pr1(j): next j
00330   let pr2(13)=pr1(12)
00340   for j=13 to 18: let pr2(j+18)=pr1(j): next j
00350   write #2,using L360: pr1$,mat pr2
00360 L360: form pos 1,c 90,36*pd 5.2,2*n 5
00370   goto L300
00380 END3: close #1: 
00390   close #2: 
00400   execute "COPY "&env$('Temp')&"\Work."&session$&", "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&""
00410   execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&","&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",1,4,REPLACE,DupKeys"
00420   open #1: "Name=CNO.H"&wsid$,internal,outin,relative 
00430   rewrite #1,using L440,rec=1: cno
00440 L440: form pos 1,n 2
00450   close #1: 
00460   open #1: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",SIZE=0,RecL=110,REPLACE",internal,output 
00470   close #1: 
00480 L480: close #1: ioerr L490
00490 L490: open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno),internal,outin,relative ioerr L560
00500   read #1,using L510,rec=1: gl1$,gl2$
00510 L510: form pos 298,2*c 12
00520 ! write yes to bank rec and accumulate transactions
00530   rewrite #1,using L540,rec=1: gl2$,gl1$,"Y",1
00540 L540: form pos 298,2*c 12,pos 406,g 1,pos 417,n 1
00550   close #1: 
00560 L560: ! S:\acsGL\FINSTMT.CNV
00570   dim cnam$*40,id$(6)*40,fil$(6),idx$(6)
00580   let id$(1)=" 1 = BALANCE SHEET FILE": let fil$(1)="ACGLFNSB": let idx$(1)="FNSBINDX"
00590   let id$(2)=" 2 = INCOME STATEMENT FILE": let fil$(2)="ACGLFNSI": let idx$(2)="FNSIINDX"
00600   let id$(3)=" 3 = FUND STMT / CASH FLOW FILE": let fil$(3)="ACGLFNSF": let idx$(3)="FNSFINDX"
00610   let id$(4)=" 4 = SECONDARY BALANCE SHEET FILE": let fil$(4)="ACGLFNSC": let idx$(4)="FNSCINDX"
00620   let id$(5)=" 5 = SECONDARY INCOME STATEMENT FILE": let fil$(5)="ACGLFNSJ": let idx$(5)="FNSJINDX"
00630   let id$(6)=" 6 = SECONDARY FUND / CASH FLOW FILE": let fil$(6)="ACGLFNSG": let idx$(6)="FNSGINDX"
00640   pr newpage
00650   for j=1 to 6
00660     execute "Copy "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&" "&env$('Temp')&"\Work."&session$&" -83" ioerr L950
00670     execute "COPY  "&env$('Temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&""
00680     if j=2 or j=5 then goto L690 else goto L950
00690 L690: open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(j)&".h"&str$(cno)&"",internal,outin,keyed 
00700     end1=st1=st2=rno=rnp=0
00710 L710: gosub FIND1
00720     restore #1,key>=lpad$(str$(st1),5): nokey END2
00730 L730: read #1,using L830: rno,ic eof END2
00740     if rno<st2 then goto L780
00750     if end1=1 then goto END2
00760     let rnp=0
00770     goto L710
00780 L780: rewrite #1,using L790: rnp
00790 L790: form pos 79,n 5
00800     goto L730
00810 FIND1: st1=rno : st2=99999 : let rnp=0
00820 L820: read #1,using L830: rno,ic eof END1,conv L880
00830 L830: form pos 1,g 5,pos 75,n 1
00840     if ic=0 then goto L820
00850     if ic=1 then let rnp=rno
00860     if ic=2 then st2=rno : goto L930
00870     goto L820
00880 L880: read #1,using L900: rno$ eof END1
00890     delete #1: 
00900 L900: form pos 1,c 5,pos 75,n 1
00910     goto L820
00920 END1: end1=1
00930 L930: return 
00940 END2: close #1: 
00950 L950: next j
00960   chain "S:\acsGL\Company"
