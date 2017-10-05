00010 ! Replace S:\acsPR\Conversion\prGL-Fix
00020 ! CREATE GL ENTRIEX
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim tdc(6),tcp(22),em$*30,tgl(3),tdet(3)
00080 ! ______________________________________________________________________
00090   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00100 ! ______________________________________________________________________
00110   fncno(cno)
00120 ! 
00130   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,outin,keyed 
00140   open #3: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,outin,relative 
00150   open #4: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #5: "Name="&env$('Temp')&"\Work."&session$&",KFName="&env$('Temp')&"\Addr."&session$&",RecL=30,KPS=1,KLN=12,Replace",internal,outin,keyed 
00170   io5$(1)="11,55,N 6,U,N"
00180   io5$(2)="12,55,N 6,U,N"
00190   pr newpage
00200   close #101: ioerr L210
00210 L210: open #101: "SROW=10,SCOL=16,EROW=13,ECOL=62,BORDER=DR,CAPTION=CREATE GL ENTRIEX",display,outin 
00220   pr f "11,18,C 40": "ENTER LOWEST  DATE OR BLANK FOR ALL:"
00230   pr f "12,18,C 40": "ENTER HIGHEST DATE OR BLANK FOR ALL:"
00240   pr f "14,22,C 34,R,N": "Press F1 to continue or F5 to stop"
00250 L250: input fields mat io5$,attr "R": prd1,prd2 conv L250
00260   if ce>0 then io5$(ce)(ce1:ce2)="U": ce=0
00270   if cmdkey>0 then goto L340 else ce=curfld+1
00280   if ce>udim(io5$) then ce=1
00290 L290: io5$(ce)=rtrm$(uprc$(io5$(ce))) : ce1=pos(io5$(ce),"U",1)
00300   ce2=ce1+1 : io5$(ce)(ce1:ce1)="UC" : goto L250
00310 CONV5: if ce>0 then io5$(ce)(ce1:ce2)="U"
00320   ce=cnt+1
00330 ERR5: pr f "24,78,C 1": bell : goto L290
00340 L340: ! 
00350   if prd2=0 then prd2=prd1
00360   if fncd(prd2)<fncd(prd1) then goto L250
00370   if cmdkey=5 then goto XIT
00380   gosub L760
00390 L390: read #4,using L400: eno,prd,ckno,mat tdc,mat tcp eof END1
00400 L400: form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
00410   if fncd(prd)<fncd(prd1) or fncd(prd)>fncd(prd2) then goto L390
00420   eno$=lpad$(str$(eno),8)
00430   read #2,using L440,key=eno$: em$,ta1 nokey L390
00440 L440: form pos 9,c 30,pos 173,pd 3
00450   if ta1=0 then goto L390
00460   read #3,using L470,rec=ta1: mat tgl,mat tdet
00470 L470: form pos 12,n 3,n 6,n 3,pos 58,3*pd 4.2
00480   ot=round(tdc(2)*tdet(3),2)
00490   other=round((tdc(3)+tdc(4)+tdc(5))*tdet(2),2)
00500   bonus=tcp(18)
00510   reg=tcp(21)-ot-other-bonus
00520   accum(1)=accum(1)+reg
00530   accum(2)=accum(2)+ot
00540   accum(3)=accum(3)+other
00550   accum(4)=accum(4)+bonus
00560   pr #255,using L710: eno,em$,tgl(1),tgl(2),tgl(3),reg pageoflow L730
00570   gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2))&cnvrt$("N 3",tgl(3))
00580   ga0=reg : gosub TOTGL
00590   if ot=0 then goto L630
00600   pr #255,using L710: eno,em$,tgl(1),tgl(2)+1,tgl(3),ot pageoflow L730
00610   gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+1)&cnvrt$("N 3",tgl(3))
00620   ga0=ot : gosub TOTGL
00630 L630: if other=0 then goto L670
00640   pr #255,using L710: eno,em$,tgl(1),tgl(2)+2,tgl(3),other pageoflow L730
00650   gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+2)&cnvrt$("N 3",tgl(3))
00660   ga0=other : gosub TOTGL
00670 L670: if bonus=0 then goto L710
00680   pr #255,using L710: eno,em$,tgl(1),tgl(2)+3,tgl(3),bonus pageoflow L730
00690   gl$=cnvrt$("N 3",tgl(1))&cnvrt$("N 6",tgl(2)+3)&cnvrt$("N 3",tgl(3))
00700   ga0=bonus : gosub TOTGL
00710 L710: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
00720   goto L390
00730 L730: pr #255: newpage
00740   gosub L760
00750   continue 
00760 L760: p1=p1+1
00770   pr #255,using L780: date$,a$,"PAGE",p1
00780 L780: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
00790   pr #255,using L800: time$,"GENERAL LEDGER DISTRIBUTION FOR PAYROLL",dat1
00800 L800: form pos 1,c 8,pos 17,c 40,skip 1,pos 29,pic(zz/zz/zz),skip 2
00810   pr #255: "EMPLOYEE                                               G/L                 AMOUNT"
00820   pr #255: " NUMBER        NAME                                  ACCOUNT         DEBITS     CREDITS"
00830   pr #255: 
00840   return 
00850 TOTGL: read #5,using L860,key=gl$: gl$,ga1 nokey L900
00860 L860: form pos 1,c 12,pd 5.2
00870   ga1=ga1+ga0
00880   rewrite #5,using L860: gl$,ga1
00890   goto L910
00900 L900: write #5,using L860: gl$,ga0
00910 L910: return 
00920 END1: pr #255: " "
00930   pr #255: "  GL NUMBER          TOTAL"
00940   pr #255: "------------   -----------------"
00950   restore #5,key>="            ": nokey XIT
00960 L960: read #5,using L860: gl$,ga1 eof XIT
00970   pr #255,using L980: gl$,ga1
00980 L980: form pos 1,c 12,pic(zzz,zzz,zzz,zzz.##bcr),skip 1
00990   goto L960
01000 XIT: stop 
01010 ! ______________________________________________________________________
01020 ! <updateable region: ertn>
01030 ERTN: fnerror(program$,err,line,act$,"xit")
01040   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01050   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01060   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01070 ERTN_EXEC_ACT: execute act$ : goto ERTN
01080 ! /region
01090 ! ______________________________________________________________________
