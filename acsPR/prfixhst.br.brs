00010 ! Replace S:\acsPR\prFixHst
00020 ! Fix YTD and QTD from History !:
        ! will create qtd information based on date range for chech history
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fncno,fnerror, fntop, fnxit, fndate_mmddyy_to_ccyymmdd,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ty1(22),t1(22),t2(22),t3(22)
00080   dim em$(3)*30,ss$*11,rs(2),em(16),ta(2),holdem$*30
00090   dim tdt(4),tcd(3),tdet(17),tc2(22),ty(21),tqm(17),tcp(22)
00100   dim tdy(6),tdc(6),cap$*128,tmp8(13),nam$*28,n$*30
00110 ! ______________________________________________________________________
00120   fntop("S:\acsPR\prFixHst",cap$="Fix YTD and QTD From History")
00130   fncno(cno)
00135   fnconsole(1)
00140 ! 
00150 ! ______________________________________________________________________
00160   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,input,relative 
00170   read #1,using L180,rec=1: mat dedfed
00180 L180: form pos 638,10*n 1
00190   close #1: 
00200   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno'),internal,outIn,keyed 
00210   open #11: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDX2.h"&env$('cno'),internal,outIn,keyed 
00220   open #4: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\PRCKINDX.h"&env$('cno')&",Shr",internal,outIn,keyed 
00230   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno'),internal,outIn,relative 
00240   goto L430
00250 ! ______________________________________________________________________
00260 UPDT: ent$=lpad$(str$(heno),8)
00270 ! IF SUM(T1)=0 THEN GOTO asdf
00280   read #1,using 'Form POS 1,N 8,3*C 30,C 11,2*N 1,7*N 2,2*PD 3.3,6*PD 4.2,2*N 6,PD 5.2,2*PD 3',key=ent$: teno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta nokey L400
00290   tadr=ta(1)
00300 L300: if tadr=0 then goto L400
00310   read #2,using 'Form POS 1,N 8,N 3,N 3,N 6,N 3,4*N 6,3*N 2,24*PD 4.2,5*PD 3.2,POS 471,PD 4.2,POS 168,60*PD 5.2,PD 3',rec=tadr: teno,tdn,gl1,gl2,gl3,mat tdt,mat tcd,tli,mat tdet,mat tdy,mat tdc,mat ty,mat tqm,mat tcp,nta
00320   if qtd$="N" then goto L350
00330   for j=1 to 15 : tqm(j)=t1(j) : next j
00340   tqm(16)=t1(21)
00350 L350: if ytd$="N" then goto L370
00360   for j=1 to 21 : ty(j)=ty1(j) : next j ! ( asdf )
00370 L370: rewrite #2,using 'Form POS 168,38*PD 5.2',rec=tadr: mat ty,mat tqm
00380   mat t1=(0) : mat ty1=(0) : tadr=nta
00390   goto L300
00400 L400: mat t1=(0) : mat ty1=(0) : heno=0
00410   return 
00420 ! ______________________________________________________________________
00430 L430: gosub INPSCRN
00440   fnwait(message$,0)
00450 L450: read #4,using L460: eno,prd,ckno,mat tdc,mat tc2 eof L540
00460 L460: form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
00470   if heno=0 then goto L490
00480   if heno><eno then gosub UPDT
00490 L490: heno=eno
00500 ! pRD=fndate_mmddyy_to_ccyymmdd(PRD) ! IF IN MMDDYY FORMAT
00510   if prd>=yd1 and prd<=yd2 then mat ty1=ty1+tc2 ! ytd$
00520   if prd>=qd1 and prd<=qd2 then mat t1=t1+tc2 ! QTR
00530   goto L450
00540 L540: gosub UPDT
00550 XIT: fnxit
00560 ! ______________________________________________________________________
00570 INPSCRN: ! 
00580   pr newpage !:
        fnopenwin(win=101,10,15,15,62,cap$)
00590   pr #win,fields "4,2,Cr 38,N": "Fix Year-To-Date Information (Y/N):"
00600   pr #win,fields "5,2,Cr 38,N": "Fix Quarter-To-Date Information (Y/N):"
00610   io5$(1)="4,41,Cu 1,UET,N"
00620   io5$(2)="5,41,Cu 1,UET,N"
00630   pr f "16,30,C 09,B,1": "Next (F1)" !:
        pr f "16,41,C 09,B,5": "Exit (F5)"
00640   ytd$="N" : qtd$="N"
00650 L650: input #win,fields mat io5$: ytd$,qtd$ conv CONV1
00660   if ce>0 then io5$(ce)(ce1:ce2)="U": ce=0
00670   if cmdkey>0 then goto L740 else ce=curfld+1
00680   if ce>udim(io5$) then ce=1
00690 L690: io5$(ce)=rtrm$(io5$(ce)) : ce1=pos(io5$(ce),"U",1)
00700   ce2=ce1+1 : io5$(ce)(ce1:ce1)="UC" : goto L650
00710 CONV1: if ce>0 then io5$(ce)(ce1:ce2)="U"
00720   ce=cnt+1
00730 ERR1: pr f "24,78,C 1": bell : goto L690
00740 L740: if cmdkey=5 then goto XIT
00750   if ytd$<>"Y" and ytd$<>"N" then ce=1 : goto ERR1
00760   if qtd$<>"Y" and qtd$<>"N" then ce=2 : goto ERR1
00770   if ytd$="N" and qtd$="N" then ce=1 : goto ERR1
00780   if ytd$="N" then goto L1020
00790 ! _______
00800   pr newpage !:
        fnopenwin(win=101,10,18,15,62,cap$)
00810   pr #win,fields "4,2,Cr 36,N": "Starting Date for the Year (mmddyy):"
00820   pr #win,fields "5,2,Cr 36,N": "Ending Date for the Year (mmddyy):"
00830   io5$(1)="4,39,Nz 6,UT,N"
00840   io5$(2)="5,39,Nz 6,UT,N"
00850   pr f "16,30,C 09,B,1": "Next (F1)"
00860   pr f "16,41,C 09,B,5": "Exit (F5)"
00870 L870: input #win,fields mat io5$: yd1,yd2 conv CONV2
00880   if ce>0 then io5$(ce)(ce1:ce2)="U": ce=0
00890   if cmdkey>0 then goto L960 else ce=curfld+1
00900   if ce>udim(io5$) then ce=1
00910 L910: io5$(ce)=rtrm$(uprc$(io5$(ce))) : ce1=pos(io5$(ce),"U",1)
00920   ce2=ce1+1 : io5$(ce)(ce1:ce1)="UC" : goto L870
00930 CONV2: if ce>0 then io5$(ce)(ce1:ce2)="U"
00940   ce=cnt+1
00950 ERR2: pr f "24,78,C 1": bell : goto L910
00960 L960: if cmdkey=5 then goto XIT
00970   if yd1=0 then ce=1 : goto ERR2
00980   if yd2=0 then ce=2 : goto ERR2
00990   yd1=fndate_mmddyy_to_ccyymmdd(yd1)
01000   yd2=fndate_mmddyy_to_ccyymmdd(yd2)
01010   if yd2<yd1 then ce=1 : goto ERR2
01020 L1020: ! UPDATE QTR
01030   if qtd$="N" then goto L1260
01040 ! _______
01050   pr newpage !:
        fnopenwin(win=101,10,16,15,63,cap$)
01060   pr #win,fields "04,2,Cr 39,N": "Starting Date for the Quarter (mmddyy):"
01070   pr #win,fields "05,2,Cr 39,N": "Ending Date for the Quarter (mmddyy):"
01080   pr f "16,30,C 09,B,1": "Next (F1)" !:
        pr f "16,41,C 09,B,5": "Exit (F5)"
01090   io5$(1)="4,42,Nz 6,UT,N"
01100   io5$(2)="5,42,Nz 6,UT,N"
01110 L1110: input #win,fields mat io5$: qd1,qd2 conv CONV2
01120   if ce>0 then io5$(ce)(ce1:ce2)="U": ce=0
01130   if cmdkey>0 then goto L1200 else ce=curfld+1
01140   if ce>udim(io5$) then ce=1
01150 L1150: io5$(ce)=rtrm$(uprc$(io5$(ce))) : ce1=pos(io5$(ce),"U",1)
01160   ce2=ce1+1 : io5$(ce)(ce1:ce1)="UC" : goto L1110
01170 CONV3: if ce>0 then io5$(ce)(ce1:ce2)="U"
01180   ce=cnt+1
01190 ERR3: pr f "24,78,C 1": bell : goto L1150
01200 L1200: if cmdkey=5 then goto XIT
01210   if qd1=0 then ce=1 : goto ERR2
01220   if qd2=0 then ce=2 : goto ERR2
01230   qd1=fndate_mmddyy_to_ccyymmdd(qd1)
01240   qd2=fndate_mmddyy_to_ccyymmdd(qd2)
01250   if qd2<qd1 then ce=1 : goto ERR2
01260 L1260: return 
01270 ! ______________________________________________________________________
01280 ! <Updateable Region: ERTN>
01290 ERTN: fnerror(program$,err,line,act$,"xit")
01300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01330 ERTN_EXEC_ACT: execute act$ : goto ERTN
01340 ! /region
01350 ! ______________________________________________________________________
