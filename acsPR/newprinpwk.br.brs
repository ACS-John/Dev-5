00010 ! Replace S:\acsPR\newprInpWk
00020 ! pr Input Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fnGetPayrollDates
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$*30,em(3),tdt(4),tdy(6),ta(2),dat$*20,cap$*128,message$*40
00080   dim gl$*12,tdt(4),tcd(3),tdet(23)
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Input Worksheet")
00120   on fkey 5 goto DONE
00130   fnopenprn
00150   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,dat$)
00170 L170: gosub HDR
00180   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00190   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&", KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outin,keyed 
00200 READ_EMPLOYEE: ! 
00210 L210: read #1,using L220: eno,em$,em4,mat em eof DONE
00220 L220: form pos 1,n 8,c 30,pos 118,n 2,pos 132,2*pd 4.2,pos 156,n 6,pos 173,2*pd 3
00230   if em4=9 then goto L210
00240   let fsttrl=1
00250   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey READ_EMPLOYEE
00260 L260: read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L210
00270   if teno<>eno then pr #255: pageoflow NEWPGE: goto READ_EMPLOYEE
00280   if fsttrl=1 then goto L320
00290   pr #255,using L300: tdn,tdet(2),rpt$("  __________",3) pageoflow NEWPGE
00300 L300: form pos 44,n 3,n 10.2,c 36,skip 1
00310   goto L350
00320 L320: pr #255,using L330: eno,em$,tdn,tdet(2),rpt$("  __________",3) pageoflow NEWPGE
00330 L330: form pos 1,pic(zzzzzzzz),x 2,c 33,n 3,n 10.2,c 36,skip 1
00340   let fsttrl=0
00350 L350: goto L260
00360 ! ______________________________________________________________________
00370 NEWPGE: pr #255: newpage : gosub HDR : continue 
00380 ! ______________________________________________________________________
00390 DONE: close #1: ioerr ignore
00400   close #2: ioerr L430
00410   pr #255,using L420: "Totals",rpt$("  ==========",3)
00420 L420: form pos 11,c 33,skip 1,pos 57,c 36,skip 1
00430 L430: let fncloseprn
00440   fnxit
00450 ! ______________________________________________________________________
00460 HDR: ! 
00470   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00480   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00490   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00500   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00510   pr #255: "\ql   "
00520   pr #255: "Employee                                  Dept    Hourly  <----------- Hours -------------->"
00530   pr #255: " Number   Employee Name                   Numb      Rate   Regluar     Overtime      Other"
00540   pr #255: "________  ______________________________  ____    ______  __________  __________  __________"
00550   return 
00560 ! ______________________________________________________________________
00570 ! <Updateable Region: ERTN>
00580 ERTN: let fnerror(program$,err,line,act$,"xit")
00590   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00610   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00620 ERTN_EXEC_ACT: execute act$ : goto ERTN
00630 ! /region
00640 ! ______________________________________________________________________
00650 XIT: let fnxit
00660 ! ______________________________________________________________________
