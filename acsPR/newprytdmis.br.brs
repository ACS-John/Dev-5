00010 ! Replace S:\acsPR\newprYTDMis
00020 ! Miscellaneous Deductions Register - YTD
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs,fnGetPayrollDates,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$*30,em(6),cap$*128,message$*40
00080   dim dat$*20,t1(20),t2(20)
00090   dim dedcode(20),calcode(20),dedfed(20)
00100   dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00110   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
00120   dim e$(10)*12,tpt(32),cap$*128,message$*40,resp$(15)*30
00130   dim tcp(32),tdc(10),ytdtotal(32),ss$*11
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="Deductions Register YTD")
00170   fnopenprn
00180   fnGetPayrollDates(beg_date,end_date)
00190 ! ______________________________________________________________________
00200 MENU1: ! 
00210   fntos(sn$="prytdmis")
00212   respc=0
00220   fnlbl(1,43," ",1,1)
00230   fnlbl(1,1,"Beginning Date of Tax Year:",26,1)
00240   fntxt(1,30,12,0,0,"3",0,"") 
00242   resp$(respc+=1)=str$(beg_date)
00250   fnlbl(2,1,"Ending Date of Tax Year:",26,1)
00260   fntxt(2,30,12,0,0,"3",0,"") 
00262   resp$(respc+=1)=str$(end_date)
00270   fncmdset(2): fnacs(sn$,0,mat resp$,ck)
00280   if ck=5 then goto XIT
00290   beg_date=val(resp$(1)) ! beginning of year
00310   end_date=val(resp$(2)) ! ending day of year
00320 ! ______________________________________________________________________
00330   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
00340   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00350   read #20,using L360: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
00355   ficamaxw=ficamaxw*10
00360 L360: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
00370   close #20: 
00380   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00390   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00400 ! 
00410   open #3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
00420 ! 
00430 ! 
00440   for j=1 to 20: abbrevname$(j)=lpad$(rtrm$(abbrevname$(j)),6) : next j
00450   gosub HDR
00460 L460: read #2,using L470: eno,em$,mat em eof EOJ
00470 L470: form pos 1,n 8,c 30,pos 112,6*n 2,pos 173,2*pd 3
00480   mat ytdtotal=(0)
00490   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00500   restore #4,key>=checkkey$: nokey L460
00510 L510: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof PRINT_INFO
00520   if heno<>eno then goto PRINT_INFO
00530   if prd<beg_date or prd>end_date then goto L510 ! not this year
00540   mat ytdtotal=ytdtotal+tcp
00550   goto L510
00560 ! ______________________________________________________________________
00570 PRINT_INFO: ! 
00580   for j=1 to 20
00590     t1(j)=ytdtotal(j+4)
00600   next j
00610   pr #255,using L660: eno,em$(1:18),t1(1),t1(2),t1(3),t1(4),t1(5),t1(6),t1(7),t1(8),t1(9),t1(10) pageoflow NEWPGE
00620   for j=1 to 10
00630     if trim$(abbrevname$(j+10))<>"" then goto L670 ! have more than 10 deductions
00640   next j
00650   goto L690
00660 L660: form pos 1,g 8,x 2,c 18,10*n 10.2,skip 1
00670 L670: pr #255,using L680: t1(11),t1(12),t1(13),t1(14),t1(15),t1(16),t1(17),t1(18),t1(19),t1(20) pageoflow NEWPGE
00680 L680: form pos 34,10*n 10.2,skip 1
00690 L690: mat t2=t2+t1
00700   goto L460
00710 ! ______________________________________________________________________
00720 EOJ: ! 
00730   pr #255,using L740: rpt$("  ________",10)
00740 L740: form pos 29,c 100,skip 1
00750   pr #255,using L660: "","Totals",t2(1),t2(2),t2(3),t2(4),t2(5),t2(6),t2(7),t2(8),t2(9),t2(10)
00760   for j=1 to 10
00770     if trim$(abbrevname$(j+10))<>"" then goto L800 ! have more than 10 deductions
00780   next j
00790   goto L810
00800 L800: pr #255,using L680: t2(11),t2(12),t2(13),t2(14),t2(15),t2(16),t2(17),t2(18),t2(19),t2(20)
00810 L810: ! ______________________________________________________________________
00820 DONE: ! 
00830   fncloseprn
00840   close #1: ioerr L850
00850 L850: close #2: ioerr L860
00860 L860: fnxit
00870 ! ______________________________________________________________________
00880 NEWPGE: pr #255: newpage : gosub HDR : continue 
00890 ! ______________________________________________________________________
00900 HDR: ! 
00910   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00920   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00930   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00940   pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
00950   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00960   pr #255: "\ql   "
00970   pr #255,using L980: "Emp-Numb  Employee Name",abbrevname$(1),abbrevname$(2),abbrevname$(3),abbrevname$(4),abbrevname$(5),abbrevname$(6),abbrevname$(7),abbrevname$(8),abbrevname$(9),abbrevname$(10)(1:6)
00980 L980: form pos 1,c 32,9*c 10,c 6
00990   pr #255: "________  __________________";rpt$("  ________",10)
01000   for j=1 to 10
01010     if trim$(abbrevname$(j+10))<>"" then goto L1040 ! have more than 10 deductions
01020   next j
01030   goto L1070
01040 L1040: pr #255,using L1050: abbrevname$(11),abbrevname$(12),abbrevname$(13),abbrevname$(14),abbrevname$(15),abbrevname$(16),abbrevname$(17),abbrevname$(18),abbrevname$(19),abbrevname$(20)
01050 L1050: form pos 38,10*c 10
01060   pr #255: "                                 ";rpt$("  ________",10)
01070 L1070: return 
01080 XIT: fnxit
01090 ! ______________________________________________________________________
01100 ! <Updateable Region: ERTN>
01110 ERTN: fnerror(program$,err,line,act$,"xit")
01120   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01130   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01140   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01150 ERTN_EXEC_ACT: execute act$ : goto ERTN
01160 ! /region
01170 ! ______________________________________________________________________
