00010 ! Replace S:\acsPR\newprEmpRev
00020 ! Employee Review Register
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror
00042   library 'S:\Core\Library': fntos,fnlbl,fntxt,fncmdset,fnacs,fnGetPayrollDates
00050   on error goto ERTN
00060 ! gosub CHECK_PASSWORD
00070 ! ______________________________________________________________________
00080   dim em$*30,em(3),tdt(4),tdy(6),d1$*20,tcd(3)
00090   dim ytdtdc(10),tdc(10),tcp(32),ytdtotal(32),tdet(23)
00100   dim message$*40,cap$*128
00110 ! ______________________________________________________________________
00130   fntop(program$,cap$="Employee Review Register")
00140 ! ______________________________________________________________________
00150   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
00180 ! ______________________________________________________________________
00190 MENU1: ! 
00200   fntos(sn$="premprev")
00202   respc=0
00210   fnlbl(1,47," ",1,1)
00220   fnlbl(1,1,"Beginning Date of Tax Year:",30,1)
00230   fntxt(1,34,12,0,0,"3",0,"") 
00232   resp$(respc+=1)=str$(beg_date)
00240   fnlbl(2,1,"Last Payroll Date to Analyze:",30,1)
00250   fntxt(2,34,12,0,0,"3",0,"") 
00252   resp$(respc+=1)=str$(end_date)
00260   fncmdset(2): fnacs(sn$,0,mat resp$,ck)
00270   if ck=5 then goto XIT
00280   beg_date=val(resp$(1)) ! beginning of year
00300   end_date=val(resp$(2)) ! ending day of year
00310 ! ______________________________________________________________________
00320   on fkey 5 goto DONE
00330   fnopenprn
00340 ! ______________________________________________________________________
00350   gosub HDR
00360   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00370   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outin,keyed 
00380   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00390 L390: read #1,using L430: eno,em$,em4,mat em eof DONE
00400   a=pos (rtrm$(em$)," ",1)
00410   b=pos (rtrm$(em$)," ",a+1)
00420   em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
00430 L430: form pos 1,n 8,c 30,pos 118,n 2,pos 132,2*pd 4.2,pos 156,n 6,pos 173
00440   let fsttrl=1
00460   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey L390
00480 L480: read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L390
00490   if teno<>eno then goto L390
00500   if tdet(1)> 0 then payrate=tdet(1) else payrate=tdet(2) ! set payrate as salary or hourly
00520   mat ytdtotal=(0) : mat ytdtdc=(0)
00530   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00540   restore #4,key>=checkkey$: nokey L480
00550 L550: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,ctdn,prd,ckno,mat tdc,mat tcp eof L610
00560   if heno<>teno then goto L610 ! read next department
00570   if prd<beg_date or prd>end_date then goto L550 ! not this year
00575   if ctdn<>tdn then goto L550 ! not same department
00580   mat ytdtdc=ytdtdc+tdc !  hours etc
00590   mat ytdtotal=ytdtotal+tcp ! earnings, etc
00600   goto L550
00610 L610: if fsttrl=1 then goto L640
00620   gosub L730 ! pr TRAILER ONLY
00630   goto L660
00640 L640: gosub L670 ! pr MASTER AND FIRST TRAILER
00650   let fsttrl=0
00660 L660: goto L480
00670 L670: pr #255,using L710: eno,em$(1:23),tdn,em(3),tdt(1),tli,tdt(3),tdt(2),em(2),ytdtdc(4),em(1),ytdtdc(3),ytdtdc(5),payrate pageoflow L690
00680   goto L710
00690 L690: pr #255: newpage
00700   gosub HDR
00710 L710: form pos 1,pic(zzzzzzzz),pos 10,c 23,pos 34,pic(zzz),pos 38,pic(zz/zz/zz),pos 47,pic(zz/zz/zz),pos 55,pic(------.##),pos 66,pic(zz/zz/zz),pos 75,pic(zz/zz/zz),pos 83,6*n 10.2,skip 1
00720   return 
00730 L730: pr #255,using L740: tdn,tdt(1),tli,tdt(3),tdt(2),tdc(4),tdc(3),tdc(5),payrate pageoflow L760
00740 L740: form pos 34,pic(zzz),pos 47,pic(zz/zz/zz),pos 55,pic(------.##),pos 66,pic(zz/zz/zz),pos 75,pic(zz/zz/zz),pos 93,pic(---,---.##),pos 113,3*n 10.2,skip 1
00750   goto L780
00760 L760: pr #255: newpage
00770   gosub HDR
00780 L780: return 
00790 ! ______________________________________________________________________
00800 DONE: ! 
00810   close #1: ioerr ignore
00820   close #2: ioerr ignore
00830   fncloseprn
00840   goto XIT
00850 ! ______________________________________________________________________
00860 HDR: ! 
00870   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00880   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00890   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
00900   pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
00910   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00920   pr #255: "\ql   "
00930   pr #255,using L940: "Employee    Employee Name","Dept   Date   Last Rev", "Last Increase   Next Rev     Vacation Hours","   Sick Hours   Hol Hours"
00940 L940: form pos 1,c 25,pos 33,c 22,pos 59,c 43,pos 108,c 25
00950   pr #255,using L960: "Number","Hired     Date     Amount      Date","Date     Accrued     Taken   Accrued     Taken     Taken  Pay Rate"
00960 L960: form pos 2,c 6,pos 39,c 35,pos 77,c 66
00970   pr #255: ""
00980   return 
00990 ! ______________________________________________________________________
01000 ! <Updateable Region: ERTN>
01010 ERTN: fnerror(program$,err,line,act$,"xit")
01020   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01040   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01050 ERTN_EXEC_ACT: execute act$ : goto ERTN
01060 ! /region
01070 ! ______________________________________________________________________
01080 XIT: fnxit
01090 ! ______________________________________________________________________
01100 ! CHECK_PASSWORD: ! 
01110 !   library 'S:\Core\Library': fntos,fnlbl,fntxt,fnacs,fncmdkey,fncmdset
01120 !   return ! if env$('client')="Washington Parrish" then goto L1130 else return 
01130 ! L1130: if wsid$="09" or wsid$="99" then return 
01140 ! L1140: fntos(sn$="WpTrap") !:
      !   respc=0 : mylen=25 : mypos=mylen+2
01150 !   fnlbl(1,1,"         Quit!      ",mylen,2)
01160 !   fnlbl(2,1,"Stay out of Payroll!",mylen,2)
01170 !   fnlbl(3,1,"Call Brenda for Password",mylen,2)
01180 !   fntxt(3,mylen+3,8,8,1,"",0,"You must have a password to get out.") !:
      !   resp$(respc+=1)=""
01190 !   fncmdkey("E&xit",5,1,1,"Returns to menu")
01200 !   fnacs(sn$,0,mat resp$,ckey)
01210 !   if trim$(uprc$(resp$(1)))="GETMEOUT" then goto XIT else goto L1140
