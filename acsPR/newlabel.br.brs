00010 ! Replace S:\acsPR\newLabel
00020 ! Payroll Labels
00030 ! r: setup
00040   library 'S:\Core\Library': fntop,fnxit,fnwait,fnerror,fnaddlabel,fnlabel,fncomboa,fntos,fnlbl,fnchk,fncombof,fncmdset,fnacs,fntxt,fndate_mmddyy_to_ccyymmdd,fncmdkey,fnGetPayrollDates
00050   on error goto ERTN
00060   ! ______________________________________________________________________
00070   dim in1(9),io1$(8),lb$(6,6)*40,ln$*260,msgline$(2)*60,response$(5)*1
00080   dim resp$(10)*40
00090   dim em$(3)*30,ss$*11,in$*1,cap$*128,log$*128
00100   dim labeltext$(5)*120,pt$(5),message$*40,tcp(32),tdc(10)
00110   dim item1$(4)*40
00120   dim rs(2) ! RS(1)=Race RS(2)=Sex
00130   dim em(17) ! see PR Master File Layout  em(4)=employment status
00140   ! ______________________________________________________________________
00150   fntop(program$,cap$="Labels")
00180   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
00190 ! /r
00200   gosub SCR1
00210   gosub OPEN_KEYED
00220   goto GET_STARTED
00240 ! ______________________________________________________________________
00250 OPEN_KEYED: ! r:
00260   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00270   L270: form pos 1,n 8,3*c 30,c 11,pos 110,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6
00280   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00290 return ! /r
00310 GET_STARTED: ! r:
00330   lb1=0
00340   mat lb$=("")
00350   eno$=lpad$(str$(eno),8)
00360 L360: if sey$="Y" then gosub ASK_EMP : goto JUST_AFTER_READ else goto READ_SEQUENTIAL
00370 ! /r
00380 READ_SEQUENTIAL: ! r:
00390   let foundone=0
00400 L400: read #1,using L270: eno,mat em$,ss$,mat rs,mat em eof FINIS
00410   if prtall=2 and fndate_mmddyy_to_ccyymmdd(em(17))=d1 then goto JUST_AFTER_READ
00420   if prtall=2 and fndate_mmddyy_to_ccyymmdd(em(17))<>d1 then goto READ_SEQUENTIAL
00430   if date_to_select>0 and date_to_select<>d1 then gosub CHECK_FOR_OLD_DATE
00440   if date_to_select >0 and em(17)=0 then goto L400 ! pr new people also
00450   if foundone=1 then goto L470 ! found an old matching date
00460   if date_to_select >0 and fndate_mmddyy_to_ccyymmdd(em(17))<>date_to_select then goto L400 ! only last payroll date
00470 L470: if empstatuse>0 and em(4)<>empstatuse then goto L400 ! based on employment status
00480   if starting_employee<>0 and eno< starting_employee then goto L400
00490 JUST_AFTER_READ: ! 
00500   if lb1=>1 then gosub PRINT_LABEL
00510   lb1=lb1+1
00520   if empyn$="Y" then lb$(lb1,1)=str$(eno)&"  "
00530   if ssyn$="Y" then lb$(lb1,1)=lb$(lb1,1)&ss$&"  "
00540   if date_to_print>0 and ssyn$="N" then !:
          lb$(lb1,1)=lb$(lb1,1)&cnvrt$("PIC(ZZ/ZZ/ZZ)",date_to_print)
00550   if date_to_print>0 and ssyn$="Y" then !:
          lb$(lb1,5)=lb$(lb1,5)&cnvrt$("PIC(ZZ/ZZ/ZZ)",date_to_print)
00560   lb$(lb1,2)=em$(1)
00570   if empadryn$="Y" then lb$(lb1,3)=em$(2)
00580   if empadryn$="Y" then lb$(lb1,4)=em$(3)
00590   goto L360
00600 ! /r
00610 CHECK_FOR_OLD_DATE: ! r:
00620   let foundone=0
00630   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00640   restore #4,key>=checkkey$: nokey L690
00650 L650: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L690
00660   if heno<>eno then goto L690
00670   if prd=date_to_select then let foundone=1 : goto L690 ! FOUND A MATCHING OLD PAYROLL DATE
00680   goto L650
00690 L690: return ! /r
00720 FINIS: ! r:
00722   close #1: ioerr ignore
00740   if lb1>0 then gosub PRINT_LABEL
00750   close #2: ioerr ignore
00760   fnlabel(101,cap$,mat pt$,cp,nw) 
00762 goto XIT ! /r
00770 XIT: fnxit
00790 ASK_EMP: ! r: 
00800   respc=0
00810   fntos(sn$="prlabel-2")
00820   fnlbl(1,1,"Employee Number to Print:",25,1)
00830   fncombof("Employee",1,28,20,env$('Q')&"\PRmstr\rpmstr.h"&env$('cno'),1,8,9,20,env$('Q')&"\PRmstr\Rpindex.h"&env$('cno'),1,0, "Select any employee number you wish printed") !:
        resp$(respc+=1)=""
00840   fncmdkey("&Next",1,1,0,"Add this employee to list of labels to be printed.")
00850   fncmdkey("&Complete",2,0,0,"Print selected labels.")
00860   fnacs(sn$,0,mat resp$,ck) !:
        if ck=2 then goto FINIS
00870   eno=val(resp$(1)(1:8))
00880   if eno=0 then goto FINIS
00890   read #1,using L270,key=lpad$(str$(eno),8): eno,mat em$,ss$,mat rs,mat em nokey ASK_EMP
00900   return ! /r
00920 SCR1: ! r:
00940   fntos(sn$="prlabel-1")
00950   respc=0 : mylen=50 : mypos=mylen+3 : right=1
00960   fnlbl(1,1,"Print Labels For:",mylen,right)
00970   let fi$="cllabels" !:
        item1$(print_all=1)="[All]" : all=1 !:
        item1$(2)="Employees from last payroll only": last_payroll=2 !:
        item1$(3)="Select employees to print": select_employee=3 !:
        item1$(4)="Select employment status to print": emp_status=4 !:
        fncomboa(fi$,1,mypos,mat item1$,"Print labels based on certain employment status code. (Eg. all full time).") !:
        resp$(respc+=1)=item1$(1)
00980   fnchk(5,mypos+2,'Print Employee Number on Label:',right) !:
        resp$(respc+=1)='False'
00990   fnchk(6,mypos+2,'Print Social Security Number on Label:',right) !:
        resp$(respc+=1)='False'
01000   fnchk(7,mypos+2,'Print Employee Address on Label:',right) !:
        resp$(respc+=1)='True'
01010   fnlbl(9,1,"Payroll Date to Use (if applicable):",mylen,1)
01020   fntxt(9,mypos,12,0,1,"3",0,'You can pr labels for any payroll period. Only applicable if printing labels for those employees who got paid."') ! !:
        resp$(respc+=1)=""
01030   fnlbl(10,1,"Payroll Date to pr on Label:",mylen,1)
01040   fntxt(10,mypos,12,0,1,"1",0,'Used for placing a date on the label. Leave blank if not applilcable."') ! !:
        resp$(respc+=1)=""
01050   fnlbl(11,1,"Employment Status to pr (if applicable):",mylen,1)
01060   fntxt(11,mypos,2,0,1,"30",0,'Used for selectiing a specific employment status code. Leave blank if not applilcable."') ! !:
        resp$(respc+=1)=""
01070   fnlbl(13,1,"Starting Employee Number (if applicable):",mylen,1)
01080   fncombof("Employee",13,mypos,20,env$('Q')&"\PRmstr\rpmstr.h"&env$('cno'),1,8,9,20,env$('Q')&"\PRmstr\Rpindex.h"&env$('cno'),1,0, "Select starting employee record for printing. Only applicable if not starting with first employee.") !:
        resp$(respc+=1)=""
01090   fncmdset(2)
01100   fnacs(sn$,0,mat resp$,ck) !:
        if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then prtall=all else !:
            if resp$(1)=item1$(2) then prtall=last_payroll else !:
              if resp$(1)=item1$(3) then prtall=select_employee else !:
                if resp$(1)=item1$(4) then prtall=emp_status
01110   if resp$(2) ="True" then empyn$="Y" else empyn$="N"
01120   if resp$(3) ="True" then ssyn$="Y" else ssn$="N"
01130   if resp$(4) ="True" then empadryn$="Y" else empadryn$="N"
01140   date_to_select=val(resp$(5)) ! payroll date to use in selecting employees
01150   date_to_print=val(resp$(6)) ! payroll date to pr on label
01160   empstatuse=val(resp$(7)) ! employment status used as criteria
01170   starting_employee=val(resp$(8)(1:8)) ! starting employee #
01180   if prtall=3 then sey$="Y" else sey$="N" !:
          ! CHOSEN TO SELECT EMPLOYEE TO PRINT
01190   return ! /r
01210 PRINT_LABEL: ! r:
01220   mat labeltext$=("")
01230   for j1=1 to 5
01240     ln$="       "
01250     for j2=1 to lb1 : ln$=ln$&rpad$(lb$(j2,j1)(1:40),42) : next j2
01260     labeltext$(j1)=ln$
01270   next j1
01280   fnaddlabel(mat labeltext$)
01290   lb1=0 : mat lb$=("")
01300   return ! /r
01320 ! <Updateable Region: ERTN>
01330 ERTN: fnerror(program$,err,line,act$,"xit")
01340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01370 ERTN_EXEC_ACT: execute act$ : goto ERTN
01380 ! /region
