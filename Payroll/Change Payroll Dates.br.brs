00010 ! formerly S:\acsPR\newprchangedate
00020 ! this program changes the default date of a payroll.  Can be used if you need to reprint some old registers, etc
00030 fn_setup
00080 dim cap$*128
00050 fntop(program$,cap$="Change Payroll Date")
20000 fn_ChangePayrollDates
20020 goto XIT 
32000 def fn_ChangePayrollDates
32020   dim d1$*20,resp$(10)*60
32040   fn_getPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
34000   fnTos(sn$="Calculation-1") 
34020   rc=cf=0: mylen=42: mypos=45: frameno=1
34040   fnFra(1,1,4,66,"Payroll Date","Enter the payroll date.")
34060   fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
34080   fnTxt(1,mypos,10,0,1,"3",0,"Enter the date which you want used for your earnings records. ",frameno) 
34100   resp$(rc+=1)=str$(d1)
34120   fnLbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
34140   fnTxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report headings, etc." ,frameno) 
34160   resp$(rc+=1)= d1$
34180   fnFra(7,25,6,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.") 
34200   frameno=2 : mylen=26 : mypos=mylen+2
34220   fnLbl(1,1,"Starting Date:",mylen,1,0,frameno)
34240   fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno) 
34260   resp$(rc+=1)=str$(beg_date)
34280   fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
34300   fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno) 
34320   resp$(rc+=1)=str$(end_date)
34340   fnLbl(3,1,"1st Day of 1st quarter:",mylen,1,0,frameno)
34360   fnTxt(3,mypos,10,0,1,"3",0,"Enter the first day of the first quarter. Could be something other than January 1st if your last payroll of the previous year should be included in this year",frameno) 
34380   resp$(rc+=1)=str$(qtr1)
34400   fnLbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,frameno)
34420   fnTxt(4,mypos,10,0,1,"3",0,"Normally would be April 1st, but could be different if your payroll dates and check dates are not the same.",frameno) 
34440   resp$(rc+=1)=str$(qtr2)
34460   fnLbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,frameno)
34480   fnTxt(5,mypos,10,0,1,"3",0,"Normally would be July 1st",frameno) 
34500   resp$(rc+=1)=str$(qtr3)
34520   fnLbl(6,1,"1st Day of 4th quarter:",mylen,1,0,frameno)
34540   fnTxt(6,mypos,10,0,1,"3",0,"Normally would be October 1st.",frameno) 
34560   resp$(rc+=1)=str$(qtr4)
34580   fnCmdKey("Next",1,1,0,"Save and Continue")
34600   fnCmdKey("Cancel",5,0,1,"Close without Saving")
34620   fnAcs(sn$,0,mat resp$,ckey)
36000   if ckey<>5 then 
36020     prd=d1=val(resp$(1))
36040     d1$=resp$(2)
36080     beg_date=val(resp$(3)) 
36100     end_date=val(resp$(4)) 
36120     qtr1=val(resp$(5)) 
36140     qtr2=val(resp$(6)) 
36160     qtr3=val(resp$(7)) 
36180     qtr4=val(resp$(8))
36200     qtr5=val(resp$(8)(1:4))*10000+1231
36220     begin_year=val(resp$(8)(1:4))*10000+0101
36240     end_year=val(resp$(8)(1:4))*10000+1231
36260     fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
36280   end if
36300 fnend
55000 XIT: fnxit
60000 def fn_setup
60020   if ~setup then
60040     setup=1
60060     library 'S:\Core\Library': fntop,fnxit,fnerror,fnTos,fnFra,fnChk,fnLbl,fnTxt,fnCmdKey,fnAcs,fngethandle
60080     on error goto ERTN
60100    end if
60120 fnend
70000 def library fnGetPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4,&d1,&d1$)
70020   if ~setup then let fn_setup
70040   fnGetPayrollDates=fn_getPayrollDates(beg_date,end_date, qtr1,qtr2,qtr3,qtr4,d1,d1$)
70060 fnend
72000 def fn_getPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4,&d1,&d1$)
72020   GpdTop: !
72040   open #hDates:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno]",internal,input,relative ioerr GpdNoRec
72060   read #hDates,using "form pos 1,6*n 8,n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1 noRec GpdNoRec
72080   !
72100   d1$=rpt$(' ',20) soflow SkipReadingD1
72120   read #hDates,using "form pos 1,x 56,c 20",rec=1: d1$ noRec GpdNoRec
72140   SkipReadingD1: !
72160   d1$=rtrm$(d1$)
72180   !
72200   close #hDates:
72220   goto GpdFinis
73000   GpdNoRec: ! r:
73020   close #hDates:
73040   beg_date=val('0101'&date$('ccyy'))
73060   end_date=val('1231'&date$('ccyy'))
73080   qtr1    =val('0101'&date$('ccyy'))
73100   qtr2    =val('0401'&date$('ccyy'))
73120   qtr3    =val('0701'&date$('ccyy'))
73140   qtr4    =val('1001'&date$('ccyy'))
73160   fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,0,'')
73180   goto GpdTop ! /r
74000   GpdFinis: !
74020 fnend
84000 def fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$*20)
84020   open #hDates:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno],USE,RecL=76",internal,outIn,relative 
84040   rewrite #hDates,using "form pos 1,6*n 8,n 8,c 20",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$
84060   close #hDates: 
84080 fnend
96020 ! <updateable region: ertn>
96040 ERTN: fnerror(program$,err,line,act$,"xit")
96060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
96080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
96100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
96120 ERTN_EXEC_ACT: execute act$ : goto ERTN
96140 ! </updateable region: ertn>
96510 ! ERTN: fnerror(program$,err,line,act$,"NO") ! r:
96520 !   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
96530 !   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
96540 !   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
96550 ! ERTN_EXEC_ACT: execute act$ : goto ERTN ! /r