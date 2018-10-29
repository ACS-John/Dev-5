! formerly S:\acsPR\newprchangedate
! this program changes the default date of a payroll.  Can be used if you need to reprint some old registers, etc
fn_setup
dim cap$*128
fntop(program$,cap$="Change Payroll Date")
fn_ChangePayrollDates
goto XIT 
def fn_ChangePayrollDates
  dim d1$*20,resp$(10)*60
  fn_getPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
  fnTos(sn$="Calculation-1") 
  rc=cf=0: mylen=42: mypos=45: frameno=1
  fnFra(1,1,4,66,"Payroll Date","Enter the payroll date.")
  fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
  fnTxt(1,mypos,10,0,1,"3",0,"Enter the date which you want used for your earnings records. ",frameno) 
  resp$(rc+=1)=str$(d1)
  fnLbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
  fnTxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report headings, etc." ,frameno) 
  resp$(rc+=1)= d1$
  fnFra(7,25,6,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.") 
  frameno=2 : mylen=26 : mypos=mylen+2
  fnLbl(1,1,"Starting Date:",mylen,1,0,frameno)
  fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno) 
  resp$(rc+=1)=str$(beg_date)
  fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
  fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno) 
  resp$(rc+=1)=str$(end_date)
  fnLbl(3,1,"1st Day of 1st quarter:",mylen,1,0,frameno)
  fnTxt(3,mypos,10,0,1,"3",0,"Enter the first day of the first quarter. Could be something other than January 1st if your last payroll of the previous year should be included in this year",frameno) 
  resp$(rc+=1)=str$(qtr1)
  fnLbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,frameno)
  fnTxt(4,mypos,10,0,1,"3",0,"Normally would be April 1st, but could be different if your payroll dates and check dates are not the same.",frameno) 
  resp$(rc+=1)=str$(qtr2)
  fnLbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,frameno)
  fnTxt(5,mypos,10,0,1,"3",0,"Normally would be July 1st",frameno) 
  resp$(rc+=1)=str$(qtr3)
  fnLbl(6,1,"1st Day of 4th quarter:",mylen,1,0,frameno)
  fnTxt(6,mypos,10,0,1,"3",0,"Normally would be October 1st.",frameno) 
  resp$(rc+=1)=str$(qtr4)
  fnCmdKey("Next",1,1,0,"Save and Continue")
  fnCmdKey("Cancel",5,0,1,"Close without Saving")
  fnAcs(sn$,0,mat resp$,ckey)
  if ckey<>5 then 
    prd=d1=val(resp$(1))
    d1$=resp$(2)
    beg_date=val(resp$(3)) 
    end_date=val(resp$(4)) 
    qtr1=val(resp$(5)) 
    qtr2=val(resp$(6)) 
    qtr3=val(resp$(7)) 
    qtr4=val(resp$(8))
    qtr5=val(resp$(8)(1:4))*10000+1231
    begin_year=val(resp$(8)(1:4))*10000+0101
    end_year=val(resp$(8)(1:4))*10000+1231
    fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
  end if
fnend
XIT: fnxit
def fn_setup
  if ~setup then
    setup=1
    library 'S:\Core\Library': fntop,fnxit,fnerror,fnTos,fnFra,fnChk,fnLbl,fnTxt,fnCmdKey,fnAcs,fngethandle
    on error goto ERTN
   end if
fnend
def library fnGetPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4,&d1,&d1$)
  if ~setup then let fn_setup
  fnGetPayrollDates=fn_getPayrollDates(beg_date,end_date, qtr1,qtr2,qtr3,qtr4,d1,d1$)
fnend
def fn_getPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4,&d1,&d1$)
  GpdTop: !
  open #hDates:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno]",internal,input,relative ioerr GpdNoRec
  read #hDates,using "form pos 1,6*n 8,n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1 noRec GpdNoRec
  !
  d1$=rpt$(' ',20) soflow SkipReadingD1
  read #hDates,using "form pos 1,x 56,c 20",rec=1: d1$ noRec GpdNoRec
  SkipReadingD1: !
  d1$=rtrm$(d1$)
  !
  close #hDates:
  goto GpdFinis
  GpdNoRec: ! r:
  close #hDates:
  beg_date=val('0101'&date$('ccyy'))
  end_date=val('1231'&date$('ccyy'))
  qtr1    =val('0101'&date$('ccyy'))
  qtr2    =val('0401'&date$('ccyy'))
  qtr3    =val('0701'&date$('ccyy'))
  qtr4    =val('1001'&date$('ccyy'))
  fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,0,'')
  goto GpdTop ! /r
  GpdFinis: !
fnend
def fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$*20)
  open #hDates:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno],USE,RecL=76",internal,outIn,relative 
  rewrite #hDates,using "form pos 1,6*n 8,n 8,c 20",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$
  close #hDates: 
fnend
include: ertn
