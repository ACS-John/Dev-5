! formerly S:\acsPR\newprchangedate
! this program changes the default date of a payroll.  Can be used if you need to reprint some old registers, etc
fn_setup
fnTop(program$)
fn_ChangePayrollDates
goto Xit 
def fn_ChangePayrollDates
	dim resp$(10)*60
	fn_getPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	d1=fn_payPeriodEndingDate
	CpdTos: !
	fnTos
	rc=cf=0: mylen=27: mypos=mylen+2: frameno=1
	fnFra(1,1,1,42,"Payroll Date","Enter the payroll date.")
	fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Enter the date which you want used for your earnings records. ",frameno) 
	resp$(rc+=1)=str$(d1)
	fnFra(4,1,7,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.") 
	frameno=2 : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno) 
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno) 
	resp$(rc+=1)=str$(end_date)
	fnLbl(4,1,"1st Day of 1st quarter:",mylen,1,0,frameno)
	fnTxt(4,mypos,10,0,1,"3",0,"Enter the first day of the first quarter. Could be something other than January 1st if your last payroll of the previous year should be included in this year",frameno) 
	resp$(rc+=1)=str$(qtr1)
	fnLbl(5,1,"1st Day of 2nd quarter:",mylen,1,0,frameno)
	fnTxt(5,mypos,10,0,1,"3",0,"Normally would be April 1st, but could be different if your payroll dates and check dates are not the same.",frameno) 
	resp$(rc+=1)=str$(qtr2)
	fnLbl(6,1,"1st Day of 3rd quarter:",mylen,1,0,frameno)
	fnTxt(6,mypos,10,0,1,"3",0,"Normally would be July 1st",frameno) 
	resp$(rc+=1)=str$(qtr3)
	fnLbl(7,1,"1st Day of 4th quarter:",mylen,1,0,frameno)
	fnTxt(7,mypos,10,0,1,"3",0,"Normally would be October 1st.",frameno) 
	resp$(rc+=1)=str$(qtr4)
	fnCmdKey('Year Backward',3,0,0,'Adjust date range years lower one')
	fnCmdKey('Year Forward',2,0,0,'Adjust date range years higher one')
	fnCmdKey("Save",1,1,0,"Save and Continue")
	fnCmdKey("Cancel",5,0,1,"Close without Saving")
	ckey=fnAcs(mat resp$)
	if ckey<>5 then 
		prd=d1=val(resp$(1))
		beg_date=val(resp$(2)) 
		end_date=val(resp$(3)) 
		qtr1=val(resp$(4)) 
		qtr2=val(resp$(5)) 
		qtr3=val(resp$(6)) 
		qtr4=val(resp$(7))
		! qtr5=val(resp$(7)(1:4))*10000+1231
		! begin_year=val(resp$(7)(1:4))*10000+0101
		! end_year=val(resp$(7)(1:4))*10000+1231
		if ckey=2 then
			beg_date+=10000
			end_date+=10000
			qtr1+=10000
			qtr2+=10000
			qtr3+=10000
			qtr4+=10000
			goto CpdTos
		else if ckey=3 then
			beg_date-=10000
			end_date-=10000
			qtr1-=10000
			qtr2-=10000
			qtr3-=10000
			qtr4-=10000
			goto CpdTos
		end if
		fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
		fn_payPeriodEndingDate(d1)
	end if
fnend
Xit: fnXit

def library fnGetPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4)
	if ~setup then fn_setup
	fnGetPayrollDates=fn_getPayrollDates(beg_date,end_date, qtr1,qtr2,qtr3,qtr4)
fnend
def fn_getPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4)
	GpdTop: !
	open #hDates=fnH: "Name=[Q]\PRmstr\Dates.h[cno]",internal,input,relative ioerr GpdNoRec
	read #hDates,using "form pos 1,6*n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4 noRec GpdNoRec
	close #hDates:
	goto GpdFinis
	GpdNoRec: ! r:
	close #hDates:
	fn_payPeriodEndingDate(0)
	goto GpdTop ! /r
	GpdFinis: !
fnend
def fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4; d1)
	open #hDates=fnH: "Name=[Q]\PRmstr\Dates.h[cno],use,RecL=76",internal,outIn,relative
	PutPayrollDatesReWrite: !
	if d1 then
		rewrite #hDates,using "form pos 1,6*n 8,n 8"	,rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1 noRec PutPayrollDatesNoRec
	else
		rewrite #hDates,using "form pos 1,6*n 8"     	,rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4 noRec PutPayrollDatesNoRec
	end if
	goto XitPutPayrollDates

	PutPayrollDatesNoRec: ! 
	write #hDates,using "form pos 1,6*n 8,n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1
	goto XitPutPayrollDates

	XitPutPayrollDates: !
	close #hDates:
fnend
def library fnSetPayrollDatesForYear(; setYear)
	if ~setup then fn_setup
	fnSetPayrollDatesForYear=fn_setPayrollDatesForYear( setYear)
fnend
def fn_setPayrollDatesForYear(; setYear)
	if ~setYear then setYear=Date('ccyy')
	beg_date	=val(str$(setYear)&'0101')
	end_date	=val(str$(setYear)&'1231')
	qtr1    	=val(str$(setYear)&'0101')
	qtr2    	=val(str$(setYear)&'0401')
	qtr3    	=val(str$(setYear)&'0701')
	qtr4    	=val(str$(setYear)&'1001')
	fn_putPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
fnend
def library fnPayPeriodEndingDate(; setIt)
	if ~setup then fn_setup
	fnPayPeriodEndingDate=fn_payPeriodEndingDate( setIt)
fnend
def fn_payPeriodEndingDate(; setIt,hPrDates)
	if setIt then
		open #hPrDates=fnH: "Name=[Q]\PRmstr\Dates.h[cno],Shr",internal,outin,relative 
		rewrite #hPrDates,using "form pos 1,x 48,n 8",rec=1: setIt
		close #hPrDates: 
	else
		open #hPrDates=fnH: "Name=[Q]\PRmstr\Dates.h[cno],Shr",internal,input,relative 
		read #hPrDates,using "form pos 1,x 48,n 8",rec=1: setIt
		close #hPrDates: 
	end if
	fn_payPeriodEndingDate=setIt
fnend
def library fnCompanyPayPeriodEndingDate(cno; ___,returnN)
	if ~setup then fn_setup
	open #hPrDates=fnH: 'Name=[Q]\PRmstr\Dates.h'&str$(cno)&',Shr',internal,input,relative ioerr CppedFinis
	read #hPrDates,using "form pos 1,x 48,n 8",rec=1: returnN
	close #hPrDates: 
	CppedFinis: !
	fnCompanyPayPeriodEndingDate=returnN
fnend

include: fn_setup
