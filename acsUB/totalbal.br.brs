! Replace S:\acsUB\totalBal
fn_setup
fnTop(program$,"View Total Accounts Receivable")
do 
	fnTos
	mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Total Accounts Receivable:",28,1)
	fnTxt(1,mypos,18,0,1,"10",1)
	resp$(1)=str$(fn_total_ar)
	fnChk(3,mypos,'Exclude Final Billed', 1)
	resp$(2)=excludeFinalBilled$
	fnCmdKey('Close',5,0,1,'Save option(s) and exit')
	fnCmdKey('Refresh',1,1,0,'Recalculate')
	fnAcs2(mat resp$,ckey)
	excludeFinalBilled$=resp$(2)
	fncreg_write(env$('program_caption')&' - Exclude Final Billed',excludeFinalBilled$)
loop until ckey=5
Xit: fnXit
def fn_setup
	if ~setup then 
		setup=1
		autoLibrary
		on error goto Ertn
	end if 
	fnLastBillingDate(lastBillingDate)
	fncreg_read(env$('program_caption')&' - Exclude Final Billed',excludeFinalBilled$, 'False')
	! if env$('client')='French Settlement' then filterByBillingDate=1 else filterByBillingDate=0
fnend 
def library fntotal_ar
	fn_setup
	fntotal_ar=fn_total_ar
fnend 
def fn_total_ar
	totalBal=0
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],Shr",internal,input
	do 
		read #h_customer,using "Form POS 292,PD 4.2,PD 4,pos 1821,N 1": bal,customerBillingDate,finalBillingCode eof TA_FINIS
		if excludeFinalBilled$='False' or finalBillingCode=0 then 
			! pr lastBillingDate,customerBillingDate : pause
			if ~filterByBillingDate or lastBillingDate=customerBillingDate then 
				totalBal+=bal
			end if
		end if
	loop 
	TA_FINIS: ! 
	close #h_customer: 
	fn_total_ar=totalBal
fnend 
include: Ertn
