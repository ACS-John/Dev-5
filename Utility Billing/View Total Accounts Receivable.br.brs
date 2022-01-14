fn_setup
fnTop(program$)
fn_scanCustomerFile(totalAr,activeCount)
do 
	fnTos
	mylen=28 : mypos=mylen+2 : rc=lc=0
	fnLbl(lc+=1,1,"Total Accounts Receivable:",28,1) : fnTxt(lc,mypos,18,0,1,'10',1) : resp$(rc+=1)=str$(totalAr)
	fnLbl(lc+=1,1,"Total Accounts Receivable:",28,1) : fnTxt(lc,mypos,18,0,1,'10',1) : resp$(rc+=1)=str$(totalAr)
	fnChk(lc+=1,mypos,'Exclude Final Billed', 1)
	resp$(respc_excludeFianlBilled=rc+=1)=excludeFinalBilled$
	fnCmdKey('Close',5,0,1,'Save option(s) and exit')
	fnCmdKey('Refresh',1,1,0,'Recalculate')
	ckey=fnAcs(mat resp$)
	excludeFinalBilled$=resp$(respc_excludeFianlBilled)
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
	fn_scanCustomerFile(totalBal,activeCount)
	fn_total_ar=totalBal
fnend
def fn_scanCustomerFile(&totalBal,&activeCount)
	totalBal=activeCount=0
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],Shr",internal,input
	do 
		read #hCustomer,using "form pos 292,PD 4.2,PD 4,pos 1821,N 1": bal,customerBillingDate,finalBillingCode eof TA_FINIS
		if excludeFinalBilled$='False' or finalBillingCode=0 then 
			! finalBillingCode   (0 Active, 1 Inactive/Final Billed, 2 Inactive/Deposit Refunded, 3 Active/but Do Not Bill, 4 Finaled/but Not Billed)
			if finalBillingCode=0 or finalBillingCode=3 then
				activeCount+=1
			end if
			! pr lastBillingDate,customerBillingDate : pause
			if ~filterByBillingDate or lastBillingDate=customerBillingDate then 
				totalBal+=bal
			end if
		end if
	loop 
	TA_FINIS: ! 
	close #hCustomer: 
fnend
def library fnActiveCustomerCount
	fn_setup
	fn_scanCustomerFile(totalBal,activeCount)
	fnActiveCustomerCount=activeCount
fnend
include: ertn
