autoLibrary
on error goto Ertn
fnTop(program$)

dim billingdate$*10
dim msgtext$(1)*1000
dim readings(12),charges(12)
dim breakdown(10)
dim readingdates(2)

remove_total=2
do
	cont=fn_options(route,billingdate$) ! collect user options
	if trim$(billingdate$)="0" then valid=0 else valid=1
	mat msgtext$(1:1)=("You must enter a billing date")
	if valid=0 then let fnmsgbox(mat msgtext$,answer$,"Invalid Entry",0)
loop while not valid

mat msgtext$(5)
msgtext$(1) = "Warning:"
msgtext$(2) = "This action will reduce the balance and balance breakdown of all customers "
if route <>0 then msgtext$(1)=msgtext$(1)&" (within route "&str$(route)&")"
msgtext$(3) = "by "&str$(remove_total)&"x the amount of the billing on "&billingdate$&'.'
msgtext$(4) = "This action is irreversible and should only be performed by an ACS Technician."
msgtext$(5) = "Do you want to continue?"
fnmsgbox(mat msgtext$,answer$,"Confirm Action",4)
if (answer$<>"Yes") then cont=0

undocount=0
if cont then
	dim acct$*10,custname$*30,trcust$(3)*10,trdate(3)
	Fcustomer: form c 10,x 30,c 30,pos 1741,n 2,pos 217,12*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1750,2*n 6
	fn_openfiles ! open data files
	open #h_iphold=fnH: "Name=[Q]\UBmstr\IpHold7.h[cno]",i,i
	fnopenprn : fn_printheader
	do
		NEXT_CUSTOMER: !
		read #h_iphold,using 'form pos 1,C 10': z$ eof CUSTDONE
		read #hCustomer,using Fcustomer,key=z$: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE
		!       read #hCustomer,using Fcustomer: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE
		! if trim$(acct$)='107000.00' then pause
		if chargedate=val(billingdate$) then
			if route=0 or custroute=route then ! if a route was selected and customer doesn't match, skip customer
				if fn_get_trans then ! get latest and 2 prior charge transactions for this customer
					undocount+=1
					for remove_item=1 to remove_total
						balance=balance-tamt
						for item=1 to 9 ! assuming 10 is the penalty
							breakdown(item)=breakdown(item)-tg(item)
						next item
					next remove_item
					rewrite #hCustomer,using Fcustomer: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
					pr #255,using "form pos 5,c 10,x 5,pic(zz/zz/zz),X 5,N 10.2": acct$,str$(chargedate),balance pageoflow PRINTPAGEOVERFLOW
				else
					pr #255: "Could not find transaction for account "&acct$
				end if
			end if
		end if
	loop
goto CUSTDONE
PRINTPAGEOVERFLOW: !
	pr #255: newpage
	fn_printheader
continue
CUSTDONE: !
	mat msgtext$(1)=("Customers reversed: "&str$(undocount))
	fnmsgbox(mat msgtext$,answer$,"Report",0)
	fncloseprn
end if
Xit: fnXit
def fn_options(&route,&billingdate$) ! show options dialog to user and return selections
	dim screen_name$*100,resp$(4)*255
	fnTos
	! screen instructions
	fnLbl(2,2,"Use the options below to limit the customers to reverse.")
	fnLbl(3,2,"Warning: only the most recent billing date can be reversed for any account.")

	! combo for route selection
	fnLbl(7,2,"Select a route (or undo all):")
	fncmbrt2(7,35)
	resp$(1)="[All]"

	! billing date text box
	fnLbl(9,2,"Billing date:")
	fnTxt(9,35,8,0,0,"1")
	fnLastBillingDate(lastbilling) ! get last billing date and use it for the default
	resp$(2)=str$(lastbilling)

	fnCmdSet(2) ! show "Next" and "Cancel" buttons

	ckey=fnAcs(mat resp$) ! run the screen

	if ckey=5 then ! if user pressed Cancel
		fn_options=0
	else
		if resp$(1)="[All]" then route=0 else route=val(resp$(1))
		billingdate$=resp$(2)

		fn_options=1
	end if
fnend
def fn_openfiles
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",i,outIn,k
	open #hTrans=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno]",i,i,k
	! open #hTrans2=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno]",i,outIn,k
fnend
def fn_printheader
	pg+=1
	pr #255: env$('program_caption')
	pr #255: "Page "&str$(pg)
	pr #255: ""
	pr #255: "All accounts listed have been modified."
	pr #255: ""
	pr #255: "Account           Billing Date"
	pr #255: "_______________   ____________"
fnend

def fn_get_trans
	dim transacct$*10

	gt_return=0
	dateshouldbe=date(days(val(billingdate$),"mmddyy"),"ccyymmdd") : if str$(dateshouldbe)(1:2)="19" then dateshouldbe+=1000000

	read #hTrans,using Ftrans,key=lpad$(acct$,10)&str$(dateshouldbe)&"1": transacct$,transdate,transcode,tamt,mat tg,tnet,wread,wused,tbal,pcode nokey GT_FINIS
	Ftrans: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
	gt_return=1

	GT_FINIS: !
	fn_get_trans=gt_return
fnend
include: ertn
