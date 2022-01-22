	autoLibrary
library program$: fnworkOrderAdd
library 'S:\Core\Library': fnXit,fnTop,fnask_account,fnH
fnTop(program$)
open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k 
do
	if fnask_account('Work Order',z$,hCustomer)=5 then 
		goto Xit
	else
		fnworkOrderAdd(z$)
	end if 
loop
Xit: !
close #hCustomer:
fnXit
def library fnworkOrderAdd(z$*10)
	if ~wo_setup then ! r:
		wo_setup=1
		autoLibrary
		on error goto Ertn
		dim resp$(128)*512
		dim i$(16)*320
		dim line$(5)*100
		dim customer_name$*30
		dim customer_phone_number$*12
		dim z$*10
		dim e$(4)*30
		dim f$(3)*12
		dim a(7)
		dim b(11)
		dim c(4)
		dim d(15)
		dim g(12)
		dim adr(2)
		dim alp$*7
		dim gb(10)
		dim rw4(22,13)
		dim extra(23)
		dim extra$(11)*30
	end if ! /r

	dat$=date$("Month DD, CCYY")
	open #wo_hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k 
	! really only need these: (z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
	read #wo_hCustomer,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
	cell$=extra$(8)
	F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	customer_name$=e$(2)
	customer_phone_number$=extra$(2)
	close #wo_hCustomer:

	WO_TOS: !
	fnTos(sn$="workorder")
	respc=0 : lc=0
	fnLbl(lc+=1,30,"WORK ORDER",20,0,4)
	fnLbl(lc+=1,1,"Account:",10,1)
	fnTxt(lc,12,10,0,1,"",1)
	resp$(respc_accont=respc+=1)=z$
	fnLbl(lc,24,"Name:",5,1)
	fnTxt(lc,31,25,30,0,"",1)
	lc+=1
	resp$(respc+=1)=customer_name$
	fnLbl(lc+=1,1,"Date Order Taken:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=dat$
	fnLbl(lc+=1,1,"Taken by:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Date Last Reading:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=""
	lc+=1
	fnLbl(lc+=1,1,"Date to be Completed:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Request made by:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Phone:",23,1)
	fnTxt(lc,25,14)
	resp$(respc+=1)=customer_phone_number$
	fnLbl(lc+=1,1,'Cell Phone:',23,1)
	fnTxt(lc,25,14)
	resp$(resp_cell:=respc+=1)=cell$
	lc+=1
	fnLbl(lc+=1,1,"Name In:",23,1)
	fnTxt(lc,25,25)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Turn On:",23,1)
	fnTxt(lc,25,8,0,0,"1")
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Turn Off:",23,1)
	fnTxt(lc,25,8,0,0,"1")
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Leave On:",23,1)
	fnTxt(lc,25,8)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Forwarding Address:",23,1)
	fnTxt(lc,25,30)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Forwarding City St Zip:",23,1)
	fnTxt(lc,25,30)
	resp$(respc+=1)=""
	fnLbl(lc+=1,1,"Comments:",23,1)
	fnTxt(lc,25,50,280)
	resp$(resp_comment:=respc+=1)=""
	fnCmdKey("Print History",8,0,0,"This allows you to review the description of any work order issued in the past")
	fnCmdKey("&Print",1,1,0,"Prints a workorder on this customer for the information entered above.")
	fnCmdKey("&Cancel",5,0,1,"Returns to main customer record.")
	ckey=fnAcs(mat resp$) ! work order screen

	if ckey=5 then goto woaXIT
	z$=resp$(respc_accont)(1:10) ! lpad$(trim$(resp$(respc_accont)(1:10)),10)
	if ckey=8 then let fnWorkOrderList(z$) : goto WO_TOS
	! for j=3 to 15 : i$(j-2)=resp$(j) : next j
	i$(1 )=resp$(3 )
	i$(2 )=resp$(4 )
	i$(3 )=resp$(5 )
	i$(4 )=resp$(6 )
	i$(5 )=resp$(7 )
	i$(6 )=resp$(8 ) ! phone number
	cell$ =resp$(resp_cell)
	i$(7 )=resp$(10)
	i$(8 )=resp$(11)
	i$(9 )=resp$(12)
	i$(10)=resp$(13)
	i$(11)=resp$(14)
	i$(12)=resp$(15)
	i$(13)=resp$(resp_comment)
	for j=1 to 12
		if trim$(resp$(j+2))="" then i$(j)="________________"
	next j
	for j=2 to 12
		if i$(j)(1:5)<>"_____" then i$(j)="{\ul "&i$(j)&"}" ! underline the answer if there was one
	next j
	y=55: z=1
	for j=1 to 5
		x=pos(i$(13)," ",y)
		if x>0 and x<=j*70 then 
			line$(j)=i$(13)(z:x)
			z=x+1
			y=x+55
		end if 
		if x=0 or x>j*70 then 
			line$(j)=i$(13)(z:j*70)
			y=z+70
			z=z+70
		end if 
	next j

	fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$, cell$)

	 ! r: write to WorkOrder History file (z$)
		open #h_workorder=fnH: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",i,outIn,k
		write #h_workorder,using "form pos 1,Cr 10,n 8,c 30,5*c 100": z$,date('ccyymmdd'),customer_name$,mat line$
		close #h_workorder: 
	! /r
	! r: append to note file
		open #h_notefile=fnH: "Name="&fnNoteDir$&"\"&trim$(z$)&".txt,Use",d,o
		pr #h_notefile: '** Work Order added '&date$('mm/dd/ccyy')&' at '&time$&' **'
		pr #h_notefile:   '              Account: '&z$&'  '&customer_name$
		if fn_not_blank(i$(5)) then
			pr #h_notefile: "      Request made by: "&fn_clean_ul$(i$(5))
		end if
		if fn_not_blank(i$(2)) then
			pr #h_notefile: "             Taken by: "&fn_clean_ul$(i$(2))
		end if
		if fn_not_blank(i$(7)) then
			pr #h_notefile: "              Name In: "&fn_clean_ul$(i$(7))
		end if
		if fn_not_blank(i$(3)) then
			pr #h_notefile: "    Date Last Reading: "&fn_clean_ul$(i$(3))
		end if
		if fn_not_blank(i$(4)) then
			pr #h_notefile: " Date to be Completed: "&fn_clean_ul$(i$(4))
		end if
		if fn_not_blank(i$(8)) then
			pr #h_notefile: "              Turn On: "&date$(days(fn_clean_ul$(i$(8),1008),'mmddyy'),'mm/dd/ccyy') ! fn_clean_ul$(i$(8)) : pause
		end if
		if fn_not_blank(i$(9)) then
			pr #h_notefile: "             Turn Off: "&date$(days(fn_clean_ul$(i$(9),1008),'mmddyy'),'mm/dd/ccyy')
		end if
		if fn_not_blank(i$(10)) then
			pr #h_notefile: "             Leave On: "&fn_clean_ul$(i$(10))
		end if
		if fn_not_blank(i$(11)&i$(12)) then
			pr #h_notefile: "   Forwarding Address: "&fn_clean_ul$(i$(11))&"  "&fn_clean_ul$(i$(12))
		end if
		for lineItem=1 to udim(mat line$)
			if trim$(line$(lineItem))<>'' then
				pr #h_notefile: '  '&line$(lineItem)
			end if
		nex lineItem
		pr #h_notefile: '**'
		close #h_notefile: 
	! /r
	woaXIT: !
fnend
def fn_clean_ul$*256(cu_in$*256; cu_reformat)
	cu_len=len(rtrm$(cu_in$))
	if cu_in$(1:5)='{\ul ' and cu_in$(cu_len:cu_len)='}' then 
		cu_in$(cu_len:cu_len)=''
		cu_in$(1:5)=''
	end if
	if cu_reformat=1008 then ! means that it is a date and it may need a leading zero added
		if len(cu_in$)<8 then
			cu_in$='0'&cu_in$
		end if
	end if
	fn_clean_ul$=cu_in$
fnend

def fn_pwo_service_data(service_name$*80,reading_prior,meter_number$,serial_number$)
	if trim$(service_name$)<>"" and reading_prior>0 then 
		pr #255: "{\b <-------------"&trim$(service_name$)&"--------------->}"
		pr #255: ""
		pr #255,using 'form pos 1,c 25,g 20': "{\b Previous Reading:}",str$(reading_prior)
		pr #255: ""
		if trim$(meter_number$)<>'' then 
			pr #255,using 'form pos 1,c 25,g 20': "{\b Meter Number:}","{\ul "&meter_number$&"}"
			pr #255: ""
		end if 
		if trim$(serial_number$)<>'' then 
			pr #255,using 'form pos 1,c 25,g 20': "{\b Serial Number:}","{\ul "&serial_number$&"}"
			pr #255: ""
		end if 
		pr #255,using 'form pos 1,c 25,g 20': "{\b Current Reading:}","______________"
		pr #255: ""
	end if 
fnend 
def fn_not_blank(nbTestText$*256)
	nbReturn=1
	nbTestText$=srep$(nbTestText$,'0','')
	nbTestText$=srep$(nbTestText$,'{\ul ','')
	nbTestText$=srep$(nbTestText$,'}','')
	nbTestText$=srep$(nbTestText$,' ','')
	nbTestText$=srep$(nbTestText$,'_','')
	if nbTestText$='' then
		nbReturn=0
	end if
	fn_not_blank=nbReturn
fnend
include: ertn
