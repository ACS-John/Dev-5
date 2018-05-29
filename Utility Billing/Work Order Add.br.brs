library program$: fnworkOrderAdd
library 'S:\Core\Library': fnxit,fntop,fnask_account,fngethandle
fntop(program$)
open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
do
	if fnask_account('Work Order',z$,h_customer)=5 then 
		goto XIT
	else
		fnworkOrderAdd(z$)
	end if 
loop
XIT: !
close #h_customer:
fnxit
def library fnworkOrderAdd(z$*10)
	if ~wo_setup then ! r:
		wo_setup=1
		library 'S:\Core\Library': fnWorkOrderPrint,fnAcs,fnTos,fnLbl,fnTxt
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnNoteDir$,fnCmdKey,fnWorkOrderList,fnerror
		on error goto ERTN
		dim ws$(13)*30
		dim workinfo$(15)*512
		dim i$(16)*320
		dim line$(5)*100
		ws$(1)="Date Order Taken:"
		ws$(2)="Taken by:"
		ws$(3)="Date Last Reading:"
		ws$(4)="Date to be Completed:"
		ws$(5)="Request made by:"
		ws$(6)="Phone:"
		ws$(7)="Name In:"
		ws$(8)="Turn On:"
		ws$(9)="Turn Off:"
		ws$(10)="Leave On:"
		ws$(11)="Forwarding Address:"
		ws$(12)="Forwarding City St Zip:"
		ws$(13)="Comments:"
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
	open #wo_h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
	! really only need these: (z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
	read #wo_h_customer,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
	F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	customer_name$=e$(2)
	customer_phone_number$=extra$(2)
	close #wo_h_customer:

	WO_TOS: !
	fnTos(sn$="workorder")
	respc=0
	fnLbl(1,30,"WORK ORDER",20,0,4)
	fnLbl(2,1,"Account:",10,1)
	fnTxt(2,12,10,0,1,"",1)
	workinfo$(respc_accont=respc+=1)=z$
	fnLbl(2,24,"Name:",5,1)
	fnTxt(2,31,25,30,0,"",1)
	workinfo$(respc+=1)=customer_name$
	fnLbl(4,1,"Date Order Taken:",23,1)
	fnTxt(4,25,25)
	workinfo$(respc+=1)=dat$
	fnLbl(5,1,ws$(2),23,1)
	fnTxt(5,25,25)
	workinfo$(respc+=1)=""
	fnLbl(6,1,ws$(3),23,1)
	fnTxt(6,25,25)
	workinfo$(respc+=1)=""
	fnLbl(7,1,ws$(4),23,1)
	fnTxt(7,25,25)
	workinfo$(respc+=1)=""
	fnLbl(8,1,ws$(5),23,1)
	fnTxt(8,25,25)
	workinfo$(respc+=1)=""
	fnLbl(9,1,ws$(6),23,1)
	fnTxt(9,25,14)
	workinfo$(respc+=1)=customer_phone_number$
	fnLbl(10,1,ws$(7),23,1)
	fnTxt(10,25,25)
	workinfo$(respc+=1)=""
	fnLbl(11,1,ws$(8),23,1)
	fnTxt(11,25,8,0,0,"1")
	workinfo$(respc+=1)=""
	fnLbl(12,1,ws$(9),23,1)
	fnTxt(12,25,8,0,0,"1")
	workinfo$(respc+=1)=""
	fnLbl(13,1,ws$(10),23,1)
	fnTxt(13,25,8)
	workinfo$(respc+=1)=""
	fnLbl(14,1,ws$(11),23,1)
	fnTxt(14,25,30)
	workinfo$(respc+=1)=""
	fnLbl(15,1,ws$(12),23,1)
	fnTxt(15,25,30)
	workinfo$(respc+=1)=""
	fnLbl(16,1,ws$(13),23,1)
	fnTxt(16,25,50,280)
	workinfo$(respc+=1)=""
	fnCmdKey("Print History",8,0,0,"This allows you to review the description of any work order issued in the past")
	fnCmdKey("&Print",1,1,0,"Prints a workorder on this customer for the information entered above.")
	fnCmdKey("&Cancel",5,0,1,"Returns to main customer record.")
	fnAcs(sn$,0,mat workinfo$,ckey) ! work order screen

	if ckey=5 then goto woaXIT
	z$=workinfo$(respc_accont)(1:10) ! lpad$(trim$(workinfo$(respc_accont)(1:10)),10)
	if ckey=8 then let fnWorkOrderList(z$) : goto WO_TOS
	for j=3 to 15 : i$(j-2)=workinfo$(j) : next j
	for j=1 to 12
		if trim$(workinfo$(j+2))="" then i$(j)="________________"
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

	fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
! fn_workorder_print_legacy

	 ! r: write to WorkOrder History file (z$)
		open #h_workorder:=fngethandle: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",internal,outIn,keyed
		write #h_workorder,using "form pos 1,Cr 10,n 8,c 30,5*c 100": z$,date('ccyymmdd'),customer_name$,mat line$
		close #h_workorder: 
	! /r
	! r: append to note file
		open #h_notefile:=fngethandle: "Name="&fnNoteDir$&"\"&trim$(z$)&".txt,Use",display,output
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
include: ertn

def fn_workorder_print_legacy
	library 'S:\Core\Library': fnopenprn,fncloseprn,fnsavetoasstart
! if exists("[Q]\WorkOrder")=0 then execute "mkdir [Q]\WorkOrder -n"
	fnsavetoasstart("[Q]\WorkOrder\"&trim$(z$)&date$("ccyymmdd")&".rtf")
	fnopenprn
	pr #255: "\qc {\f181 {\fs32 {\b Utility Work Order}"
	pr #255: "{\fs24 "&env$('cnam')&"}}}"
	pr #255: "\qc {\fs20 "&trim$(i$(1))&"}"
	if trim$(srvnam$(3))<>"Electric" or trim$(srvnam$(3))<>"Lawn Meter" or trim$(srvnam$(4))<>"Gas" then 
		pr #255,using "Form POS 1,C 1,SKIP 3": " " ! extra lines at top if either gas or electric not used
	end if 
	pr #255: "\ql "
	pr #255: "{\b "&ws$(3)&"}"&i$(3)&"       {\b "&ws$(2)&"}"&i$(2)
	pr #255: ""
	pr #255: "{\b "&ws$(4)&"}"&i$(4)
	pr #255: ""
	pr #255: "         {\b "&ws$(5)&"}"&i$(5)&"     {\b "&ws$(6)&"}"&i$(6)
	pr #255: ""
L10730: form pos 10,c 132,skip 2
	pr #255,using L10730: "{\b Service Address: }{\ul "&e$(1)&"}"
	pr #255,using L10730: "{\b    Meter number: }"&i$(7)
	pr #255,using L10730: "{\b        Name Out: }{\ul "&customer_name$&"}{\b           Account :}{\ul "&z$&"}"
	pr #255,using "Form pos 10,C 132": "{\b         Name In: }"&i$(7)
	pr #255: ""
	pr #255,using "Form pos 10,C 132": "{\b         "&ws$(8)&"}"&i$(8)(1:12)&"    {\b "&ws$(9)&"}"&i$(9)(1:12)&"    {\b "&ws$(10)&"}"&i$(10)(1:12)
	pr #255: ""
	pr #255,using "Form pos 10,C 132": "{\b "&ws$(11)&"}"&i$(11)&"  "&i$(12)(1:23)
	pr #255: ""
	fn_pwo_service_data(srvnam$(1),d(1),f$(1),extra$(3)) ! Water
	fn_pwo_service_data(srvnam$(3),d(5),f$(2),extra$(4)) ! Electric or Lawn Meter
	fn_pwo_service_data(srvnam$(4),d(9),f$(3),extra$(5)) ! Gas
	pr #255: "\qc ";"{\b Comments:}"
	pr #255: ""
	pr #255: "\ql "
	for j=1 to 5
		if trim$(line$(j))<>"" then 
			pr #255,using "Form pos 10,C 132": line$(j)
		else 
			pr #255,using "Form pos 10,C 132": rpt$("_",80)
		end if 
		pr #255: ""
	next j
	for j=1 to 2
		pr #255,using "Form pos 10,C 132": rpt$("_",80)
		pr #255: ""
	next j
	if trim$(srvnam$(1))<>"" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Water Deposit:}",b(8),"{\b Water Code: }"&str$(a(1))
	end if 
	if trim$(srvnam$(2))<>"" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Sewer Deposit:}",b(9),"{\b Sewer Code: }"&str$(a(2))
	end if 
	if trim$(srvnam$(3))="Electric" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Electric Deposit:}",b(10),"{\b Electric Code:}"&str$(a(3))
	end if 
	if trim$(srvnam$(3))="Lawn Meter" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Lawn Meter Deposit:}",b(10),"{\b  }"&str$(a(3))
	end if 
	if trim$(srvnam$(3))="Lawn Meter" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Lawn Meter Deposit:}",b(10),"{\b }"&str$(a(3))
	end if 
	if trim$(srvnam$(4))="Gas" then 
		pr #255,using 'form pos 1,cr 25,n 7.2,x 2,c 45': "{\b Gas Deposit:}",b(11), "{\b Gas Code:}"&str$(a(4))
	end if 
	pr #255: ""
	pr #255,using 'form pos 47,c 51': "{\b Date Order Completed: _____________________}"
	pr #255: ""
	pr #255,using 'form pos 47,c 51': "{\b By: _______________________________________}"
	fncloseprn
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