! formerly S:\acsCL\Payee
	fn_setup
	fnTop(program$,"Payee")
	fn_addpayee
	goto Xit
Xit: fnXit
include: Ertn
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
	end if
fnend
def library fnaddpayee
	if ~setup then fn_setup
	fnaddpayee=fn_addpayee
fnend
def fn_addpayee
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11
	dim tr$(5)*35,item6$(11)*35
	dim item$(11)*50,cmask$(11),chdr$(11)*20
	dim contact$*30,email$*50,fax$*12,myact$*20
	dim cap$*128,key$*19
	dim ml$(3)*70,citystzip$*30,glitem$(5)*30,payeekey$*8,payeegl$*12
	dim gldesc$*30,resp$(60)*50
 
	left=0: right=1
	open #trmstr2:=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed
	open #paymstr:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #paymstr2:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #payeegl:=fngethandle: "Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Shr",internal,outIn,keyed
	open #citystzip:=fngethandle: "Name=[Q]\Data\CityStZip.dat,KFName=[Q]\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed
 
	MENU1: !
	fnTos(sn$="payee-1")
	respc=0
	fnLbl(1,1,'Payee')
	mat chdr$(12) : mat cmask$(12) : mat item$(12)
	chdr$(1)='Rec'
	chdr$(2)='Payee Number'
	chdr$(3)='Payee Name'
	chdr$(4)='Address'
	chdr$(5)='Address'
	chdr$(6)='City, ST Zip'
	chdr$(7)='Type'
	chdr$(8)='ID Number'
	chdr$(9)='Phone Number'
	chdr$(10)='Contact Name'
	chdr$(11)='E-mail'
	chdr$(12)='Fax'
	cmask$(1)=cmask$(2)=''
	cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)='80'
	fnflexinit1('Payee',2,1,20,100,mat chdr$,mat cmask$,1,0,frame)
	editrec=0
	restore #paymstr:
	READ_PAYMSTR_1: !
	read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ eof EO_FLEX1
	item$(1)=str$(rec(paymstr))
	item$(2)=vn$
	item$(3)=nam$
	item$(4)=ad1$
	item$(5)=ad2$
	item$(6)=csz$
	item$(7)=str$(typ)
	item$(8)=ss$
	item$(9)=ph$
	item$(10)=contact$
	item$(11)=email$
	item$(12)=fax$
	fnflexadd1(mat item$)
	goto READ_PAYMSTR_1
	EO_FLEX1: !
	fnCmdKey("&Add",1,0,0,"Add new payee records")
	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing payee record.")
	fnCmdKey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing payee record.")
	fnCmdKey("E&Xit",5,0,1,"Exit to menu")
	fnAcs(sn$,0,mat resp$,ck)
	add=edit=0: holdvn$=""
	if ck=5 then
		goto PayeeXIT
	else if ck=1 then
		add=1
		goto ADD_NEW_PAYEE
	end if
	if ck=2 or ck=3 then editrec=val(resp$(1))
	if editrec=0 then goto MENU1
	if ck=2 or ck=3 then
		read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',rec=editrec: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
	end if
	if ck=2 then edit=1 : holdvn$=vn$: goto EDIT_PAYEE
	if ck=3 then gosub DELETE_PAYEE : goto MENU1
 
	DELETE_PAYEE: ! r:
	! check for Linked Unpaid Invoices
	! if there are any - than tell them, and don't delete.
	open #paytrans:=fngethandle: "Name=[Q]\CLmstr\Paytrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
	restore #paytrans,key>=vn$&rpt$(chr$(0),12): nokey L490
	read #paytrans,using 'Form Pos 1,C 8',release: x$
	if x$=vn$ then
		mat ml$(2)
		ml$(1)="A Unpaid Invoice for this payee exists"
		ml$(2)="You may not delete it."
		fnmsgbox(mat ml$,resp$,cap$,0)
		goto EO_DELETE
	end if
	L490: !
	delete #paymstr,rec=editrec:
	restore #payeegl,key>=vn$: nokey EO_DELETE_PAYEE
	DELETE_PAYEEGL_LOOP: !
	read #payeegl,using 'Form Pos 1,C 8': payeekey$ eof EO_DELETE_PAYEE
	if payeekey$=vn$ then
		delete #payeegl:
		goto DELETE_PAYEEGL_LOOP
	end if
	EO_DELETE_PAYEE: !
 
	open #trans:=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed
	restore #trans, key>=holdvn$&rpt$(chr$(0),kln(trans)-len(holdvn$)): nokey EO_DEL_KEY_ON_TRANS
	L590: !
	read #trans,using 'Form Pos 28,C 8': x$ eof EO_DEL_KEY_ON_TRANS
	if x$=vn$ then
		rewrite #trans,using 'Form Pos 28,Cr 8': ''
		goto L590
	end if
	EO_DEL_KEY_ON_TRANS: !
	close #trans:
	EO_DELETE: !
return ! /r
 
	ADD_NEW_PAYEE: ! r:
	vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=contact$=email$=fax$=myact$=""
	typ=0
	goto EDIT_PAYEE ! /r
	EDIT_PAYEE: ! r:
	fnTos(sn$="payee-2")
	respc=0
	mylen=28 : mypos=mylen+2
	fnFra(1,1,12,70,"Payee Information"," ")
	fnLbl(1,1,"Payee Number:",mylen,1,0,1)
	fnTxt(1,mypos,8,0,1,"",0,"",1)
	resp$(respc+=1)=vn$
	fnLbl(2,1,"Payee Name:",mylen,1,0,1)
	fnTxt(2,mypos,30,0,0,"",0,"",1)
	resp$(respc+=1)=nam$
	fnLbl(3,1,"Address:",mylen,1,0,1)
	fnTxt(3,mypos,30,0,0,"",0,"",1)
	resp$(respc+=1)=ad1$
	fnLbl(4,1,"Address:",mylen,1,0,1)
	fnTxt(4,mypos,30,0,0,"",0,"",1)
	resp$(respc+=1)=ad2$
	fnLbl(5,1,"City, St. Zip:",mylen,1,0,1)
	fncombof("CityStZip",5,mypos,30,"[Q]\Data\CityStZip.dat",1,30,0,0,"[Q]\Data\CityStZip.idx",0,0, " ",1,0)
	resp$(respc+=1)=csz$
	fnLbl(6,1,"Type:",mylen,1,0,1)
	fncombof("Payeetype",6,mypos,27,"[Q]\CLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Only enter a type code if the payee should get a 1099",1)
	resp$(respc+=1)=str$(typ)
	fnLbl(7,1,"Federal ID or SS No.",mylen,1,0,1)
	fnTxt(7,mypos,11,0,0,"",0,"",1)
	resp$(respc+=1)=ss$
	fnLbl(8,1,"Phone Number:",mylen,1,0,1)
	fnTxt(8,mypos,12,0,0,"",0,"",1)
	resp$(respc+=1)=ph$
	fnLbl(9,1,"Contact Name:",mylen,1,0,1)
	fnTxt(9,mypos,30,0,0,"",0,"",1)
	resp$(respc+=1)=contact$
	fnLbl(10,1,"E-mail Address:",mylen,1,0,1)
	fnTxt(10,mypos,30,50,0,"",0,"",1)
	resp$(respc+=1)=email$
	fnLbl(11,1,"Fax Number:",mylen,1,0,1)
	fnTxt(11,mypos,12,0,0,"",0,"",1)
	resp$(respc+=1)=fax$
	fnLbl(12,1,"My Account Number:",mylen,1,0,1)
	fnTxt(12,mypos,20,0,0,"",0,"",1)
	resp$(respc+=1)=myact$
	fnLbl(17,20,"Standard General Ledger Breakdowns",40,2,0,0)
	! General Ledger Breakdown Grid
	mat chdr$(5) : mat cmask$(5) : mat glitem$(5)
	chdr$(1)='Refenence'
	chdr$(2)='Payee Number'
	chdr$(3)='GL Number'
	chdr$(4)='Percent'
	chdr$(5)='Description'
	cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
	cmask$(4)='32'
	fnflexinit1('PayeeGl',17,1,5,70,mat chdr$,mat cmask$,1,0,0)
	if trim$(vn$)="" then goto EO_FLEX3
	restore #payeegl,key>=vn$: nokey EO_FLEX3
	READ_PAYEE_GL: !
	read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_FLEX3
	if vn$<>payeekey$ then goto EO_FLEX3
	glitem$(1)=str$(rec(payeegl))
	glitem$(2)=payeekey$
	glitem$(3)=payeegl$
	glitem$(4)=str$(percent)
	glitem$(5)=gldesc$
	fnflexadd1(mat glitem$)
	goto READ_PAYEE_GL
	EO_FLEX3: !
	fnLbl(21,1,"",1,0,0,0) ! add space before buttons
	fnButton(lc=16,61,"Add",2,"Add a standard general ledger breakdowns",0,4)
	fnButton(lc,67,"Edit",7,"Edit or Delete a standard general ledger breakdowns")
	fnCmdKey("Save",1,1,0,"Saves and returns to Vendor selection")
	fnCmdKey("&Transactions",4,0,0,"List all checks for this payee")
	fnCmdKey("&Cancel",5,0,1,"Return to Vendor selection")
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto MENU1
	vn$=lpad$(trim$(resp$(1)(1:8)),8)
	nam$=resp$(2) ! name
	ad1$=resp$(3) ! address
	ad2$=resp$(4) ! address
	csz$=resp$(5) ! city state zip
	if add=1 then goto L1190
	if edit=1 and holdvn$<>vn$ then goto L1190
	goto L1210
	L1190: read #paymstr,using 'Form Pos 1,C 8',key=vn$: oldvn$ nokey L1210
	if add=1 then goto L1205
	mat ml$(2)
	ml$(1)="You already have a payee number "&vn$
	ml$(2)="Click ok to Cancel."
	fnmsgbox(mat ml$,resp$,cap$,16)
	goto MENU1
	L1205: ! r:
	mat ml$(2)
	ml$(1)="You already have a payee number "&vn$
	ml$(2)="Click ok to Change the number."
	fnmsgbox(mat ml$,resp$,cap$,16)
	goto EDIT_PAYEE ! /r
	L1210: if trim$(vn$)="" then goto L1220 else goto L1230
	L1220: ! r:
	mat ml$(2)
	ml$(1)="You must have a unique payee number for ."
	ml$(2)="each vendor.  Click ok to assign a payee number"
	fnmsgbox(mat ml$,resp$,cap$,16)
	goto EDIT_PAYEE ! /r
	L1230: !
	read #citystzip,using 'Form POS 1,C 30',key=rpad$(ltrm$(csz$),30),release: citystzip$ nokey L1240
	goto L1250
	L1240: !
	write #citystzip,using 'Form POS 1,C 30': csz$
	L1250: !
	typ=val(resp$(6)(1:2)) ! type
	ss$=resp$(7) ! ss or fed id
	ph$=resp$(8) ! phone
	contact$=resp$(9) ! contact name
	email$=resp$(10) ! email address
	fax$=resp$(11) ! fax number
	myact$=resp$(12) ! my account number with this vendor
	gldistrec=val(resp$(13)) ! record number of gl distribution entry
	if ck=4 then
		gosub CHECK_HISTORY
		goto EDIT_PAYEE
	else if ck=2 then
		percent=gldistrec=0
		payeekey$=gldesc$=payeegl$=""
		goto GL_BREAKDOWNS ! add gl breakdown
	else if ck=7 then
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec,release: payeekey$,payeegl$,percent,gldesc$
		goto GL_BREAKDOWNS ! edit gl breakdown
	end if
	tac=0
	! READ_STANDARD_BREAKDOWNS: !
	restore #payeegl,key>=vn$: nokey EO_TEST
	do
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
		if vn$=payeekey$ then tac+=percent
	loop while vn$=payeekey$
	EO_TEST: !
	if tac=100 or tac=0 then goto SAVE_PAYEE
	! MSGBOX4: ! percent breakdown doesn't add to 100 %
	mat ml$(3)
	ml$(1)="Your percentage breakdowns total "&str$(tac)&"."
	ml$(2)="The percentage breakdown must add to 100%."
	ml$(3)="Correct the percentages."
	fnmsgbox(mat ml$,resp$,cap$,16)
	goto EDIT_PAYEE ! /r
	SAVE_PAYEE: ! r:
	if edit=1 and vn$<>holdvn$ then gosub KEY_CHANGE
	if edit=1 then
		rewrite #paymstr, using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
	else if add=1 then
		write #paymstr,using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ duprec MSGBOX3
	end if
	goto MENU1 ! /r
	KEY_CHANGE: ! r:
	! change the references to this file in the Transaction file
	open #trans:=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed
	restore #trans,key>=holdvn$&rpt$(chr$(0),11): nokey EO_CHANGE_KEY_ON_TRANS
	L1530: read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
	if x$=holdvn$ then
		rewrite #trans,using 'Form Pos 28,Cr 8',release: vn$
		goto L1530
	end if
	EO_CHANGE_KEY_ON_TRANS: !
	close #trans:
 
	! Change references to this file in the sub-file PayeeGLBreakdown
	restore #payeegl,key=holdvn$: nokey EO_CHANGE_KEY_ON_PAYEEGL
	L1600: read #payeegl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYEEGL
	if x$=holdvn$ then
		rewrite #payeegl,using 'Form Pos 1,Cr 8': vn$
		goto L1600
	end if
	EO_CHANGE_KEY_ON_PAYEEGL: !
 
	! Change references to this file in the linked file PayTrans
	open #paytrans:=fngethandle: "Name=[Q]\CLmstr\Paytrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
	restore #paytrans,key>=holdvn$&rpt$(chr$(0),12): nokey EO_CHANGE_KEY_ON_PAYTRANS
	L1670: !
	read #paytrans,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYTRANS
	if x$=holdvn$ then
		rewrite #paytrans,using 'Form Pos 1,Cr 8': vn$
		goto L1670
	end if
	EO_CHANGE_KEY_ON_PAYTRANS: !
	close #paytrans:
 
	! Change references to this file in the linked file UnPdAloc
	open #unpdaloc:=fngethandle: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr",internal,outIn,keyed
	restore #unpdaloc,key>=holdvn$&rpt$(chr$(0),kln(unpdaloc)-len(holdvn$)): nokey EO_CHANGE_KEY_ON_UNPDALOC
	read #unpdaloc,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_UNPDALOC
	if x$=holdvn$ then
		rewrite #unpdaloc,using 'Form Pos 1,Cr 8': vn$
		goto L1670
	end if
	EO_CHANGE_KEY_ON_UNPDALOC: !
	close #unpdaloc:
 
return ! /r
	MSGBOX3: ! r: dupkey
	mat ml$(2)
	ml$(1)="A record for payee number "&vn$&" already exists"
	ml$(2)="You must select a different payee number."
	fnmsgbox(mat ml$,resp$,cap$,16)
	goto EDIT_PAYEE ! /r
	CHECK_HISTORY: ! r:
	open #trans:=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,input,keyed
	fnTos(sn$='payee_hist')
	lc=0 : mylen=25 : mypos=mylen+2 : width=50
	lc+=1
	fnLbl(lc+=1,30,'Check History Selection Criteria',width,center)
	fnLbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
	fnTxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All')
	resp$(1)=''
	fnLbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
	fnTxt(lc,mypos,8,0,left,'CCYYMMDD',0,'Blank for All')
	resp$(2)=''
	wbc=0
	fnLbl(lc=6,40,'Transaction Grid')
	mat chdr$(11) : mat cmask$(11) : mat item6$(11)
	chdr$(1)='Rec'
	chdr$(2)='Ck/Rf'
	chdr$(3)='Date'
	chdr$(4)='Amount'
	chdr$(5)='Payee'
	chdr$(6)='Name/Description'
	chdr$(7)='PC'
	chdr$(8)='Stmt Clr Date'
	chdr$(9)='SC'
	chdr$(10)='Bank'
	chdr$(11)='Type'
	cmask$(1)=cmask$(2)='20'
	cmask$(3)='1'
	cmask$(4)='10'
	cmask$(8)='1'
	fnflexinit1('Gayee-'&str$(wbc)&'-'&str$(wtt),7,1,10,85,mat chdr$,mat cmask$,1,0,frame)
	key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$("pic(#)",wtt)&rpt$(chr$(0),8)
	restore #trans,key>=key$: nokey EO_FLEX2
	transactionstotal=0
	READ_TRANS: !
	read #trans,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof EO_FLEX2
	if trim$(vn$)<>trim$(tr$(4)) then goto EO_FLEX2
	if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
	if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRANS
	item6$(1)=str$(rec(trans)) : item6$(2)=tr$(1)
	item6$(3)=tr$(2) : item6$(4)=str$(tr3)
	item6$(5)=tr$(4) : item6$(6)=tr$(5)
	item6$(7)=str$(pcde) : item6$(8)=str$(clr)
	item6$(9)=str$(scd) : item6$(10)=str$(bank_code)
	item6$(11)=str$(tcde)
	fnflexadd1(mat item6$)
	transactionstotal+=tr3
	goto READ_TRANS
	EO_FLEX2: !
	fnLbl(5,1,'Transactions Total:',mylen,right)
	fnTxt(5,mypos,12,0,right,"10",1,'This is the total of only the transactions shown in the Transaction Grid above. ')
	resp$(3)=str$(transactionstotal)
	fnCmdKey('&Refresh',2,1,0,"If you select a date range, you must refresh the screen to see the transactions for that date range.")
	fnCmdKey('&Close',5,0,1)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 or ck=cancel then goto EO_CHECK_HISTORY
	transactionstartingdate=val(resp$(1))
	transactionendingdate=val(resp$(2))
	if ck=2 then goto CHECK_HISTORY ! goto the top of this function
	EO_CHECK_HISTORY: !
	close #trans:
return
 
	GL_BREAKDOWNS: ! r:
	fnTos(sn$='payee_gl_dist')
	respc=0 : mylen=28 : mypos=mylen+2
	fnLbl(1,25,"Breakdown for "&nam$(1:20),40)
	fnLbl(3,1,"General Ledger Number:",mylen,right)
	fnqgl(3,mypos)
	resp$(respc+=1)=fnrgl$(payeegl$) ! think maybe here kj
	fnLbl(4,1,'Percent:',mylen,right)
	fnTxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!")
	resp$(respc+=1)=str$(percent)
	fnLbl(5,1,"Description:",mylen,right)
	fnTxt(5,mypos,30)
	resp$(respc+=1)=gldesc$
	fnCmdSet(7)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto EDIT_PAYEE
	payeekey$=vn$
	payeegl$=fnagl$(resp$(1))
	percent=val(resp$(2)) ! percent
	gldesc$=resp$(3)
	if ck=4 and gldistrec>0 then
		delete #payeegl,rec=gldistrec:
	else if ck=1 and gldistrec=0 then
		write #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$
	else if ck=1 and gldistrec>0 then
		rewrite #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
	end if
	goto EDIT_PAYEE ! /r
	! execute "Index [Q]\CLmstr\payeeglbreakdown.H[cno]"&' '&"[Q]\CLmstr\Payeeglbkdidx.H[cno] 1 8 Replace DupKeys -n"
 
	PayeeXIT: !
	close #trmstr2: ioerr ignore
	close #paymstr: ioerr ignore
	close #paymstr2: ioerr ignore
	close #payeegl: ioerr ignore
	close #citystzip: ioerr ignore
fnend
