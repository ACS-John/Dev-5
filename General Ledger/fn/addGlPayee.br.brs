def library fnAddGlPayee
	autoLibrary

	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,de$*30
	dim item6$(11)*35
	dim item$(11)*50,cmask$(11),chdr$(11)*20
	dim contact$*30,email$*50,fax$*12,myact$*20
	dim cap$*128,key$*19
	dim ml$(3)*70,citystzip$*30,glitem$(5)*30,payeekey$*8,payeegl$*12
	dim gldesc$*30,resp$(60)*50
	! r: setup files
	if ~exists("[Q]\GLmstr\PayMstr.h[cno]") then
		open #paymstr=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],RecL=276,kln=8,kps=1,Replace",internal,outIn,keyed
		close #paymstr: ioerr ignore
	end if
	if ~exists("[Q]\GLmstr\PayIdx1.h[cno]") or ~exists("[Q]\GLmstr\PayIdx2.h[cno]") then
		fnIndex('[Q]\GLmstr\PayMstr.h[cno]','[Q]\GLmstr\Payidx1.h[cno]','1 8 ')
		fnIndex('[Q]\GLmstr\PayMstr.h[cno]','[Q]\GLmstr\Payidx2.h[cno]','9 38')
		fnStatusClose
	end if
	open #paymstr=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #paymstr2=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #payeegl=fnH: "Name=[Q]\GLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\GLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
	open #citystzip=fnH: "Name=[Q]\Data\CityStZip.dat,KFName=[Q]\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed
	! /r
	MENU1: ! r:
		fnTos
		respc=0
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
		fnflexinit1('Hayee',1,1,20,100,mat chdr$,mat cmask$,1,0,frame)
		editrec=0
		restore #paymstr:
		do
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
		loop
		EO_FLEX1: !
		fnCmdKey("&Add",1,0,0,"Add new payee records")
		fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing payee record.")
		fnCmdKey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing payee record.")
		fnCmdKey("E&xit",5,0,1,"Exit to menu")
		ckey=fnAcs(mat resp$)
		add=_edit=0
		if ckey=5 then
			goto XitFn
		else if ckey=1 then
			add=1
			goto ADD_NEW_PAYEE
		else if ckey=2 or ckey=3 then
			editrec=val(resp$(1))
		end if
		if editrec=0 then goto MENU1
		if ckey=2 or ckey=3 then
			read #paymstr,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',rec=editrec: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
		end if
		if ckey=2 then
			_edit=1
			goto EDIT_PAYEE
		end if
		if ckey=3 then gosub DELETE_PAYEE : goto MENU1
	goto MENU1 ! /r
	DELETE_PAYEE: ! r: a gosub routine
		delete #paymstr,rec=editrec:
		restore #payeegl,key>=vn$: nokey EO_DELETE_PAYEE
		do
			read #payeegl,using 'Form Pos 1,C 8': payeekey$ eof EO_DELETE_PAYEE
			if payeekey$=vn$ then
				delete #payeegl:
			end if
		loop
		EO_DELETE_PAYEE: !

		open #trans=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed
		if trim$(holdvn$)="" then goto EO_DEL_KEY_ON_TRANS
		restore #trans, key>=holdvn$: nokey EO_DEL_KEY_ON_TRANS
		L570: !
		read #trans,using 'Form Pos 1,C 8': trx$ eof EO_DEL_KEY_ON_TRANS
		if trx$=vn$ then
			rewrite #trans,using 'Form Pos 1,Cr 8': ''
			goto L570
		end if
		EO_DEL_KEY_ON_TRANS: !
		close #trans: ioerr ignore
		! EO_DELETE: !
	return ! /r
	ADD_NEW_PAYEE: ! r:
		vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=contact$=email$=fax$=myact$=""
		typ=0
	goto EDIT_PAYEE  ! /r
	EDIT_PAYEE: ! r:
		holdvn$=vn$
		fnTos
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
		fncombof("Payeetype",6,mypos,27,"[Q]\GLmstr\PayeeType.dat",1,2,3,25,"",0,0, "The payee type is a code used to detemine which box should be used on a 1099 misc form.  Only enter a type code if the payee should get a 1099",1)
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
		fnLbl(15,20,"Standard General Ledger Breakdowns",40,2,0,0)
		! r: General Ledger Breakdown Grid
		mat chdr$(5) : mat cmask$(5) : mat glitem$(5)
		chdr$(1)='Refenence'
		chdr$(2)='Payee Number'
		chdr$(3)='GL Number'
		chdr$(4)='Percent'
		chdr$(5)='Description'
		cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
		cmask$(4)='32'
		fnflexinit1('PayeeGl',16,1,5,70,mat chdr$,mat cmask$,1,0,0)
		if trim$(vn$)="" then goto EO_FLEX3
		restore #payeegl,key>=vn$: nokey EO_FLEX3
		do
			read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_FLEX3
			if vn$<>payeekey$ then goto EO_FLEX3
			glitem$(1)=str$(rec(payeegl)) : glitem$(2)=payeekey$
			glitem$(3)=payeegl$ : glitem$(4)=str$(percent)
			glitem$(5)=gldesc$
			fnflexadd1(mat glitem$)
		loop
		EO_FLEX3: ! /r
		fnLbl(21,1,"",1,0,0,0) ! add space before buttons
		!        lc=21
		fnButton(lc=15,61,"Add",2,"Add a standard general ledger breakdowns",0,4)
		! fnButton(lyne,ps,txt$*200,comkey; tt$*200,height,width,container,tabcon,default,cancel)
		fnButton(lc,67,"Edit",7,"Edit or Delete a standard general ledger breakdowns",0,0,0,0,1)
		fnCmdKey("Save",1,0,0,"Saves and returns to payee selection")
		fnCmdKey("&Transactions",4,0,0,"List all checks for this payee")
		fnCmdKey("&Cancel",5,0,1,"Return to payee selection")
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto MENU1
		vn$=lpad$(trim$(resp$(1)(1:8)),8)
		nam$=resp$(2) ! name
		ad1$=resp$(3) ! address
		ad2$=resp$(4) ! address
		csz$=resp$(5) ! city state zip
		read #citystzip,using 'Form POS 1,C 30',key=rpad$(ltrm$(csz$),30),release: citystzip$ nokey L1150 : goto L1160
		L1150: write #citystzip,using 'Form POS 1,C 30': csz$
		L1160: typ=val(resp$(6)(1:2)) ! type
		ss$=resp$(7) ! ss or fed id
		ph$=resp$(8) ! phone
		contact$=resp$(9) ! contact name
		email$=resp$(10) ! email address
		fax$=resp$(11) ! fax number
		myact$=resp$(12) ! my account number with this payee
		gldistrec=val(resp$(13)) ! record number of gl distribution entry
		if ckey=4 then
			gosub PAYEE_TRANSACTIONS
			goto EDIT_PAYEE
		else if ckey=2 then  ! add gl breakdown
			percent=gldistrec=0: payeekey$=gldesc$=payeegl$=""
			gosub GL_BREAKDOWNS
			goto EDIT_PAYEE
		else if ckey=7 then  ! edit gl breakdown
			read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
			gosub GL_BREAKDOWNS
			goto EDIT_PAYEE
		end if
		tac=fn_payeeTotalAllocationPercent
		if tac<>100 and tac<>0 then  ! r: percent breakdown doesn't add to 100 %
			mat ml$(3)
			ml$(1)="Your percentage breakdowns total "&str$(tac)&"."
			ml$(2)="The percentage breakdown must add to 100%."
			ml$(3)="Correct the percentages."
			fnmsgbox(mat ml$,resp$,cap$,16)
			goto EDIT_PAYEE ! /r
		else
			goto SAVE_PAYEE
		end if ! /r
	SAVE_PAYEE: ! r:
		if _edit=1 and vn$<>holdvn$ then gosub KEY_CHANGE
		if _edit=1 then
			rewrite #paymstr, using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
		else if add=1 then
			write #paymstr,using 'Form Pos 1,Cr 8,4*C 30,x 5,N 2,C 11,x 6,C 12,C 30,C 50,C 12,C 20': vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$ duprec MSGBOX3
		end if
	goto MENU1 ! /r
	KEY_CHANGE: ! r: a gosub routine
		! change the references to this file in the payee transaction file
		close #trans: ioerr ignore
		open #trans=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed
		restore #trans,key>=holdvn$: nokey EO_CHANGE_KEY_ON_TRANS
		do
			read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
			if x$=holdvn$ then
				rewrite #trans,using 'Form Pos 28,Cr 8',release: vn$
			end if
		loop while x$=holdvn$
		EO_CHANGE_KEY_ON_TRANS: !
		close #trans: ioerr ignore

		! Change references to this file in the sub-file PayeeGLBreakdown
		restore #payeegl,key=holdvn$: nokey EO_CHANGE_KEY_ON_PAYEEGL
		do
			read #payeegl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYEEGL
			if x$=holdvn$ then
				rewrite #payeegl,using 'Form Pos 1,Cr 8': vn$
			end if
		loop while x$=holdvn$
		EO_CHANGE_KEY_ON_PAYEEGL: !
	return ! /r
	MSGBOX3: ! r: dupkey
		mat ml$(2)
		ml$(1)="A record for payee number "&vn$&" already exists"
		ml$(2)="You must select a different payee number."
		fnmsgbox(mat ml$,resp$,cap$,16)
	goto EDIT_PAYEE ! /r
	XitFn: ! r:
	close #trmstr2: ioerr ignore
	close #paymstr: ioerr ignore
	close #paymstr2: ioerr ignore
	close #payeegl: ioerr ignore
	close #citystzip: ioerr ignore
	! /r
fnend
PAYEE_TRANSACTIONS: ! r:
	close #trans: ioerr ignore
	open #trans=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed
	fnTos
	lc=0 : mylen=25 : mypos=mylen+2 : width=50
	lc+=1
	fnLbl(lc+=1,30,'Payee Transactions',width,center)
	fnLbl(lc+=1,1,"Transaction Starting Date:",mylen,1)
	fnTxt(lc,mypos,8,0,0,'CCYYMMDD',0,'Blank for All')
	resp$(1)=str$(transactionstartingdate)
	fnLbl(lc+=1,1,"Transaction Ending Date:",mylen,1)
	fnTxt(lc,mypos,8,0,0,'CCYYMMDD',0,'Blank for All')
	resp$(2)=str$(transactionendingdate)
	wbc=0
	fnLbl(lc=6,40,'Transaction Grid')
	mat chdr$(6) : mat cmask$(6) : mat item6$(6)
	chdr$(1)='Rec'
	chdr$(2)='Payee'
	chdr$(3)='Date'
	chdr$(4)='Amount'
	chdr$(5)='Ref #'
	chdr$(6)='Name/Description'
	fnflexinit1('glPayee-'&str$(wbc)&'-'&str$(wtt),7,1,10,85,mat chdr$,mat cmask$,1,0,frame)
	key$=vn$
	transOnScreenCount=0
	restore #trans,key>=key$: nokey EO_FLEX2
	transactionstotal=0
	do
		READ_TRANS: !
		read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',release: trvn$,dt,am,rn$,de$,nta eof EO_FLEX2
		if trim$(vn$)<>trim$(trvn$) then goto EO_FLEX2
		if transactionstartingdate<>0 and transactionstartingdate>fndate_mmddyy_to_ccyymmdd(dt) then goto READ_TRANS
		if transactionendingdate<>0 and transactionendingdate<fndate_mmddyy_to_ccyymmdd(dt) then goto READ_TRANS
		item6$(1)=str$(rec(trans)) : item6$(2)=trvn$
		item6$(3)=str$(dt): item6$(4)=str$(am)
		item6$(5)=rn$ : item6$(6)=de$
		fnflexadd1(mat item6$)
		transOnScreenCount+=1
		transactionstotal+=am
	loop
	EO_FLEX2: !
	fnLbl(5,1,'Transactions Total:',mylen,1)
	fnTxt(5,mypos,12,0,1,"10",1,'This is the total of only the transactions shown in the Transaction Grid above. ')
	resp$(3)=str$(transactionstotal)
	fnCmdKey('&Refresh',2,0,0,"If you select a date range, you must refresh the screen to see the transactions for that date range.")
	fnCmdKey('&Add',3,1,0,"Allows you to add a transaction that you would like to have included in the totals on a 1099 form.")
	if transOnScreenCount>0 then
		fnCmdKey('&Edit',4,0,0,"Allows you to change or delete a transaction.")
	end if
	fnCmdKey('&Close',5,0,1)
	ckey=fnAcs(mat resp$)
	edittrans=0
	if ckey<>5 then
		if ckey=3 then
			gosub ADD_TRANSACTIONS
			goto PAYEE_TRANSACTIONS
		else if ckey=4 then
			edittrans=1
			editrec=val(resp$(3))
			gosub EDIT_TRANSACTIONS
			goto PAYEE_TRANSACTIONS
		else if ckey=2 then
			transactionstartingdate=val(resp$(1))
			transactionendingdate=val(resp$(2))
			goto PAYEE_TRANSACTIONS ! goto the top of this function
		end if
	end if
	close #trans: ioerr ignore
return ! /r
GL_BREAKDOWNS: ! r: sub routine
	fnTos
	respc=0 : mylen=28 : mypos=mylen+2
	fnLbl(1,25,"Breakdown for "&nam$(1:20),40)
	fnLbl(3,1,"General Ledger Number:",mylen,1)
	fnqgl(3,mypos)
	resp$(respc+=1)=fnrgl$(payeegl$) ! think maybe here kj
	fnLbl(4,1,'Percent:',mylen,1)
	fnTxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!")
	resp$(respc+=1)=str$(percent)
	fnLbl(5,1,"Description:",mylen,1)
	fnTxt(5,mypos,30)
	resp$(respc+=1)=gldesc$
	fnCmdSet(7)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		payeekey$=vn$
		payeegl$=fnagl$(resp$(1))
		percent=val(resp$(2)) ! percent
		gldesc$=resp$(3)
		if ckey=4 and gldistrec>0 then
			delete #payeegl,rec=gldistrec:
			goto GlBreakdownsXit
		else if ckey=1 and gldistrec=0 then
			write #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$
		else if ckey=1 and gldistrec>0 then
			rewrite #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: payeekey$,payeegl$,percent,gldesc$
		end if
	end if
	GlBreakdownsXit: !
return ! /r
def fn_payeeTotalAllocationPercent
	tac=0
	! r: READ_STANDARD_BREAKDOWNS: !
	restore #payeegl,key>=vn$: nokey EO_TEST
	do
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
		if vn$<>payeekey$ then goto EO_TEST
		tac+=percent
	loop while vn$=payeekey$
	EO_TEST: ! /r
	fn_payeeTotalAllocationPercent=tac
fnend
EDIT_TRANSACTIONS: ! r:
	read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=editrec: vn$,dt,am,rn$,de$
	gosub ADD_TRANSACTIONS
return ! /r
ADD_TRANSACTIONS: !  r: sub routnie - allows you to manually add a transaction
	if edittrans=0 then dt=am=0 : rn$=de$=""
	fnTos
	respc=0 : mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Date:",mylen,1)
	fnTxt(1,mypos,6,0,0,'1',0,'')
	resp$(1)=str$(dt)
	fnLbl(2,1,"Amount:",mylen,1)
	fnTxt(2,mypos,12,0,0,'10',0,'')
	resp$(2)=str$(am)
	fnLbl(3,1,"Ref #:",mylen,1)
	fnTxt(3,mypos,12,0,0,'',0,'')
	resp$(3)=rn$
	fnLbl(4,1,"Description:",mylen,1)
	fnTxt(4,mypos,30,0,0,'',0,'')
	resp$(4)=de$
	fnCmdKey("Save",1,1,0,"Saves any changes and returns to Payee selection")
	fnCmdKey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing transaction.")
	fnCmdKey("&Cancel",5,0,1,"Return to Payee selection screen.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto XitTransactionAdd
	else if ckey=3 then
		mat ml$(2)
		ml$(1)="You have chosen to delete this transaction."
		ml$(2)="Click OK to delete or Cancel to retain the transaction."
		fnmsgbox(mat ml$,resp$,cap$,49)
		if resp$="OK" then
			delete #trans,rec=editrec:
			goto XitTransactionAdd
		end if
	end if
	dt=val(resp$(1))
	am=val(resp$(2))
	rn$=resp$(3)
	de$=resp$(4)
	if edittrans=1 then
		rewrite #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',rec=editrec: vn$,dt,am,rn$,de$,0
		edittrans=0
	else
		write #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3': vn$,dt,am,rn$,de$,0
	end if
	XitTransactionAdd: !
return ! /r
include: ertn