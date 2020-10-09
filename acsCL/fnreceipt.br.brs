! Replace S:\acsCL\fnReceipt
! Standard Receipt file
def library fnaddreceipt
	autoLibrary
	on error goto Ertn
	dim ml$(0)*128	

	fnIndex('[Q]\CLmstr\Recmstr.h[cno]','[Q]\CLmstr\Recidx1.h[cno]','1 8')
	fnStatusClose
	open #trmstr2:=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
	if exists("[Q]\CLmstr\RECmstr.H[cno]")=0 then gosub CREATERECEIPTFILE
	open #receipt:=fnH: "Name=[Q]\CLmstr\recmstr.h[cno],Version=1,KFName=[Q]\CLmstr\recidx1.h[cno],Shr",internal,outIn,keyed 
	open #receiptgl:=fnH: "Name=[Q]\CLmstr\ReceiptGLBreakdown.h[cno],Version=1,KFName=[Q]\CLmstr\Receiptglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed 
	open #citystzip:=fnH: "Name=[Q]\Data\CityStZip.dat,KFName=[Q]\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed 

	MENU1: ! 
	fnTos
	respc=0
	dim cmask$(11)
	dim chdr$(11)*20
	dim item$(0)*50
	mat chdr$(3) : mat cmask$(3) : mat item$(3) 
	chdr$(1)='Rec' 
	chdr$(2)='Receipt Type'
	chdr$(3)='Description'
	cmask$(1)=cmask$(2)='' 
	cmask$(3)="80" 
	fnflexinit1('Receipt1',1,1,10,35,mat chdr$,mat cmask$,1,0,frame) 
	editrec=0
	restore #receipt: 
	READ_RECEIPT_1: ! 
	dim rec$*8
	dim nam$*30
	read #receipt,using 'Form Pos 1,C 8,c 30,',release: rec$,nam$ eof EO_FLEX1
	item$(1)=str$(rec(receipt)) 
	item$(2)=rec$ : item$(3)=nam$ 
	fnflexadd1(mat item$)
	goto READ_RECEIPT_1
	EO_FLEX1: ! 
	fnCmdKey("&Add",1,0,0,"Add new receipt records") 
	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit or press Alt+E to change any existing receipt record.") 
	fnCmdKey("&Delete",3,0,0,"Highlight any record and press Alt+D or click Delete to remove any existing receipt record.") 
	fnCmdKey("E&Xit",5,0,1,"Exit to menu")
	dim resp$(60)*50
	fnAcs(mat resp$,ckey)
	add=xedit=0
	if ckey=5 then 
		goto Xit 
	else if ckey=1 then 
		add=1
		goto ADD_NEW_RECEIPT
	end if
	if ckey=2 or ckey=3 then editrec=val(resp$(1))
	if editrec=0 then goto MENU1
	if ckey=2 or ckey=3 then 
		read #receipt,using 'Form Pos 1,C 8,c 30',rec=editrec: rec$,nam$
	end if
	if ckey=2 then xedit=1 : goto EDIT_RECEIPT
	if ckey=3 then gosub DELETE_RECEIPT : goto MENU1
	DELETE_RECEIPT: ! r:
		! check for Linked Unpaid Invoices 
		! if there are any - than tell them, and don't delete.
		open #paytrans:=fnH: "Name=[Q]\CLmstr\Paytrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed 
		restore #paytrans,key>=rec$&rpt$(chr$(0),12): nokey L490
		read #paytrans,using 'Form Pos 1,C 8': x$
		if x$=rec$ then 
			
			mat ml$(2) 
			ml$(1)="A Unpaid Invoice for this Receipt exists" 
			ml$(2)="You may not delete it." 
			fnmsgbox(mat ml$,resp$,'',0) 
			goto EO_DELETE
		end if
		L490: ! 
		delete #receipt,rec=editrec: 
		restore #receiptgl,key>=rec$: nokey EO_DELETE_RECEIPT
		DELETE_RECEIPTGL_LOOP: ! 
		dim receiptkey$*8
		read #receiptgl,using 'Form Pos 1,C 8': receiptkey$ eof EO_DELETE_RECEIPT
		if receiptkey$=rec$ then 
			delete #receiptgl: 
			goto DELETE_RECEIPTGL_LOOP
		end if
		EO_DELETE_RECEIPT: ! 
		! 
		open #trans:=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
		restore #trans, key>=holdrec$&rpt$(chr$(0),kln(trans)-len(holdrec$)): nokey EO_DEL_KEY_ON_TRANS
		L590: read #trans,using 'Form Pos 28,C 8': x$ eof EO_DEL_KEY_ON_TRANS
		if x$=rec$ then 
			rewrite #trans,using 'Form Pos 28,Cr 8': '' 
			goto L590
		end if
		EO_DEL_KEY_ON_TRANS: ! 
		close #trans: 
	EO_DELETE: return ! /r
	ADD_NEW_RECEIPT: ! r:
		rec$=nam$=""
	goto EDIT_RECEIPT ! /r
	EDIT_RECEIPT: ! r:
		holdrec$=rec$
		fnTos
		respc=0
		mylen=28 : mypos=mylen+2
		fnFra(1,1,12,70,"Receipt Information"," ")
		fnLbl(1,1,"Receipt Type:",mylen,1,0,1)
		fnTxt(1,mypos,8,0,1,"",0,"If a deposit ticket normally contains the same general breakdowns, you can create a receipt record for quickly displaying the breakdowns.  Assign each type of deposit a receipt type code.",1)
		resp$(respc+=1)=rec$
		fnLbl(2,1,"Description:",mylen,1,0,1)
		fnTxt(2,mypos,30,0,0,"",0,"",1)
		resp$(respc+=1)=nam$
		fnLbl(15,20,"Standard General Ledger Breakdowns",40,2,0,0)
		! General Ledger Breakdown GridPE)
		mat chdr$(5)
		chdr$(1)='Refenence'
		chdr$(2)='Receipt Type'
		chdr$(3)='GL Number'
		chdr$(4)='Percent'
		chdr$(5)='Description'
		mat cmask$(5)
		cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
		cmask$(4)='32'
		dim glitem$(5)*30
		mat glitem$(5)
		fnflexinit1('ReceiptGl',17,1,5,70,mat chdr$,mat cmask$,1,0,0)
		if trim$(rec$)="" then goto EO_FLEX3
		restore #receiptgl,search>=rec$: nokey EO_FLEX3
		READ_RECEIPT_GL: ! 
			dim receiptgl$*12
			dim gldesc$*30
			read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$ eof EO_FLEX3
			if rec$<>receiptkey$ then goto EO_FLEX3
			glitem$(1)=str$(rec(receiptgl)) : glitem$(2)=receiptkey$
			glitem$(3)=receiptgl$ : glitem$(4)=str$(percent)
			glitem$(5)=gldesc$
			fnflexadd1(mat glitem$)
		goto READ_RECEIPT_GL
		EO_FLEX3: ! 
		pas=1 ! don't redo combo boxes on gl
		fnLbl(16,1,"",1,0,0,0) ! add space before buttons
		fnButton(16,61,"Add",2,"Add a standard general ledger breakdown",0,4)
		fnButton(16,67,"Edit",7,"Edit or Delete a standard general ledger breakdown")
		fnCmdKey("Save",1,1,0,"Saves and returns to Receipt selection")
		fnCmdKey("&Cancel",5,0,1,"Return to Receipt selection")
		fnAcs(mat resp$,ckey)
		if ckey=5 then goto MENU1
		rec$=lpad$(trim$(resp$(1)(1:8)),8)
		nam$=resp$(2) ! name
		gldistrec=val(resp$(3)) ! record number of gl distribution entry
		if ckey=2 then 
			percent=gldistrec=0: receiptkey$=gldesc$=receiptgl$=""
			goto GL_BREAKDOWNS ! add gl breakdown
		else if ckey=7 then 
			read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: receiptkey$,receiptgl$,percent,gldesc$
			goto GL_BREAKDOWNS ! edit gl breakdown
		end if 
		tac=0
		READ_STANDARD_BREAKDOWNS: ! 
		restore #receiptgl,key>=rec$: nokey EO_TEST
		L1020: !
		read #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$ eof EO_TEST
		if rec$<>receiptkey$ then goto EO_TEST
		tac+=percent
		goto L1020
		EO_TEST: ! 
		if tac=100 or tac=0 then goto SAVE_RECEIPT
		MSGBOX4: ! percent breakdown doesn't add to 100 %
		mat ml$(3)
		ml$(1)="Your percentage breakdowns total "&str$(tac)&"."
		ml$(2)="The percentage breakdown must add to 100%."
		ml$(3)="Correct the percentages."
		fnmsgbox(mat ml$,resp$,'',16)
		goto EDIT_RECEIPT
		SAVE_RECEIPT: ! 
		if xedit=1 and rec$<>holdrec$ then gosub KEY_CHANGE
		if xedit=1 then 
			rewrite #receipt, using 'Form Pos 1,Cr 8,C 30': rec$,nam$
		else if add=1 then 
			write #receipt,using 'Form Pos 1,Cr 8,C 30': rec$,nam$ duprec MSGBOX3
		end if  
	goto MENU1 ! /r
	KEY_CHANGE: ! r: don't do on receipts
		goto L1500 ! don't change any other files
		! change the references to this file in the Transaction file
		open #trans:=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
		restore #trans,key>=holdrec$&rpt$(chr$(0),11): nokey EO_CHANGE_KEY_ON_TRANS
		L1230: ! 
		read #trans,using 'Form Pos 28,C 8': x$ eof EO_CHANGE_KEY_ON_TRANS
		if x$=holdrec$ then 
			rewrite #trans,using 'Form Pos 28,Cr 8',release: rec$
			goto L1230
		end if 
		EO_CHANGE_KEY_ON_TRANS: ! 
		close #trans: 

		! Change references to this file in the sub-file ReceiptGLBreakdown
		restore #receiptgl,search=holdrec$: nokey EO_CHANGE_KEY_ON_RECEIPTGL
		L1300: !
		read #receiptgl,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_RECEIPTGL
		if x$=holdrec$ then 
			rewrite #receiptgl,using 'Form Pos 1,Cr 8': rec$
			goto L1300
		end if 
	EO_CHANGE_KEY_ON_RECEIPTGL: ! 

	! Change references to this file in the linked file PayTrans
	open #paytrans:=fnH: "Name=[Q]\CLmstr\Paytrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed 
	restore #paytrans,key>=holdrec$&rpt$(chr$(0),12): nokey EO_CHANGE_KEY_ON_PAYTRANS
	L1370: !
	read #paytrans,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_PAYTRANS
	if x$=holdrec$ then 
		rewrite #paytrans,using 'Form Pos 1,Cr 8': rec$
		goto L1370
	end if 
	EO_CHANGE_KEY_ON_PAYTRANS: ! 
	close #paytrans: 

	! Change references to this file in the linked file UnPdAloc
	open #unpdaloc:=fnH: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr",internal,outIn,keyed 
	restore #unpdaloc,key>=holdrec$&rpt$(chr$(0),kln(unpdaloc)-len(holdrec$)): nokey EO_CHANGE_KEY_ON_UNPDALOC
	read #unpdaloc,using 'Form Pos 1,C 8': x$ eof EO_CHANGE_KEY_ON_UNPDALOC
	if x$=holdrec$ then 
		rewrite #unpdaloc,using 'Form Pos 1,Cr 8': rec$
		goto L1370
	end if 
	EO_CHANGE_KEY_ON_UNPDALOC: ! 
	close #unpdaloc: 

	L1500: ! 
	return ! /r
	MSGBOX3: ! r: dupkey
		mat ml$(2)
		ml$(1)="A record for receipt type "&rec$&" already exists"
		ml$(2)="You must select a different receipt type."
		fnmsgbox(mat ml$,resp$,'',16)
	goto EDIT_RECEIPT ! /r
	GL_BREAKDOWNS: ! r:
		fnTos
		respc=0 : mylen=28 : mypos=mylen+2
		fnLbl(1,25,"Breakdown for "&nam$(1:20),40)
		fnLbl(3,1,"General Ledger Number:",mylen,1)
		fnqgl(3,mypos)
		resp$(respc+=1)=fnrgl$(receiptgl$) ! think maybe here kj
		fnLbl(4,1,'Percent:',mylen,1)
		fnTxt(4,mypos,6,0,0,'32',0,"Percent of total check to be charged to this g/l account.  Enter 25% as 25.00!")
		resp$(respc+=1)=str$(percent)
		fnLbl(5,1,"Description:",mylen,1)
		fnTxt(5,mypos,30)
		resp$(respc+=1)=gldesc$
		fnCmdSet(7)
		fnAcs(mat resp$,ckey)
		if ckey=5 then goto EDIT_RECEIPT
		receiptkey$=rec$
		receiptgl$=fnagl$(resp$(1))
		percent=val(resp$(2)) ! percent
		gldesc$=resp$(3)
		if ckey=4 and gldistrec>0 then 
			delete #receiptgl,rec=gldistrec: 
		else if ckey=1 and gldistrec=0 then 
			write #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': receiptkey$,receiptgl$,percent,gldesc$
		else if ckey=1 and gldistrec>0 then 
			rewrite #receiptgl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',rec=gldistrec: receiptkey$,receiptgl$,percent,gldesc$
		end if 
	goto EDIT_RECEIPT ! /r
	Xit: ! 
	close #trmstr2: ioerr ignore
	close #receipt: ioerr ignore
	close #receiptgl: ioerr ignore
	close #citystzip: ioerr ignore
fnend 
CREATERECEIPTFILE: ! r:
	open #receipt:=fnH: "Name=[Q]\CLmstr\recmstr.h[cno],Version=1,KFName=[Q]\CLmstr\recidx1.h[cno],REPLACE,RecL=38,KPS=1,KLN=8",internal,outIn,keyed 
	close #receipt: 
	execute "Index [Q]\CLmstr\Recmstr.h[cno]"&' '&"[Q]\CLmstr\Recidx1.h[cno] 1 8 Replace DupKeys,Shr"
	execute "Index [Q]\CLmstr\Receiptglbreakdown.h[cno]"&' '&"[Q]\CLmstr\receiptglbkdidx.h[cno] 1 8 Replace DupKeys,Shr"
return  ! /r
include: ertn
