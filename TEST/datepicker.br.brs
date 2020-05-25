! S:\acsCL\unpaidinvoice - backup copy in C:\ACS\Dev-5\acsCL\UnpaidInvoice(before_overhaul09-28-2016).br.brs
autoLibrary
fnTop(program$,"Test DatePicker")
 
dim chdr$(16),cmask$(16) ! used with flex grid
dim resp$(256)*50
 
open #clearing=89: "Name=[Q]\CLmstr\clearing.H"&wsid$&",replace,RecL=114",internal,outIn,relative  ! kj wrong recl
fnTos
respc=0 : mat resp$=('')
fnLbl(1,1,trim$(env$('cnam')(1:30))&"-"&'',65,2)
					fnflexinit1('unpaidinv',5,27,15,55,mat chdr$,mat cmask$,1)
					restore #clearing:
					if nextrec>0 and displayattop$="True" then goto L4890 else goto L5030
				L4890: for j=nextrec to lrec(clearing) ! read starting with next record
						read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L4940
						flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
						flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
						flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
						flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
						flxitm$(16)=str$(pdte)
						flxitm$(1)=str$(rec(clearing))
						if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
				fnflexadd1(mat flxitm$)
				L4940: next j
				if nextrec=1 then goto L5020 ! thinks it rereads the 1st record twice
				for j=1 to max(nextrec-1,1) ! read records previously coded or skipped
					read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
					flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
					flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
					flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
					flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
					flxitm$(1)=str$(rec(clearing))
					if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
				fnflexadd1(mat flxitm$)
				next j
				L5020: goto L5070
				L5030: !
				read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
				flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
				flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
				flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
				flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
				flxitm$(16)=str$(pdte)
				flxitm$(1)=str$(rec(clearing)) ! assign flxitm$(1) with new record #
				if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
				fnflexadd1(mat flxitm$) : goto L5030
L5070: !
fnFra(2,1,13,23,"Approval Options"," ")
fnButton(1,2,"&Approve All",62,"Will select to pay all unpaid invoices",1,18,1)
fnButton(3,2,"&Approve by Range",63,"Enter a range of reference numbers to approve.  The reference # is the number to the left assigned by the computer.",1,18,1)
fnLbl(4,4,"From:",5,1,0,1)
fnTxt(4,11,5,0,1,"30",0,"Select the first reference # to be approved",1)
resp$(respc+=1)=""
fnLbl(5,4,"To:",5,1,0,1)
fnTxt(5,11,5,0,1,"30",0,"Select the last reference # to be approved",1)
resp$(respc+=1)=""
fnButton(7,2,"&Approve by Due Date",64,"Approve all invoices due by a certain date.",1,18,1)
fnLbl(8,2,"Date:",5,1,0,1)
fnTxt(8,8,8,0,1,"1",0,"All invoices with a due by date equal to or less than this date will be approved",1)
resp$(respc+=1)=""
! fnButton(10,2,"Approve &Highlighted",65,"Approves one invoice at a time.  Highlight the selected invoice. To remove the approval, use this option and highlight the same invoice second time.",1,18,1,0,1)
fnButton(10,2,"Approve By Payee",66,"Approves all invoices with this payee number in invoice record.",1,18,1)
fnLbl(11,2,"Payee #:",8,1,0,1)
fnTxt(11,11,8,0,1,"",0,"Enter payee # to approve all invoices on that payee",1)
resp$(respc+=1)=""
if displayunpaid=1 or displayunpaid=0 then wording$="Total Selected:" else wording$= "Total Unapproved:"
fnLbl(2,28,wording$,18,1)
fnTxt(2,49,12,0,1,"10",0," ")
resp$(respc+=1)=str$(total)
fnChk(3,47,"Display at Top:",1)
resp$(respc+=1)=displayattop$
fnCmdKey("&Approve Highlighted",1,1,0,"Approves or cancels the invoice that is highlighted.")
fnCmdKey("&Display All",9,0,0,"Displays all remaining records in the unpaid file.")
fnCmdKey("&Display Selected",3,0,0,"Displays all invoices selected for payment")
fnCmdKey("&Display UnSelected",2,0,0,"Displays all remaining uncleared invoices")
fnCmdKey("C&omplete",5,0,1,"Return to main unpaid invoice menu")
fnAcs2(mat resp$,ck)
