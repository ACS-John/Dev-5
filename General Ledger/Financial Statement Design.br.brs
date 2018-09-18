! formerly S:\acsGL\financialstatement
! r: setup
	library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnTos,fnFra,fnOpt,fnCmdKey,fnAcs,fndat,fnflexinit1,fnflexadd1,fnLbl,fnTxt,fnChk,fncomboa,fnmsgbox
	on error goto ERTN

	dim ac(9)
	dim id$(6)*40,fil$(6)*18,idx$(6)*18,dat$*20
	dim ml$(4)*80
	dim resp$(25)*60
	dim cap$*128,item$(24)*50,option2$(10)*55
	dim choice$*55
	fncno(cno)
	fndat(dat$)
	hp1=66-int(len(rtrm$(env$('cnam')))/2)
	hp2=66-int(len(rtrm$(dat$))/2)
	id$(1)=" 1. Balance Sheet File"              : fil$(1)="ACGLFNSB.H[cno]" : idx$(1)="FNSBINDX.H[cno]"
	id$(2)=" 2. Income Statement File"           : fil$(2)="ACGLFNSI.H[cno]" : idx$(2)="FNSIINDX.H[cno]"
	id$(3)=" 3. Fund Statement / Cash Flow File" : fil$(3)="ACGLFNSF.H[cno]" : idx$(3)="FNSFINDX.H[cno]"
	id$(4)=" 4. Secondary Balance Sheet File"    : fil$(4)="ACGLFNSC.H[cno]" : idx$(4)="FNSCINDX.H[cno]"
	id$(5)=" 5. Secondary Income Statement File" : fil$(5)="ACGLFNSJ.H[cno]" : idx$(5)="FNSJINDX.H[cno]"
	id$(6)=" 6. Secondary Fund / Cash Flow File" : fil$(6)="ACGLFNSG.H[cno]" : idx$(6)="FNSGINDX.H[cno]"

! /r
	fntop(program$)
	fn_field_labels ! get column headers

MENU1: ! 
	fnTos(sn$="FsDesign")
	mylen=20: mypos=mylen+3 : right=1
	fnFra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
	fnOpt(1,2,id$(1),0,1)
	resp$(1)="True"
	fnOpt(2,2,id$(2) ,0,1)
	resp$(2)="False"
	fnOpt(3,2,id$(3),0,1)
	resp$(3)="False"
	fnOpt(4,2,id$(4),0,1)
	resp$(4)="False"
	fnOpt(5,2,id$(5),0,1)
	resp$(5)="False"
	fnOpt(6,2,id$(6),0,1)
	resp$(6)="False"
	f1=curfld
	fnCmdKey("&Next",1,1,0,"Access the chosen financial statement design.")
	fnCmdKey("&Build D Records",2,0,0,"Allows you to build all type D records automatically without having to rekey all descriptions.")
	fnCmdKey("&Proof List",3,0,0,"Allows you to pr a proof list of the financial statement layout.")
	fnCmdKey("&Close",5,0,1,"Return to main menu.")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	if ckey=2 then chain "S:\acsGL\Bld_D_Records"
	if resp$(1)="True" then selection=1
	if resp$(2)="True" then selection=2
	if resp$(3)="True" then selection=3
	if resp$(4)="True" then selection=4
	if resp$(5)="True" then selection=5
	if resp$(6)="True" then selection=6
	f1=selection
	close #1: ioerr L520
L520: open #fin_stmt=1: "Name=[Q]\GLmstr\"&fil$(f1)&",KFName=[Q]\GLmstr\"&idx$(f1)&",Shr",internal,outIn,keyed ioerr MENU1
	if ckey=3 then let fn_print_proof
FIN_STMT_GRID: ! 
	fnTos(sn$="fin_stmt")
	fnflexinit1('fin_stmtgl',lc=1,1,10,70,mat chdr$,mat cmask$,1)
	restore #fin_stmt: 
READ_FIN_STMT: ! read fin_stmt file
	dim rno$*5
	dim d$*50
	dim te$*1
	read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec READ_FIN_STMT,eof EO_FIN_STMT_GRID,conv FIXRNP
	item$(1)=str$(rec(fin_stmt))
	item$(2)=rno$
	item$(3)=d$
	item$(4)=te$
	item$(5)=str$(sp)
	item$(6)=str$(ls)
	item$(7)=str$(ds)
	item$(8)=str$(ul)
	item$(9)=str$(rs) 
	item$(10)=str$(bc)
	item$(11)=str$(ap)    
	item$(12)=str$(ac(1))
	item$(13)=str$(ac(2))
	item$(14)=str$(ac(3)) 
	item$(15)=str$(ac(4))
	item$(16)=str$(ac(5))
	item$(17)=str$(ac(6)) 
	item$(18)=str$(ac(7)) 
	item$(19)=str$(ac(8))
	item$(20)=str$(ac(9)) 
	item$(21)=str$(rnp) 
	item$(22)=str$(fc)
	item$(23)=d$(1:10)
	fnflexadd1(mat item$)
goto READ_FIN_STMT
EO_FIN_STMT_GRID: ! 
	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing financial statement reference number.")
	fnCmdKey("&Add",1,0,0,"Allows you to add new financial statement reference numbers.")
	fnCmdKey("&Delete",8,0,0,"Highlight any record and click Delete to remove the financial statement reference number.")
! fnCmdKey("&Print",3,0,0,"Takes you directly to the pr financial statement reference number option")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto L2350
	add=edit=0
	editrec=val(resp$(1))
	if ckey=1 then 
		add=1
		sp=ls=ds=ul=rs=bc=ap=ic=fc=rnp=0
		rno$=d$=te$=""
		mat ac=(0)
	goto ADD_EDIT_FIN_STMTS ! ! add
	else if ckey=2 then 
		edit=1
		read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5",rec=editrec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec FIN_STMT_GRID
		holdrno$=rno$
	goto ADD_EDIT_FIN_STMTS ! 
	else if ckey=8 then 
		read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5",rec=editrec,release: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec FIN_STMT_GRID
		gosub DELETEIT
		goto FIN_STMT_GRID
	end if 
goto FIN_STMT_GRID
ADD_EDIT_FIN_STMTS: ! r:
	fnTos(sn$="fin_stmt1")
	mylen=25: mypos=mylen+3 : right=1
	fnLbl(1,1,"F/S Number:",mylen,right)
	fnTxt(1,mypos,5,0,right,"30",0,"",0 )
	resp$(1)=rno$
	fnLbl(2,1,"Description::",mylen,right)
	fnTxt(2,mypos,50,0,left,"",0,"",0 )
	resp$(2)=d$
	if trim$(te$)='' or uprc$(te$)="D" then choice$=option2$(1) : goto L875
	if uprc$(te$)="T" then choice$=option2$(2) : goto L875
	if uprc$(te$)="R" then choice$=option2$(3) : goto L875
	if uprc$(te$)="H" then choice$=option2$(4) : goto L875
	if uprc$(te$)="S" then choice$=option2$(5) : goto L875
	if uprc$(te$)="E" then choice$=option2$(6) : goto L875
	if uprc$(te$)="P" then choice$=option2$(7) : goto L875
	if uprc$(te$)="E" then choice$=option2$(8) : goto L875
	if uprc$(te$)="B" then choice$=option2$(9) : goto L875
	if uprc$(te$)="C" then choice$=option2$(10) : goto L875
L875: fnLbl(3,1,"Type of Entry:",mylen,right)
	fncomboa("TypeOfEntry",3,mypos,mat option2$,"Each entry must have a type of transaction.",60)
	resp$(3)=choice$
	fnLbl(4,1,"Starting pr Position:",mylen,right)
	fnTxt(4,mypos,3,0,0,"30",0,"Number of spaces to indent.",0 )
	resp$(4)=str$(sp)
	fnLbl(5,1,"Lines to Skip:",mylen,right)
	fnTxt(5,mypos,2,0,0,"30",0,"Number of blank lines following this line.",0 )
	resp$(5)=str$(ls)
	fnChk(6,mypos,"Dollar Sign:",1)
	if ds=1 then resp$(6)="True" else resp$(6)="False"
	fnLbl(7,1,"Underlines:",mylen,right)
	fnTxt(7,mypos,1,0,0,"30",0,"Number of under lines following the amount.",0 )
	resp$(7)=str$(ul)
	fnChk(8,mypos,"Reverse Sign:",1)
	if rs=1 then resp$(8)="True" else resp$(8)="False"
	fnLbl(9,1,"Balance Sheet Column:",mylen,right)
	fnTxt(9,mypos,1,0,0,"30",0,"One of three columns available.",0 )
	resp$(9)=str$(bc)
	fnLbl(10,1,"Accumulator to Print:",mylen,right)
	fnTxt(10,mypos,1,0,0,"30",0,"One of nine sets of totals available for total records.",0 )
	resp$(10)=str$(ap)
	for j=1 to 9
		fnLbl(10+j,1,"Clear Accumulator # "&str$(j)&":",mylen,right)
		fnTxt(10+j,mypos,1,0,0,"30",0,"Place a one by each accumulator that should be cleared after this line is printed.",0 )
		resp$(10+j)=str$(ac(j))
	next j
	fnLbl(20,1,"Base Item for %:",mylen,right)
	fnTxt(20,mypos,5,0,0,"30",0,"Enter the reference # of the line that should be used in calculating this percent.",0 )
	resp$(20)=str$(rnp)
	fnLbl(21,1,"Cost Center Code:",mylen,right)
	fnTxt(21,mypos,3,0,0,"30",0,"Enter the fund number for ability to pr one fund at a time.",0 )
	resp$(21)=str$(fc)
	fnCmdKey("&Save",1,1,0,"Saves changes.")
	fnCmdKey("&Cancel",5,0,1,"Returns to list of fin_stmts withouit saving any changes.")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto FIN_STMT_GRID
	rno$=resp$(1)
	rno$=lpad$(rtrm$(rno$),5)
	d$=resp$(2)
	te$=resp$(3)(1:1)
	sp=val(resp$(4))
	ls=val(resp$(5))
	if resp$(6)="True" then ds=1 else ds=0 ! dollar sign
	ul=val(resp$(7))
	if resp$(8)="True" then rs=1 else rs=0 ! reverse sign
	bc=val(resp$(9))
	ap=val(resp$(10))
	for j=1 to 9
		ac(j)=val(resp$(j+10))
	next j
	rnp=val(resp$(20)) ! was ic but ic not big enough; moved down
	fc=val(resp$(21))
	gosub EDIT_CHECKS
	if edit=1 then goto REWRITE_EXISTING_FIN_STMT
	if add=1 then goto WRITE_NEW_FIN_STMT
goto FIN_STMT_GRID ! /r
! ______________________________________________________________________
REWRITE_EXISTING_FIN_STMT: ! r:
	if trim$(rno$)="" or trim$(rno$)="0" then goto ADD_EDIT_FIN_STMTS
	if holdrno$<>rno$ and trim$(holdrno$)<>"" then goto MSGBOX1 else goto L1420
	MSGBOX1: ! 
	mat ml$(3)
	ml$(1)="You are changing reference # "&holdrno$&" to "
	ml$(2)="reference # "&rno$&".  Click OK to continue, else"
	ml$(3)="Cancel to prevent changing the #."
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L1420 else goto ADD_EDIT_FIN_STMTS
	L1420: ! 
	rewrite #fin_stmt,using L1560,rec=editrec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
	L1560: form pos 1,c 5,c 50,c 1,2*n 2,15*n 1,n 3,n 5
	goto L1470
	! 
	WRITE_NEW_FIN_STMT: ! 
	write #fin_stmt,using L1560: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
	L1470: ! 
goto FIN_STMT_GRID ! /r

DELETEIT: ! r: delete a reference #
	mat ml$(3)
	ml$(1)="You are attempting to delete reference # "&rno$&"."
	ml$(2)="Click OK to continue, "
	ml$(3)="else Cancel to prevent deleting the reference #."
	fnmsgbox(mat ml$,resp$,cap$,49)
	if uprc$(resp$)="OK" then goto L1530 else goto ADD_EDIT_FIN_STMTS
	L1530: !
	delete #fin_stmt,rec=editrec: 
goto FIN_STMT_GRID ! /r

EDIT_CHECKS: ! r:
	rno$=lpad$(rtrm$(rno$),5)
	te$=uprc$(te$)
	if add=1 and ltrm$(rtrm$(rno$))="" then goto L1640
	if add=1 and ltrm$(rtrm$(rno$))="0" then goto L1640
goto L1660 ! /r
L1640: ! r:
	mat ml$(2)
	ml$(1)="You must have a valid reference number in order to add a new line."
	ml$(2)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1660: on pos ("DTRHSFPEBC",te$,1) goto L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690,L1690 none L1670
L1670: ! r:
	mat ml$(4)
	ml$(1)="Valid codes: D=Detail; T=Total; R=Report Heading"
	ml$(2)="H=Heading; S=Secondary Heading; F=Footnote; P=Profit or Loss"
	ml$(3)="E=Something; B=Bank Account; C=Something"
	ml$(4)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1690: ! r:
	if ds>=0 and ds<=1 then goto L1720
	mat ml$(3)
	ml$(1)="The Dollar Sign codes must be 0 or 1.  0 is no dollar"
	ml$(2)="sign.  1 will pr a dollar sign beside the entry."
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1720: ! r:
	if ul>=0 and ul<=2 then goto L1750
	mat ml$(3)
	ml$(1)="The Underline Codes must be 0,1 or 2.  0 is no underline."
	ml$(2)="1 will pr 1 underline. 2 will pr 2 underlines."
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1750: ! r:
	if rs>=0 and rs<=1 then goto L1780
	mat ml$(3)
	ml$(1)="The Reverse Sign code is simply 1 to reverse the sign."
	ml$(2)="0 will default to no action. All other codes are invalid."
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1780: ! r:

	if bc>=0 and bc<=3 then goto L1810
	mat ml$(3)
	ml$(1)="The Balance Sheet Column is only applicable in a balance"
	ml$(2)="sheet design.  The codes are 0,1,2,3. 0 will default to 1."
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1810: ! r:
	if te$="E" and (ap<1 or ap>9) then goto L36100 else goto L1840
	L36100: !
	mat ml$(3)
	ml$(1)="If the transaction type is an 'E' then you must"
	ml$(2)="enter a valid accumulator to pr of 1 thru 9. "
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1840: ! r:
	if ap>=0 and ap<=9 then goto L1870
	mat ml$(3)
	ml$(1)="The Accumulator to pr must be no less than 0 or "
	ml$(2)="or no greater than 9. All other codes are invalid, "
	ml$(3)="Click OK to fix."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto ADD_EDIT_FIN_STMTS ! /r
L1870: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=1 or ac(j)=9 then goto L1910
		mat ml$(3)
		ml$(1)="Accumulator to clear must be a 0,1, or 9."
		ml$(2)="All other codes are invalid, "
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,cap$,49)
		goto ADD_EDIT_FIN_STMTS
		L1910: !
	next j
return ! /r
def fn_print_proof
	restore #fin_stmt,key>="     ": eof FIN_STMT_GRID nokey PRINT_PROOF_XIT
	fnopenprn
	fn_print_proof_hdr
	do 
		read #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof PRINT_PROOF_FINIS
		pr #255,using L2030: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,ac(1),ac(2),ac(3),ac(4),ac(5),ac(6),ac(7),ac(8),ac(9),rnp,fc pageoflow NEWPGE
	L2030: form pos 1,c 5,x 1,c 50,c 1,x 2,pic(zzzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzz),x 1,pic(zzzzz),x 1,pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzzz),x 1,pic(zzzz),skip 1
	loop 
	PRINT_PROOF_FINIS: ! 
	fncloseprn
	PRINT_PROOF_XIT: ! 
fnend 
NEWPGE: ! r:
	pr #255: newpage
	fn_print_proof_hdr
	continue 
return ! /r
def fn_print_proof_hdr
	pr #255,using L2110: date$('mm/dd/yy'),env$('cnam')
	L2110: form skip 2,pos 1,c 8,pos hp1,c 40,skip 1
	pr #255,using L2130: time$,"Financial Statement Layout Proof List -",id$(f1)(5:32),dat$
	L2130: form pos 1,c 8,pos 38,c 39,c 32,skip 1,pos hp2,c 20,skip 2
	pr #255,using L2150: "Type Of","Start","Lines","$","Under","Rev","Bs","# To","*** Clear Accumulator ***","Base","CC"
	L2150: form pos 51,c 7,x 2,c 5,x 1,c 5,x 4,c 1,x 1,c 5,x 2,c 3,x 2,c 2,x 2,c 4,x 2,c 25,x 1,c 4,x 2,c 2,skip 1
	pr #255,using L2170: "Ref #","Description","Entry","Print","Skip","Sign","Line","Sign","Col","Print","1","2","3","4","5","6","7","8","9","Item","Code"
	L2170: form pos 1,c 5,x 19,c 11,x 16,c 5,x 3,c 5,x 2,c 4,x 1,c 4,x 2,c 4,x 1,c 4,x 1,c 3,x 1,c 5,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 1,c 4,x 1,c 4,skip 2
fnend 
XIT: fnxit
! r: (UNUSED) Initial build all financial statements
	close #1: ioerr ignore
	open #fin_stmt=1: "Name=[Q]\GLmstr\"&fil$(f1)&",NoShr",internal,output 
	close #fin_stmt,free: 
	open #fin_stmt: "Name=[Q]\GLmstr\"&fil$(f1)&",SIZE=0,RecL=83",internal,output 
goto L2350 ! /r
L2350: ! r: index things
	for j=1 to 6
		close #fin_stmt: ioerr ignore
		execute "Index [Q]\GLmstr\"&fil$(j)&' '&"[Q]\GLmstr\"&idx$(j)&" 1 5 Replace DupKeys" ioerr ignore
	next j
goto MENU1 ! /r
def fn_field_labels ! ** Field Labels    **
	ic=0 ! temporary Item Counter
	mat chdr$(24) : mat cmask$(24) : mat flxitm$(24)
	chdr$(ic+=1)="Ref #"
	chdr$(ic+=1)="F/S #"
	chdr$(ic+=1)="Description"
	chdr$(ic+=1)="Type"
	chdr$(ic+=1)="SP"
	chdr$(ic+=1)="LS"
	chdr$(ic+=1)="$s"
	chdr$(ic+=1)="UL"
	chdr$(ic+=1)="RS"
	chdr$(ic+=1)="B/S"
	chdr$(ic+=1)="PA"
	chdr$(ic+=1)="C1"
	chdr$(ic+=1)="C2"
	chdr$(ic+=1)="C3"
	chdr$(ic+=1)="C4"
	chdr$(ic+=1)="C5"
	chdr$(ic+=1)="C6"
	chdr$(ic+=1)="C7"
	chdr$(ic+=1)="C8"
	chdr$(ic+=1)="C9"
	chdr$(ic+=1)="Base%"
	chdr$(ic+=1)="CC"
	chdr$(ic+=1)="Abbr Name"
	! ** Field Masks **
	ic=0
	number$="30"
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=""
	cmask$(ic+=1)=""
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=number$
	cmask$(ic+=1)=""
	option2$(1)="D = Detail (Pulls amounts from G/L accounts)"
	option2$(2)="T = Total  (Used to pr totals or subtotals)"
	option2$(3)="R = Report Heading (Places name of report in heading)"
	option2$(4)="H = Header (Places headings within the F/S)"
	option2$(5)="S = Sub Heading (Places sub heading at top of F/S)"
	option2$(6)="F = Footnote (Used to place footnotes at bottom of F/S)"
	option2$(7)="P = Profit or Loss (Used to place P & L amount on B/S"
	option2$(8)="E = Equity (Used to combine P&L with equity"
	option2$(9)="B = Bank Accounts (Beginning and Ending on Fund Stmt)"
	option2$(10)="C = Cash Flow Pause Indicator (Pauses & asks amounts)"
fnend 
FIXRNP: ! r:
	reread #fin_stmt,using "Form POS 1,C 5,C 50,C 1,2*N 2,15*N 1,N 3,N 5": rno$ noRec READ_FIN_STMT,eof EO_FIN_STMT_GRID,ioerr L9020
	L9020: !
	delete #fin_stmt,key=rno$: 
goto READ_FIN_STMT ! /r
include: ertn
