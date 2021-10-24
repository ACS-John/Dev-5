! formerly S:\acsGL\financialstatement
fn_setup
fnTop(program$)
 
ScrMenu1: ! r:
	fnTos
	mylen=20: mypos=mylen+3 : right=1
	! fnFra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
	frame=0 ! 1
	fnOpt(1,2,id$(1),0,frame)  : resp$(1)="True"
	fnOpt(2,2,id$(2),0,frame)  : resp$(2)="False"
	fnOpt(3,2,id$(3),0,frame)  : resp$(3)="False"
	                                                fnOpt(1,35,id$(4),0,frame) : resp$(4)="False"
	                                                fnOpt(2,35,id$(5),0,frame) : resp$(5)="False"
	                                                fnOpt(3,35,id$(6),0,frame) : resp$(6)="False"
	
	
	fnCmdKey("&Next",1,1,0,"Access the chosen financial statement design.")
	fnCmdKey("&Build D Records",2,0,0,"Automaticly build all type D records (without having to rekey all descriptions).")
	fnCmdKey("&Proof List",3,0,0,"Allows you to pr a proof list of the financial statement layout.")
	fnCmdKey("&Close",5,0,1,"Return to main menu.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=2 then chain "S:\acsGL\Bld_D_Records"
	if resp$(1)="True" then selection=1
	if resp$(2)="True" then selection=2
	if resp$(3)="True" then selection=3
	if resp$(4)="True" then selection=4
	if resp$(5)="True" then selection=5
	if resp$(6)="True" then selection=6
	f1=selection
	close #1: ioerr ignore
  open #hFSDesign=1: "Name=[Q]\GLmstr\"&fil$(f1)&",KFName=[Q]\GLmstr\"&idx$(f1)&",Shr",internal,outIn,keyed ioerr ScrMenu1
	F_FSDesign: form pos 1,c 5,c 50,c 1,2*n 2,15*n 1,n 3,n 5
	if ckey=3 then let fn_print_proof
goto ScrGrid ! /r
ScrGrid: ! r:
	fnTos
	fnlbl(1,1,id$(selection))
	fnflexinit1('fin_stmtgl',lc=3,1,10,70,mat chdr$,mat cmask$,1)
	restore #hFSDesign:
	do
		READ_FIN_STMT: ! r: read hFSDesign file and populate grid
		dim rno$*5
		dim d$*50
		dim te$*1
		read #hFSDesign,using F_FSDesign: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec READ_FIN_STMT,eof EO_FIN_STMT_GRID,conv DeleteCorruptRecord
		dim item$(24)*50
		item$(1)=str$(rec(hFSDesign))
		item$(2)=rno$
		item$(3)=d$
		item$(4)=te$
		for x=1 to udim(mat option2$)
		if item$(4)=option2$(x)(1:1) then
			item$(4)=option2$(x)(1:pos(option2$(x),'(')-2)
		end if
		nex x
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
	loop
	EO_FIN_STMT_GRID: ! /r
	fnCmdKey("&Edit"  ,2,1,0,"Highlight any record and press Enter or click Edit to change any existing financial statement reference number.")
	fnCmdKey("&Add"   ,1,0,0,"Allows you to add new financial statement reference numbers.")
	fnCmdKey("&Delete",8,0,0,"Highlight any record and click Delete to remove the financial statement reference number.")
! fnCmdKey("&Print",3,0,0,"Takes you directly to the pr financial statement reference number option")
	fnCmdKey("E&xit"  ,5,0,1,"Exits to main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto IndexThenMenu1
	add=edit=0
	editRec=val(resp$(1)) ! (1:pos(resp$(1)&' ',' ')-1))
	if ckey=1 then
		add=1
		sp=ls=ds=ul=rs=bc=ap=ic=fc=rnp=0
		rno$=d$=te$=""
		mat ac=(0)
		goto AddEdit ! ! add
	else if ckey=2 then
		edit=1
		read #hFSDesign,using F_FSDesign,rec=editRec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec ScrGrid
		holdrno$=rno$
		goto AddEdit
	else if ckey=8 then
		read #hFSDesign,using F_FSDesign,rec=editRec,release: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec ScrGrid
		gosub DeleteRef
		goto ScrGrid
	end if
goto ScrGrid ! /r
AddEdit: ! r:
	fnTos
	mylen=25: mypos=mylen+3 : right=1
	fnLbl(1,1,"F/S Number:",mylen,right)
	fnTxt(1,mypos,5,0,right,"30",0,"",0 )
	resp$(1)=rno$
	fnLbl(2,1,"Description::",mylen,right)
	fnTxt(2,mypos,50,0,left,"",0,"",0 )
	resp$(2)=d$
	choice$=option2$(1)
	for x=1 to udim(mat option2$)
		if te$=option2$(x)(1:1) then choice$=option2$(x)
	nex x
	fnLbl(3,1,"Type of Entry:",mylen,right)
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
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ScrGrid
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
	gosub AddEditValidation
	if edit=1 then
		if trim$(rno$)="" or trim$(rno$)="0" then goto AddEdit
		if holdrno$<>rno$ and trim$(holdrno$)<>"" then
			mat ml$(3)
			ml$(1)="You are changing reference # "&holdrno$&" to "
			ml$(2)="reference # "&rno$&".  Click OK to continue, else"
			ml$(3)="Cancel to prevent changing the #."
			fnmsgbox(mat ml$,resp$,'',49)
			if resp$<>"OK" then goto AddEdit
		end if
		rewrite #hFSDesign,using F_FSDesign,rec=editRec: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
		
 
	else if add=1 then
		write #hFSDesign,using F_FSDesign: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
	end if
goto ScrGrid ! /r
DeleteRef: ! r: delete a reference #
	mat ml$(3)
	ml$(1)="You are attempting to delete reference # "&rno$&"."
	ml$(2)="Click OK to continue, "
	ml$(3)="else Cancel to prevent deleting the reference #."
	fnmsgbox(mat ml$,resp$,'',49)
	if uprc$(resp$)="OK" then goto L1530 else goto AddEdit
	L1530: !
	delete #hFSDesign,rec=editRec:
goto ScrGrid ! /r
 
AddEditValidation: ! r:
	rno$=lpad$(rtrm$(rno$),5)
	te$=uprc$(te$)
	if add=1 and (trim$(rno$)="" or trim$(rno$)="0") then
		mat ml$(2)
		ml$(1)="You must have a valid reference number in order to add a new line."
		ml$(2)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else if pos("DTRHSFPEBC",te$,1)<=0 then
		mat ml$(4)
		ml$(1)="Valid codes: D=Detail; T=Total; R=Report Heading"
		ml$(2)="H=Heading; S=Secondary Heading; F=Footnote; P=Profit or Loss"
		ml$(3)="E=Something; B=Bank Account; C=Something"
		ml$(4)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else if ds<0 or ds>1 then
		mat ml$(3)
		ml$(1)="The Dollar Sign codes must be 0 or 1.  0 is no dollar"
		ml$(2)="sign.  1 will pr a dollar sign beside the entry."
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else if ul<0 or ul>2 then
		mat ml$(0)
		fnAddOneC(mat ml$,'The Underline Codes must be 0,1 or 2.')
		fnAddOneC(mat ml$,'  0 is no underline.'           )
		fnAddOneC(mat ml$,'  1 will print one underline.'  )
		fnAddOneC(mat ml$,'  2 will print two underlines.' )
		fnAddOneC(mat ml$,'Click OK to fix.'               )
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else if rs<0 or rs>1 then ! reverse sign
		mat ml$(3)
		ml$(1)="The Reverse Sign code is simply 1 to reverse the sign."
		ml$(2)="0 will default to no action. All other codes are invalid."
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit !
	else if bc<0 or bc>3 then
		mat ml$(3)
		ml$(1)="The Balance Sheet Column is only applicable in a balance"
		ml$(2)="sheet design.  The codes are 0,1,2,3. 0 will default to 1."
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	end if
	if te$="E" and (ap<1 or ap>9) then
		mat ml$(3)
		ml$(1)="If the transaction type is an 'E' then you must"
		ml$(2)="enter a valid accumulator to pr of 1 thru 9. "
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else if ap<0 or ap>9 then
		mat ml$(3)
		ml$(1)="The Accumulator to pr must be no less than 0 or "
		ml$(2)="or no greater than 9. All other codes are invalid, "
		ml$(3)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',49)
		goto AddEdit
	else
		for j=1 to 9
			if ac(j)<>0 and ac(j)<>1 and ac(j)<>9 then
				mat ml$(3)
				ml$(1)="Accumulator to clear must be a 0,1, or 9."
				ml$(2)="All other codes are invalid, "
				ml$(3)="Click OK to fix."
				fnmsgbox(mat ml$,resp$,'',49)
				goto AddEdit
			end if
		next j
	end if
return ! /r
def fn_print_proof
	restore #hFSDesign,key>="     ": eof ScrGrid nokey PRINT_PROOF_XIT
	fnopenprn
	fn_print_proof_hdr
	do
		read #hFSDesign,using F_FSDesign: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof PRINT_PROOF_FINIS
		pr #255,using L2030: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,ac(1),ac(2),ac(3),ac(4),ac(5),ac(6),ac(7),ac(8),ac(9),rnp,fc pageoflow PgOf
	L2030: form pos 1,c 5,x 1,c 50,c 1,x 2,pic(zzzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzzzz),x 1,pic(zzzz),x 1,pic(zzz),x 1,pic(zzzzz),x 1,pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzz),pic(zzzz),x 1,pic(zzzz),skip 1
	loop
	PRINT_PROOF_FINIS: !
	fncloseprn
	PRINT_PROOF_XIT: !
fnend
PgOf: ! r:
	pr #255: newpage
	fn_print_proof_hdr
continue ! /r
def fn_print_proof_hdr
	pr #255,using 'form skip 2,pos 1,c 8,pos hp1,c 40': date$('mm/dd/yy'),env$('cnam')
	pr #255,using L2130: time$,env$('program_caption')&' Proof List -',id$(f1)(5:32),date$("Month DD, CCYY")
	L2130: form pos 1,c 8,pos 38,c 39,c 32,skip 1,pos hp2,c 20,skip 2
	pr #255,using L2150: "Type Of","Start","Lines","$","Under","Rev","Bs","# To","*** Clear Accumulator ***","Base","CC"
	L2150: form pos 51,c 7,x 2,c 5,x 1,c 5,x 4,c 1,x 1,c 5,x 2,c 3,x 2,c 2,x 2,c 4,x 2,c 25,x 1,c 4,x 2,c 2,skip 1
	pr #255,using L2170: "Ref #","Description","Entry","Print","Skip","Sign","Line","Sign","Col","Print","1","2","3","4","5","6","7","8","9","Item","Code"
	L2170: form pos 1,c 5,x 19,c 11,x 16,c 5,x 3,c 5,x 2,c 4,x 1,c 4,x 2,c 4,x 1,c 4,x 1,c 3,x 1,c 5,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 2,c 1,x 1,c 4,x 1,c 4,skip 2
fnend
Xit: fnXit
! r: (UNUSED) Initial build all financial statements
	close #1: ioerr ignore
	open #hFSDesign=1: "Name=[Q]\GLmstr\"&fil$(f1)&",NoShr",internal,output
	close #hFSDesign,free:
	open #hFSDesign: "Name=[Q]\GLmstr\"&fil$(f1)&",SIZE=0,RecL=83",internal,output
goto IndexThenMenu1 ! /r
IndexThenMenu1: ! r: index things
	for j=1 to 6
		close #hFSDesign: ioerr ignore
		execute "Index [Q]\GLmstr\"&fil$(j)&' '&"[Q]\GLmstr\"&idx$(j)&" 1 5 Replace DupKeys" ioerr ignore
	next j
goto ScrMenu1 ! /r
DeleteCorruptRecord: ! r:
	reread #hFSDesign,using F_FSDesign: rno$ noRec READ_FIN_STMT,eof EO_FIN_STMT_GRID,ioerr ignore
	delete #hFSDesign,key=rno$:
goto READ_FIN_STMT ! /r
include: ertn
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
 
		dim ac(9)
		dim ml$(4)*80
		dim resp$(25)*256
 
		dim choice$*55
 
		hp1=66-int(len(rtrm$(env$('cnam')))/2)
		hp2=66-int(len(rtrm$(date$("Month DD, CCYY")))/2)
	
		dim id$(6)*40                           , fil$(6)*18                , idx$(6)*18
		id$(1)=" 1. Balance Sheet"              : fil$(1)="ACGLFNSB.h[cno]" : idx$(1)="agfsidx4.h[cno]"
		id$(2)=" 2. Income Statement"           : fil$(2)="ACGLFNSI.h[cno]" : idx$(2)="agfsidx3.h[cno]"
		id$(3)=" 3. Fund Statement / Cash Flow" : fil$(3)="ACGLFNSF.h[cno]" : idx$(3)="agfsidx5.h[cno]"
		id$(4)=" 4. Secondary Balance Sheet"    : fil$(4)="ACGLFNSC.h[cno]" : idx$(4)="agfsidx1.h[cno]"
		id$(5)=" 5. Secondary Income Statement" : fil$(5)="ACGLFNSJ.h[cno]" : idx$(5)="agfsidx2.h[cno]"
		id$(6)=" 6. Secondary Fund / Cash Flow" : fil$(6)="ACGLFNSG.h[cno]" : idx$(6)="agfsidx6.h[cno]"
		! r: Column headers
		ic=0 ! temporary Item Counter
		number$="30"
		! Field Labels               Field Masks
		mat chdr$(24)              : mat cmask$(24)
		chdr$(ic+=1)="Ref #"       : cmask$(ic)=number$
		chdr$(ic+=1)="F/S #"       : cmask$(ic)=""
		chdr$(ic+=1)="Description" : cmask$(ic)=""
		chdr$(ic+=1)="Type"        : cmask$(ic)=""
		chdr$(ic+=1)="Pos"         : cmask$(ic)=number$
		chdr$(ic+=1)="LineSkip"    : cmask$(ic)=number$
		chdr$(ic+=1)="$"           : cmask$(ic)=number$
		chdr$(ic+=1)="UndrL"       : cmask$(ic)=number$
		chdr$(ic+=1)="RevSign"     : cmask$(ic)=number$
		chdr$(ic+=1)="BS Col"      : cmask$(ic)=number$
		chdr$(ic+=1)="PrAccum"     : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum1"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum2"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum3"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum4"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum5"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum6"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum7"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum8"   : cmask$(ic)=number$
		chdr$(ic+=1)="ClrAccum9"   : cmask$(ic)=number$
		chdr$(ic+=1)="Base%"       : cmask$(ic)=number$
		chdr$(ic+=1)="CostCntr"    : cmask$(ic)=number$
		chdr$(ic+=1)="Abbrv Name"  : cmask$(ic)=""
	
		dim option2$(10)*55
		option2$(1) ="D = Detail (Pulls amounts from G/L accounts)"
		option2$(2) ="T = Total  (Used to pr totals or subtotals)"
		option2$(3) ="R = Report Heading (Places name of report in heading)"
		option2$(4) ="H = Header (Places headings within the F/S)"
		option2$(5) ="S = Sub Heading (Places sub heading at top of F/S)"
		option2$(6) ="F = Footnote (Used to place footnotes at bottom of F/S)"
		option2$(7) ="P = Profit or Loss (Used to place P & L amount on B/S"
		option2$(8) ="E = Equity (Used to combine P&L with equity"
		option2$(9) ="B = Bank Accounts (Beginning and Ending on Fund Stmt)"
		option2$(10)="C = Cash Flow Pause Indicator (Pauses & asks amounts)"
		! /r
	end if
fnend
