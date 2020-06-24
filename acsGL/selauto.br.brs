! Replace S:\acsGL\SelAuto
! Select Automatic Processing Programs

autoLibrary
on error goto Ertn

dim m$(200)*80,pgm$(200)*22,hlpg$(200)*40,status$(200)
dim desc$*35,srq(20)
dim nxtpgm$(20)*35,nxtdesc$(20)*35,prim(20),pn(20),cp(20)
dim ln$*128,ln2$*128,item$(1)*35,resp$(60)*35
dim prg$(100)*40,nam$(100)*35

fnTop(program$,"Select Programs")
open #1: "Name=[Q]\GLmstr\acGLPGMN.h[cno],Use,RecL=76",internal,outIn,relative ioerr L190
goto L210
L190: !
if exists("[Q]\GLmstr\acGLPGMN.h[cno]") >0 then let fnFree("[Q]\GLmstr\acGLPGMN.h[cno]")
open #1: "Name=[Q]\GLmstr\acGLPGMN.h[cno],Use,RecL=76",internal,outIn,relative ioerr MAIN
L210: !
if lrec(1)=0 then write #1,using L1690: nxtpgm$(1),nxtdesc$(1),pn(1),cp(1),prim(1),srq(1)
for j=1 to lrec(1)
	read #1,using L1690: nxtpgm$(j),nxtdesc$(j),pn(j),cp(j),prim(j),srq(j) noRec L240
L240: !
next j
	form pos 1,c 20,c 35,n 3,3*n 1
close #1: 
MAIN: !

L280: !
	fnTos
	mylen=20: mypos=mylen+3 : right=1
	item=0: resp=0
	fnLbl(1,1,"Selected Items          Primary=1 Secondary=2        Menu Options to Select From",80,0)
	for j=1 to 20
		fnTxt(j+1,1,35,0,left,"",0,"Items select for automatic processing.",0 ) 
		resp$(resp+=1)=nxtdesc$(j)
		fnTxt(j+1,39,1,0,right,"",0,"Should the primary or secondary format be used?",0 )
		if prim(j)=0 then prim(j)=1
		resp$(resp+=1)=str$(prim(j))
	next j
	mat chdr$(2) : mat cmask$(2) : mat item$(2) 
	chdr$(1)='Item #' 
	chdr$(2)='Menu option'
	cmask$(1)='30' 
	cmask$(2)='' 
	fnflexinit1('selauto',2,50,20,35,mat chdr$,mat cmask$,1,0,frame) 
	editrec=0
	close #1: ioerr ignore
	open #1: "Name=gL.mnu",display,input 
L410: linput #1: ln$ eof L540
	if ln$(1:1)<>">" then goto L410 ! skip headings
	if ln$(1:1)=">" then ln$(1:1)=""
	if ln$(1:1)=">" then ln$(1:1)="" ! delete up to two >>
	x=pos(srep$(ln$,'^','~'),'~',1) ! pos(ln$,"^",1)
	if x=0 then goto L410 ! skip headings
	desc$=ln$(1:x-1)(1:35)
	item+=1
	y=pos(srep$(ln$,'^','~'),'~',x) ! pos(ln$,"^",x)
	prg$(item)=ln$(x+1:len(ln$))
	nam$(item)=ln$(1:x-1)(1:35)
	item$(1)=str$(item) 
	item$(2)=desc$ 
	fnflexadd1(mat item$)
	goto L410
L540: !
	close #1: ioerr ignore
	if fnclient_has('G2') then open #1: "Name=S:\General Ledger\Accountants\Menu.mnu",display,input else goto L700
L560: !
	linput #1: ln$ eof L690
	if ln$(1:1)<>">" then goto L560 ! skip headings
	if ln$(1:1)=">" then ln$(1:1)=""
	if ln$(1:1)=">" then ln$(1:1)="" ! delete up to two >>
	x=pos(srep$(ln$,'^','~'),'~',1) ! pos(ln$,"^",1)
	if x=0 then goto L560 ! skip headings
	desc$=ln$(1:x-1)(1:35)
	item+=1
	y=pos(srep$(ln$,'^','~'),'~',x) ! pos(ln$,"^",x)
	prg$(item)=ln$(x+1:len(ln$))
	nam$(item)=ln$(1:x-1)(1:35)
	item$(1)=str$(item) 
	item$(2)=desc$ 
	fnflexadd1(mat item$)
goto L560
L690: fnLbl(22,1," ")
L700: fnCmdKey("&Next",1,1,0,"Selects the highlited option for automatic processing.")
	fnCmdKey("&Save",2,0,0,"Saves the selections and returns to menu.")
	fnCmdKey("&Delete All",4,0,0,"Deletes all selections.")
	fnCmdKey("&Cancel",5,0,1,"Returns to main menu without saving the selections.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	if ckey=2 then goto L860
	if ckey=4 then 
		mat nxtdesc$=("")
		mat nxtpgm$=("") 
		mat prim=(0) 
		execute "drop [Q]\GLmstr\acGLPGMN.h[cno]" 
		goto MAIN
	end if
	sel=val(resp$(41))
	for j=1 to 20
		prim(j)=val(resp$(j*2))
	next j
	for j=1 to 20
		if trim$(nxtdesc$(j))="" then nxtdesc$(j)=nam$(sel): nxtpgm$(j)=prg$(sel): goto L850
	next j
L850: if ckey=1 then goto L280
L860: goto L1640
	data "  W-2 Special Information File","S:\acsGL\W2Box16","W2Box16","E"
	data "  Budget","S:\acsGL\BGMaint2","BGMaint2","E"
	data "  Enter Period Ending Dates","S:\General Ledger\Period Ending Dates","EntDat","E"
	data "  Enter Transactions","S:\General Ledger\Enter Transactions","GLInput","E"
	data "  Post Transactions","S:\acsGL\GLJCMRGE","GLJCMRGE","E"
	data "  Reverse Posted Transactions","S:\acsGL\UNPost","","E"
	data "  Enter Standard Adjustments","S:\General Ledger\Enter Standard Adjustments","BldStdAj","E"
	data "  Post Standard Adjustments","S:\General Ledger\Post Standard Adjustments","StdAdj","E"
	data "  pr Bank Reconciliation","S:\acsGL\GLCkRec","GLCkRec","E"
	data "  pr Trial Balance Worksheet","S:\acsGL\ACGLWKSH","acglwksh","E"
	data "  pr Budget Worksheet","S:\acsGL\BudWkSh","BudWkSh","E"
	data "  pr Transactions Journal","S:\General Ledger\Transactions Journal","Transactions Journal","ES24"
	data "  pr Trial Balance","S:\General Ledger\Trial Balance","acgltb","ES25"
! Financial Stmts.
	data "  Cover Letter","S:\acsGL\ACGLCOVL","acglcovl","E"
! Balance Sheets
	data "    Standard - B/S","S:\acsGL\ACGLBAL","acglbal","ES54"
	data "    Comparison - B/S","S:\General Ledger\Comparative Balance Sheet","acglbalc","ES55"
	data "    Period Comparison - B/S","S:\General Ledger\Period Comparison Balance Sheet","acglbaly","ES56"
	data "    Fund Comparison - B/S","S:\acsGL\ACGLBALF","","ES56"
! Income Statements
	data "    Standard - I/C","S:\General Ledger\Income Statement","acglinc","ES57"
	data "    Quarterly - I/C","S:\acsGL\ACGLINCQ","acglincq","ES59"
	data "    Percent - I/C","S:\acsGL\ACGLINCP","acglincp","ES58"
	data "    Comparison - I/C","S:\acsGL\ACGLINCC","acglincc","ES59"
	data "    Budget - I/C","S:\acsGL\ACGLINCB","acglincb","ES60"
	data "    Period Comparison - I/C","S:\acsGL\ACGLINCY","acglincy","ES61"
	data "    Fund Comparison - I/C","S:\acsGL\ACGLINCF","acglincf","ES89"
	data "    Variance - I/C","S:\acsGL\ACGLINCV","acglincv","ES84"
	data "    YTD Column Only - I/C","S:\acsGL\ACGLINC1","acglinc1","ES83"
	data "    4 Column Budget - I/C","S:\acsGL\ACGLINC4","acglinc4","ES86"
	data "    GASB Budgetary Schedule - I/C","S:\acsGL\ACGLINCo","acglinco","ES86"
	data "    Year Comparison -I/C","S:\acsGL\ACGLINYY","acglinyy","ES87"
! Stmt of Change in Financial Position
	data "    Standard (FP)","S:\General Ledger\Change in Financial Position","Change in Financial Position","ES62"
	data "    Change Amount (FP)","S:\acsGL\ACGLCHGS","acglchgs","ES63"
	data "    Comparative (FP)","S:\acsGL\acGLChg","acGLchg","ES64"
	data "    Comparative Change Amount (FP)","S:\acsGL\acGLChgC","acGLchgc","ES65"
! Cash Flow Statement
	data "    Standard (CF)","S:\acsGL\ACGLCASH","ACGLCash","ES81"
	data "    Budget (CF)","S:\acsGL\acGLCASB","acGLCasB","ES82"
	data "    Fund Comparison (CF)","S:\acsGL\acGLCasF","acGLCASF","ES88"
	data "    YTD Budget - Over Under (CF)","S:\acsGL\acGLCasO","acGLcaso","ES85"
	data "  Statement of Retained Earnings","S:\acsGL\acGLRest","acGLrest","E"
	data "  pr Schedules","S:\General Ledger\Print Schedules","GLSchPrt","E"
	data "  Calculate or pr Ratios","S:\acsGL\RatioFM","RatioFM","E"
	data "  Notes to Financial Statements","S:\acsGL\acGLNote","acGLNote","E"
! Monthly
	data "  pr Payroll Registers","S:\acsGL\ACPRReg","ACPRReg","E"
	data "  Post Payroll Checks","S:\acsGL\PRMerge","PRMerge","E"
	data "  Post Entries from Holding Files","S:\General Ledger\Post Entries from Holding File","Post Entries from Holding File","E"
! Quarterly
	data "  pr State Unemployment Compensation Report","S:\acsGL\PRStatUC","prstatuc","E"
	data "  pr 941 Summary","S:\acsGL\PR941","PR941","E"
	data "  Zero QTD Payroll Info","S:\General Ledger\Accountants\Zero QTD Payroll Info","Zero QTD Payroll Info","E"
! Annually
	data "  pr Accumulated Trial Balance","S:\General Ledger\Print Accumulated Trial Balance","acGLAcTb","ES33"
	data "  Clear Accumulated Transactions","S:\acsGL\acGLClr","acGLClr","E"
	data "  Close Books at Year End","S:\General Ledger\Close Books at Year End","acGLClos","E"
	data "  Enter Budget Amounts","S:\acsGL\BudInpt","BudInpt","E"
	data "  Prior Period Adjustments","S:\General Ledger\Prior Period Adjustments","Prior Period Adjustments","E"
	data "  pr W-2's","S:\acsGL\PRW2","PRW2","E"
	data "  Create Electronic W-2s","S:\acsGL\GLELECW2","GLElecW2","E"
	data "  pr 1099s","S:\acsGL\GLPRT109","GLPrt109","E"
	data "  Create Electronic 1099s","S:\acsGL\ELEC1099","Elec1099","E"
	data "  Zero YTD Payroll Information","S:\acsGL\PRZYTD","PRZYTD","E"
	data "  Zero YTD Vendor Purchases","S:\acsGL\GLZER109","GLZer109","E"
! Utilities
	data "  pr Chart of Accounts","S:\acsGL\acGLCHRT","acGLChrt","E"
	data "  Generate Reversing Entries","S:\acsGL\Reverse","","E"
	data "  Remove Entries Posted by Mistake","S:\acsGL\Unpost","","E"
	data "  Consolidate Master Files","S:\acsGL\COMBGL","CombGL","E"
	data "  Create ASCII Files","S:\acsGL\ASCIIGLM","ASCIIGLM","E"
	data "  Create Checkbook System Files","S:\acsGL\CLBLD","CLBld","E"
	read m$(i+=1),pgm$(i),hlpg$(i),status$(i)
	form pos 1,c 20,c 35,n 3,3*n 1
L1640: execute "drop [Q]\GLmstr\acGLPGMN.h[cno]"
	close #1: ioerr ignore
	open #1: "Name=[Q]\GLmstr\acGLPGMN.h[cno],Use,RecL=76",internal,outIn,relative 
	for j1=1 to 20 
		if j1>lrec(3) then 
			write #1,using L1690: nxtpgm$(j1),nxtdesc$(j1),pn(j1),cp(j1),prim(j1),srq(j1)
			L1690: form pos 1,c 35,c 35,n 3,3*n 1
		end if
	next j1
	close #1: ioerr ignore
	close #3: ioerr ignore
Xit: fnXit
include: Ertn
