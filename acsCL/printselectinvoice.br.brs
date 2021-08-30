! Replace S:\acsCL\PrintSelectInvoice
autoLibrary
on error goto Ertn
 
dim newkey$*20
dim de$*30,gl(3),t1(5),sc3$(5)*55,up$(4),d(2),nam$*30
 
fnTop(program$,"Print Selected Invoice Listing")
 
open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,i,r
read #20,using 'Form POS 150,2*N 1,C 2',rec=1: mat d,bc$
close #20:
bankcode=val(bc$)
 
open #bankmstr=12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal, outin, keyed
open #paymstr1=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
open #paytrans=fnH: "Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
open #unpdaloc=5: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\uaidx2.h[cno],Shr",internal,outIn,keyed
sc3$(2)=" Total Invoices selected for payment-Bank "
sc3$(2)=sc3$(2)&ltrm$(str$(bankcode))&":"
sc3$(3)=" Balance after paying selected Invoices:"
sc3$(4)=" Total Invoices not selected for payment from Bank "
sc3$(4)=sc3$(4)&rtrm$(str$(bankcode))&":"
sc3$(5)=" Total of all unpaid Invoices (less credits) on file:"
read #bankmstr,using 'Form Pos 45,PD 6.2,PD 6.2,G 8',key=bc$,release: bal,upi,lcn$ nokey L250
L250: !
sc3$(1)=" Bank Code "&bc$&" Current Bank Balance:"
t1(1)=bal
fnopenprn
restore #paytrans,key>="                    ": nokey EO_PAYTRANS
hvn$="" : t1=pg1=0
gosub HEADER
do
	READ_PAYTRANS: !
	read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,checkNumber$,dp,gde eof EO_PAYTRANS
	bank_code=bc
	if rtrm$(vn$)="" or pcde><1 or bc<>bankcode then t1(4)+=upa: goto READ_PAYTRANS ! reject and add invoices not selected or for other bank accounts
	if hvn$<>"" and hvn$><vn$ then gosub READ_AND_PRINT
	gosub BREAKDOWN
	t1+=upa : hvn$=vn$ : t1(2)+=upa
loop
NEWPGE: pr #255: newpage: gosub HDR : continue
HDR: ! r:
	pr #255,using 'Form POS 1,C 8,Cc 82': date$,env$('cnam')
	pr #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg+=1,"Unpaid Invoice File Listing"
	pr #255: ""
	pr #255: "                             Invoice    Due     PO Number                                   Pay   Bank   Check     Date "
	pr #255: "Ref#  Payee #   Invoice Numb   Date    Date     GL Number   Description            Amount   Code  Code   Number    Paid "
	pr #255: "____  ________  ____________  ______  ______  ____________  __________________  __________  ____  ____  ________  ______"
return ! /r
 
DONE: ! r:
	close #bankmstr:
	close #paymstr1:
	close #paytrans:
goto Xit ! /r
Xit: fnXit
PGOF: pr #255: newpage : gosub HEADER : continue
HEADER: ! r:
	pr #255,using 'Form POS 1,PIC(ZZ/ZZ/ZZ),CC 107': prd,env$('cnam')
	pr #255,using 'Form POS 1,C 4,N 4,CC 107,SKIP 1,POS 1,C 8': "Page",pg1+=1,"Selected Invoice Listing for Bank "&ltrm$(str$(bankcode)),date$
	pr #255: "  Payee                  Invoice     Due                                                   Line Item                       Item"
	pr #255: " Number   Invoice Numb    Date      Date    Inv. Description       Amount     GL Number    Description                    Amount"
	pr #255: "________  ____________  ________  ________  __________________  ____________  ____________ ______________________________ __________"
return ! /r
READ_AND_PRINT: ! r:
	nam$=""
	read #paymstr1,using 'Form POS 9,C 30',key=hvn$,release: nam$ nokey ignore
	pr #255,using 'Form POS 65,C 14,SKIP 1,POS 30,C 33,PIC(ZZZ,ZZZ,ZZZ.##CR)': "____________",nam$,t1 pageoflow PGOF
	pr #255: "" pageoflow PGOF
	t1=0
return ! /r
EO_PAYTRANS: ! r:
	if hvn$<>"" then gosub READ_AND_PRINT
	t1(3)=t1(1)-t1(2): t1(5)=t1(4)+t1(2)
	for j=1 to 5
		pr #255,using 'Form POS 5,C 55,PIC($$$$,$$$,$$$,$$$.##CR)': sc3$(j),t1(j)
	next j
	fncloseprn
	on fkey 99 ignore
goto Xit ! /r
BREAKDOWN: ! r:
	startxx=0
	restore #unpdaloc,key>=vn$&iv$: nokey EO_UNPDALOC
	do
		read #unpdaloc,using 'Form pos 1,c 20,Pos 21,N 3,N 6,N 3,PD 5.2,C 30': newkey$,mat gl,aa,de$ eof EO_UNPDALOC nokey EO_UNPDALOC
		if newkey$<>vn$ & iv$ then goto EO_UNPDALOC
		if sum(gl)<>0 or aa<>0 then ! only non-zero allocations print
			if startxx=0 then
				pr #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,PIC(ZZZ,ZZZ,ZZZ.##CR),POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': vn$,iv$,val(up$(1)),val(up$(2)),up$(4),upa,mat gl, de$, aa pageoflow PGOF
			else if startxx>0 then
				pr #255,using 'Form POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': mat gl, de$, aa
			end if
			startxx+=1
		end if
	loop
	EO_UNPDALOC: !
	if startxx=0 then
		pr #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,PIC(ZZZ,ZZZ,ZZZ.##CR),POS 79,N 3,N 6,N 3,X 1,C 30,X 1,PIC(---,--#.##)': vn$,iv$,val(up$(1)),val(up$(2)),up$(4),upa,mat gl, de$, aa pageoflow PGOF
		! only on noRec if startxx=0 (no allocations)
	end if
return
include: ertn
