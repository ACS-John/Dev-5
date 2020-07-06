! Replace S:\acsCL\AgedAP
! pr Aged AP Listing
 
autoLibrary
on error goto Ertn
 
dim vnam$*30,de$*50,fd$*30,ft(3)
dim io1$(7)*30,bk(3,2),t1(5),t2(5)
dim mo(13)
 
fnTop(program$,"Accounts Payable Listing (Aged)")
mo(01)=000 : mo(02)=031 : mo(03)=059 : mo(04)=090
mo(05)=120 : mo(06)=151 : mo(07)=181 : mo(08)=212
mo(09)=243 : mo(10)=273 : mo(11)=304 : mo(12)=334
mo(13)=365
 
def fn_jd(x)
	jd0=mo(int(x*.0001))+(int(x*.01)-int(x*.0001)*100)+int(fndate_mmddyy_to_ccyymmdd(x)*.0001)*365+int(int(fndate_mmddyy_to_ccyymmdd(x)*.0001)/4)
	if int(fndate_mmddyy_to_ccyymmdd(x*.0001))/4=int(int(fndate_mmddyy_to_ccyymmdd(x*.0001))/4) and int(x*.0001)<3 then jd0=jd0-1
	fn_jd=jd0
fnend
 
fnTos
respc=0
fnLbl(1,38,"",1,1)
fnLbl(1,1,"Aging Date:",23,1)
fnTxt(1,25,10,0,1,"1001")
resp$(respc+=1)=str$(d1)
fnLbl(3,1,"Aging Break 1:",23,1)
fnTxt(3,25,3,0,1,"30",0,"Aging break 1 is the maximum age of an invoice (in days) to be grouped in the first category")
resp$(respc+=1)="30"
fnLbl(4,1,"Aging Break 2:",23,1)
fnTxt(4,25,3,0,1,"30")
resp$(respc+=1)="60"
fnLbl(5,1,"Aging Break 3:",23,1)
fnTxt(5,25,3,0,1,"30")
resp$(respc+=1)="90"
fnCmdSet(2): fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit
d1=val(resp$(1))
bk(1,2)=val(resp$(2))
bk(2,2)=val(resp$(3))
bk(3,2)=val(resp$(4))
bk(1,1)=0
if bk(2,2)>0 then bk(2,1)=bk(1,2)+1
if bk(3,2)>0 then bk(3,1)=bk(2,2)+1
fnopenprn
open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.H[cno],KFName=[Q]\CLmstr\UnPdIdx1.H[cno],Shr",internal,outIn,keyed
open #paymstr=2: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,input,keyed
gosub HDR
READ_PAYTRANS: !
read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof END1
if upa=0 then goto READ_PAYTRANS
if hvn$<>"" and hvn$><vn$ then gosub VNTOT
hvn$=vn$
gosub AGE2
pr #255,using 'Form POS 1,C 10,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS P1,N 12.2,POS 111,N 12.2': vn$,iv$,ivd,dd,de$,upa,upa pageoflow NEWPGE
goto READ_PAYTRANS
 
NEWPGE: pr #255: newpage: gosub HDR : continue
 
HDR: f1=1
	pr #255,using 'Form POS 1,C 8,Cc 86': date$,env$('cnam')
	pr #255,using 'Form POS 1,C 8,Cc 86': time$,env$('program_caption')
	pr #255,using 'Form POS 1,C 4,N 4,POS 9,Cc 86': "Page",pg+=1,"As of "&cnvrt$("pic(zz/zz/zz)",d1)
	pr #255: ""
	pr #255,using 'Form POS 1,C 62,N 8,"-",N 3,N 8,"-",N 3,N 8,"-",N 3,2*C 12': "Payee  #  Invoice Numb    Date      Date    Description       ",mat bk,"    Over "&str$(bk(3,2)),"     Total"
	gosub PRINT_
return
 
PRINT_: !
	pr #255: "________  ____________  ________  ________  __________________  __________  __________  __________  __________  __________" pageoflow NEWPGE
return
PRINT2_: !
	pr #255: "                                                                __________  __________  __________  __________  __________" pageoflow NEWPGE
return
 
END1: !
	gosub VNTOT
	pr #255,using 'Form POS 33,C 30,5*N 12.2': "Final Total",mat t2
	pr #255: tab(63);rpt$("  ==========",5)
	fncloseprn
goto Xit
 
Xit: fnXit
 
VNTOT: !
	vnam$=""
	if hvn$<>"" then
		read #paymstr,using 'Form POS 9,C 30',key=hvn$: vnam$ nokey L830
	end if
	L830: !
	pr #255: tab(63);rpt$("  ----------",5)
	pr #255,using 'Form POS 33,C 30,5*N 12.2': vnam$,mat t1
	gosub PRINT2_
	mat t2=t2+t1 : mat t1=(0)
return
 
AGE2: !
	if ivd=0 then goto L950
	das=max(0,int(fn_jd(d1)-fn_jd(ivd)))
	for j=1 to 3
		if das>=bk(j,1) and das<=bk(j,2) then goto L940
	next j
	L940: p1=j*12+51 : t1(j)+=upa : t1(5)+=upa
	L950: !
return
include: Ertn
