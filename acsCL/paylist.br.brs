! Replace S:\acsCL\PayList
! payee listing
 
	autoLibrary
	on error goto Ertn
 
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,de$*30,io1$(2),text$*25,item1$(2)*20
	dim contact$*30,email$*50,fax$*12,myact$*20
	dim gl$*12,gldesc$*30,key$*19,tr$(5)*35,payeegl$*12,payeekey$*12
 
	fnTop(program$,cap$="Payee Listing")
	fndat(dat$)
	fncno(cno,cnam$)
	open #1: "Name=[Q]\CLmstr\PAYMSTR.h[cno],KFName=[Q]\CLmstr\PAYIDX1.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\PAYMSTR.h[cno],KFName=[Q]\CLmstr\PAYIDX2.h[cno],Shr",internal,outIn,keyed
	open #trmstr2=31: "Name=[Q]\CLmstr\TRMSTR.h[cno],KFName=[Q]\CLmstr\TRIDX2.h[cno],Shr",internal,outIn,keyed
	open #payeegl=3: "Name=[Q]\CLmstr\payeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Shr",internal,outIn,keyed
	pr newpage
	fnTos("ubnamlst") : _
	respc=0
	text$="Report Heading Date:" : _
	fnLbl(1,1,text$,25,1)
	fnTxt(1,27,20) : _
	resp$(respc+=1)=dat$
	fnLbl(2,1,"Print Order:",25,1)
	item1$(1)="Payee Number" : _
	item1$(2)="Alphabetic by Name"
	fncomboa("paylist-srt",2,27,mat item1$,tt$) : _
	resp$(respc+=1)=item1$(1)
	fnChk(4,29,"Print G/L Breakdowns:",1) : _
	resp$(respc+=1)="False"
	fnChk(6,29,"Print Total Payments:",1) : _
	resp$(respc+=1)="False"
	fnLbl(8,1,"Transaction Starting Date:",25,1)
	fnTxt(8,27,8,0,0,"3",0,'Blank for All (Only applicable if need Total Payments Printed)') : _
	resp$(respc+=1)=" "
	fnLbl(9,1,"Transaction Ending Date:",25,1)
	fnTxt(9,27,8,0,0,"3",0,'Blank for All (Only applicable if need Total Payments Printed)') : _
	resp$(respc+=1)=""
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	seq$=resp$(2)(1:1)
	fndat(dat$,2)
	if resp$(3)(1:1)="T" then printgl=1 else printgl=0 ! pr general ledger breakdowns
	if resp$(4)(1:1)="T" then printtotal=1 else printtotal=0 ! pr total payments
	begdate=val(resp$(5))
	enddate=val(resp$(6)) ! ending date for adding purchases for period of time
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
	fnopenprn
	gosub L730
L450: if seq$="A" then read #2,using L460,release: vn$,nam$,ad1$,ad2$,csz$,ph$ eof L790 else read #1,using L460,release: vn$,nam$,ad1$,ad2$,csz$,ph$,contact$,email$,fax$,myact$ eof L790
L460: form pos 1,c 8,4*c 30,pos 153,c 12,c 30,c 50,c 12,c 20
	if printtotal=0 then goto L580
	transactionstotal=0
	key$=vn$&cnvrt$('pic(Z#)',0)&cnvrt$("pic(#)",1)&rpt$(chr$(0),8) : _
	restore #trmstr2,key>=key$: nokey EO_FLEX2
READ_TRMSTR2: !
	read #trmstr2,using 'Form Pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof EO_FLEX2
	if trim$(vn$)<>trim$(tr$(4)) then goto EO_FLEX2
	if begdate<>0 and begdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR2
	if enddate<>0 and enddate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR2
	transactionstotal+=tr3
	goto READ_TRMSTR2
EO_FLEX2: !
L580: if printtotal= 0 then pr #255,using L690: vn$,nam$,ad1$,ad2$(1:25),csz$,ph$ else pr #255,using L690: vn$,nam$,ad1$,ad2$(1:25),csz$,ph$,transactionstotal pageoflow L710
	if printgl<>1 then goto L690
	restore #payeegl,key>=vn$: nokey EO_TEST
READ_STANDARD_BREAKDOWNS: !
	read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_TEST
	if vn$<>payeekey$ then goto EO_TEST
	if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto EO_TEST
	pr #255,using L660: payeegl$,percent,gldesc$
L660: form pos 11,c 12,x 2,pic(----.zz),x 2,c 30,skip 1
	goto READ_STANDARD_BREAKDOWNS
EO_TEST: !
L690: form pos 1,c 8,x 2,2*c 30,c 25,c 30,c 12,pic($$$$,$$$.## cr),skip 1
	goto L450
L710: pr #255: newpage: gosub L730
	continue
L730: pr #255,using L740: date$('mm/dd/yy'),cnam$,time$,"Payee Listing",dat$
L740: form pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 60,c 20,skip 1,pos dattab,c 20,skip 2
	pr #255: "Payee No  Payee Name                    Address                       Address                  City, State Zip              Phone"
	pr #255: "________  __________________________    __________________________    _____________________    ___________________________  ____________"
	form pos 1,c 132,skip 1
return
L790: fncloseprn
	goto Xit
 
Xit: fnXit
 
include: Ertn
