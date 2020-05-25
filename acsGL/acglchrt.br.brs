! Replace S:\acsGL\acglChrt
! General Ledger Chart of Accounts
 
	autoLibrary
	on error goto Ertn
 
	dim gl(2,3),io2$(6),io1$(2),wrd1$(2)*46,cap$*128,p$(20)*50
	dim d$*50,dat$*20,cnam$*40,cap$*128,resp$(10)*50
 
	fnTop(program$,cap$="Chart of Accounts")
	fncno(cno,cnam$)
	fndat(dat$)
	process=fnprocess
 
	cap$="General Ledger Chart of Accounts"
	if process=1 then sel=1 : goto L330
 
	pr newpage
	fnTos(sn$="ChartAccoutnts") : _
	mylen=50: mypos=mylen+3 : right=1
	fnFra(1,1,2,70,"Chart of Accounts"," ",0)
	fnOpt(1,3,"Print Financial Statement Reference Numbers",0,1) : _
	resp$(rc+=1)="False"
	fnOpt(2,3,"Print Account Numbers and Names only",0,1) : _
	resp$(rc+=1)="True"
	fnLbl(5,1,"Beginning General Ledger Number (blank for all):",mylen,right)
	fnqgl(5,mypos,0,2) : _
	resp$(1)=""
	fnLbl(6,1,"Ending General Ledger Number (blank for all):",mylen,right)
	fnqgl(6,mypos,0,2) : _
	resp$(1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="True" then sel=1 else sel=2
	if trim$(resp$(3))<>"" then gl1$=fnagl$(resp$(3))
	if trim$(resp$(4))<>"" then gl2$=fnagl$(resp$(4))
L330: open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
 
	pr newpage
	fnopenprn
	pr #255,using "Form pos 1,C 20,Cc 90": date$('mm/dd/yy'),cnam$
	pr #255,using "Form pos 1,C 20,Cc 90": time$,"Chart of Accounts"
	pr #255,using 'form pos 1,Cc 130': dat$
	pr #255:
	gosub L680
L420: read #1,using L430: dno,ano,sno,d$,br,sbr,ir,sir,fr,sfr eof L650
L430: form pos 1,n 3,n 6,n 3,c 50,6*pd 3
	gl3$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	if trim$(gl1$)="" then goto L461
	if gl3$<gl1$ then goto L420
L461: if trim$(gl2$)="" then goto L480
	if gl3$>gl2$ then goto L650
L480: if sel=1 then goto L490 else goto L530
L490: pr #255,using L500: dno,ano,sno,d$,br,ir,fr,sbr,sir,sfr pageoflow L570
L500: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,x 3,n 5,x 5,n 5,x 5,n 5,x 7,n 5,x 5,n 5,x 5,n 5
	goto L420
 
L530: pr #255,using L540: dno,ano,sno,d$ pageoflow L570
L540: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,skip 1
	goto L420
 
L570: pr #255: newpage
	pr #255,using "Form pos 1,C 20,Cc 90": date$('mm/dd/yy'),cnam$
	pr #255: time$;tab(57);"Chart of Accounts"
	pr #255,using 'form pos 1,Cc 130': dat$
	pr #255:
	gosub L680
	goto L420
 
L650: fncloseprn
	goto Xit
 
L680: if sel=2 then goto L730
	pr #255,using L700: "********** Primary *********","********* Secondary ********"
L700: form pos 71,c 28,pos 103,c 28,skip 2
	pr #255,using L720: "Bal Sheet","Income","Chg Fin","Bal Sheet","Income","Chg Fin"
L720: form pos 71,c 9,x 4,c 6,x 2,c 7,x 4,c 9,x 4,c 6,x 2,c 7
L730: if sel=1 then goto L780
	pr #255: "" : _
	pr #255,using L750: "Account #","Description"
L750: form pos 2,c 9,pos 38,c 11
	goto L810
 
L780: pr #255,using L800: "Account #","Description","Reference","Reference","Position","Reference","Reference","Position"
	pr #255: ""
L800: form pos 2,c 9,pos 38,c 11,pos 71,c 9,x 1,c 9,x 1,c 8,x 4,c 9,x 1,c 9,x 1,c 8
L810: return
 
Xit: fnXit
 
include: Ertn
