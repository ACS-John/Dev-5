! Replace S:\acsUB\ubMetRB2
! pr Complete Route Sheets
autoLibrary
on error goto Ertn

dim z$*10,e$(4)*30,x$*10,f$(1)*12,f$(3)*12
dim z2$*10,e2$(4)*30,f12$*12,f32$(12),f22$(12),a(7),a2(7)
dim snm$(10)*20,a(7),option$(5),extra(13),service$(3)*26,ms$(13)*3
dim txt$*50,resp$(9)*50

fnTop(program$,"Route Book Pages")
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
open #11: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 

! this section+the comboA on the first screen is just what you need 
! for a fnCurrently availableServiceTypeComboBox
ms$(1)="DEC"
ms$(2)="NOV"
ms$(3)="OCT"
ms$(4)="SEP"
ms$(5)="AUG"
ms$(6)="JUL"
ms$(7)="JUN"
ms$(8)="MAY"
ms$(9)="APR"
ms$(10)="MAR"
ms$(11)="FEB"
ms$(12)="JAN"
ms$(13)="DEC"
fnGetServices(mat snm$,mat srv$)
x=0
for j=1 to 4
	if j=1 and trim$(snm$(j))="Water" then option$(x+=1)=srv$(j)    :            service=service+1
	if j=3 and trim$(snm$(j))="Electric" then option$(x+=1)=srv$(j) :            service=service+1
	if j=3 and trim$(snm$(j))="Lawn Meter" then option$(x+=1)=srv$(j) : _
		service=service+1
	if j=4 and trim$(snm$(j))="Gas" then option$(x+=1)=srv$(j) : _
		service=service+1
next j
option$(x+=1)=" "
mat option$(x)
mat service$(service)
service =udim(service$)

MENU1: ! 
	fnTos
	mylen=35 : mypos=mylen+3 : respc=lc=0
	fnLbl(lc+=1,1,"Route Number:",mylen,1)
	fncmbrt2(lc,mypos) 
	resp$(1)="1"
	fnLbl(lc+=1,1,"Service Type - 1st Service:",mylen,1)
	fncomboa("ubrate3",lc,mypos,mat option$) 
	resp$(2)=option$(1)
	fnLbl(lc+=1,1,"Service Type - 2nd Service:",mylen,1)
	fncomboa("ubrate3",lc,mypos,mat option$) 
	resp$(3)=option$(1)
	fnFra(5,1,4,45,"Single Wide or Double Wide","Allow one or two customers per page.",0)
	fnOpt(1,2,"Singe Wide",0,1) 
	resp$(4)="True"
	fnOpt(2,2,"Double Wide - Different customers",0,1) 
	resp$(5)="False"
	fnOpt(3,2,"Double Wide - Different services",0,1) 
	resp$(6)="False"
	if env$('client')="Franklinton" or env$('client')="Divernon" then resp$(4)="False": resp$(6)="True"
	fnChk(11,28,"Select Accounts to Print:",1) 
	resp$(7)="False"
	fnFra(13,1,2,45,"Option for printing","The system can pr the actual form or just fill in the blanks on a pre-printed form.",0)
	fnOpt(1,2,"Print complete form",0,2) 
	resp$(8)="True"
	fnOpt(2,2,"Fill in the blanks",0,2)
	if env$('client')="Carrizo" then resp$(9)="True" else resp$(9)="False"
	fnCmdSet(3) 
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if uprc$(resp$(1))=uprc$("[All]") then route=0 else         route=val(resp$(1))
	svt$=resp$(2)
	svt2$=resp$(3)
	if resp$(4)="True" then width=1
	if resp$(5)="True" then width=2
	if resp$(6)="True" then width=3
	if resp$(7)="True" then selectone=1 else selectone=0
	if resp$(8)="True" then formoption=1 ! complete form
	if resp$(9)="True" then formoption=2 ! fill in blanks
	service=1 ! only printing one service if not                                                answered as all
	for j=1 to 10
		if svt$=trim$(srv$(j)) then service$=snm$(j)(1:10)
		if svt2$=trim$(srv$(j)) then service2$=snm$(j)(1:10)
	next j

	on fkey 5 goto DONE
	if formoption=2 then fnpa_open else fnopenprn
	if selectone=1 then goto SELECTONE
	LOOP_TOP: ! 
	x=0
	! print
	L870: read #1,using L1060: z$,mat e$,f1$,d1,d3,f3$,c4,f2$,d5,d7,d9,d11,book1,mat a,d13,extra16,b2,b5 eof DONE
	z2$=f12$="" : d12=d32=d52=d72=d92=d112=c42=0 : mat e2$=("")
	if route>0 and book1<>route then goto L870
	if width=1 or width=3 then goto L920
	read #1,using L1060: z2$,mat e2$,f12$,d12,d32,f32$,c42,f22$,d52,d72,d92,d112,book2,mat a2 eof L1060
	L920: !
	if svt$=srv$(1) and width=1                   then previous=d1 : columnonename$=srv$(1): firstmeter$=f1$: secondmeter$="": firstcode$=str$(a(1)): secondcode$="" ! water only service
	if svt$=srv$(1) and width=2                   then previous=d1 : previous2=d12  : firstmeter$=f1$: secondmeter$=f12$: firstcode$=str$(a(1)): secondcode$=str$(a2(1)) ! two different water customers
	if svt$=srv$(1) and svt2$=srv$(3) and width=3 then previous=d1 : previous2=d5   : firstmeter$=f1$: secondmeter$=f2$: firstcode$=str$(a(1)): secondcode$=str$(a(3)) ! water and electric
	if svt$=srv$(1) and svt2$=srv$(4) and width=3 then previous=d1 : previous2=d9   : firstmeter$=f1$: secondmeter$=f3$: firstcode$=str$(a(1)): secondcode$=str$(a(4)) ! water and gas
	if svt$=srv$(3) and width=1                   then previous=d5 :                  firstmeter$=f2$: secondmeter$="": firstcode$=str$(a(3)): secondcode$="" ! electric only
	if svt$=srv$(3) and width=2                   then previous=d5 : previous2=d52  : firstmeter$=f2$: secondmeter$=f22$: firstcode$=str$(a(3)): secondcode$=str$(a2(3)) ! electric or lawn meter
	if svt$=srv$(3) and svt2$=srv$(1) and width=3 then previous=d5 : previous2=d1   : firstmeter$=f2$: secondmeter$=f1$: firstcode$=str$(a(3)): secondcode$=str$(a(1)) ! electric or lawn meter  and water
	if svt$=srv$(3) and svt2$=srv$(4) and width=3 then previous=d5 : previous2=d9   : firstmeter$=f2$: secondmeter$=f3$: firstcode$=str$(a(3)): secondcode$=str$(a(4)) ! electric or lawn meter  and gas
	if svt$=srv$(4) and width=1                   then previous=d9 : firstmeter$=f3$: secondmeter$="": firstcode$=str$(a(4)): secondcode$="" ! gas only
	if svt$=srv$(4) and width=2                   then previous=d9 : previous2=d92  : firstmeter$=f3$: secondmeter$=f32$: firstcode$=str$(a(4)): secondcode$=str$(a2(4)) ! gas
	if svt$=srv$(4) and svt2$=srv$(1) and width=3 then previous=d9 : previous2=d1   : firstmeter$=f3$: secondmeter$=f1$: firstcode$=str$(a(4)): secondcode$=str$(a(1)) ! gas and water
	if svt$=srv$(4) and svt2$=srv$(3) and width=3 then previous=d9 : previous2=d5   : firstmeter$=f3$: secondmeter$=f2$: firstcode$=str$(a(4)): secondcode$=str$(a(3)) ! gas and electric
	if route=0 or route=book2 then goto L1060
	z2$=f12$="" : d12=d32=d52=d72=d92=d112=c42=0 : mat e2$=("")
L1060: form pos 1,c 10,4*c 30,c 12,pos 217,pd 5,pos 227,pd 5,pos 373,c 12,pos 213,pd 4,pos 361,c 12,pos 237,pd 5,pos 247,pd 5,pos 257,pd 5,pos 267,pd 5,pos 1741,n 2,pos 143,7*pd 2,pos 277,pd 5,pos 1818,n 3,pos 161,pd 4.2,pos 173,pd 4.2
	if formoption=2 then gosub BLANKS : goto L1700
	if width=2 or width=3 then for j=1 to 2: pr #255,using L1320: "|": next j
	if width=1 then for j=1 to 2: pr #255,using L1320: "|": next j
	if width=1 then pr #255,using L1160: env$('cnam')(1:37),"|"
	if width=2 or width=3 then pr #255,using L1160: env$('cnam')(1:37),"|",env$('cnam')(1:37)
	if width=1 then pr #255,using L1160: service$
	if width=2 then pr #255,using L1160: service$,"|",service$
	if width=3 then pr #255,using L1160: service$,"|",service2$
! 
L1160: form pos 2,cc 37,pos 45,c 1,x 1,cc 37,skip 1
	if width=2 or width=3 then for j=1 to 2: pr #255,using L1320: "|": next j
	if width=1 then for j=1 to 2: pr #255,using L1320: "|": next j
	form pos 14,c 12,pos 45,c 1,pos 54,c 12,skip 1
	if width=1 then pr #255,using L1290: e$(1),"|"
	if width=2 then pr #255,using L1290: e$(1),"|",e2$(1)
	if width=3 then pr #255,using L1290: e$(1),"|"
	if width=1 then pr #255,using L1290: "Meter #: "&firstmeter$,"|"
	if width=2 then pr #255,using L1290: "Meter #: "&firstmeter$,"|","Meter #: "&firstmeter$
	if width=3 then pr #255,using L1290: "Meter #: "&firstmeter$,"|","Meter #: "&secondmeter$
	if width=1 then pr #255,using L1290: "Rate Code: "&firstcode$,"|"
	if width=2 then pr #255,using L1290: "Rate Code: "&firstcode$,"|","Rate Code: "&firstcode$
	if width=3 then pr #255,using L1290: "Rate Code: "&firstcode$,"|","Rate Code: "&secondcode$
L1290: form pos 10,c 30,pos 45,c 1,pos 50,c 30,skip 1
	if width=2 or width=3 then for j=1 to 5: pr #255,using L1320: "|": next j
	if width=1 then for j=1 to 5: pr #255,using L1320: "|": next j
L1320: form pos 45,c 1,skip 1
	if width=2 or width=3 then pr #255,using L1340: year$,"|",year$ else pr #255,using L1340: year$,"|"
L1340: form pos 2,c 4,pos 45,c 1,x 1,c 4,skip 1
L1350: form pos 6,c 80
! 
	if width=1 then pr #255,using L1350: "=======================================|"
	if width=2 or width=3 then pr #255,using L1350: "=======================================|======================================="
	if width=1 then pr #255,using L1350: "Dat|  READINGS  |CONSUMPTION|  REMARKS |"
	if width=2 or width =3 then pr #255,using L1350: "Dat|  READINGS  |CONSUMPTION|  REMARKS |Dat|  READINGS  |CONSUMPTION|  REMARKS "
	if width=1 then pr #255,using L1350: "___|____________|___________|__________|"
	if width=2 or width=3 then pr #255,using L1350: "___|____________|___________|__________|___|____________|___________|__________"
	for j=1 to 12
		if width=2 or width=3 then pr #255,using L1350: ms$(j)&"|            |           |          |"&ms$(j)&"|            |           |          "
		if width=1 then pr #255,using L1350: ms$(j)&"|            |           |          |"
		if width=2 or width=3 then pr #255,using L1350: "___|____________|___________|__________|___|____________|___________|__________"
		if width=1 then pr #255,using L1350: "___|____________|___________|__________|"
	next j
	if width=1 then pr #255,using L1510: ms$(13)&"|",previous,"| PREVIOUS  |","|"
	if width=2 or width=3 then pr #255,using L1510: ms$(13)&"|",previous,"| PREVIOUS  |","|"&ms$(13)&"|",previous2,"| PREVIOUS  |"
L1510: form pos 6,c 4,pic(zzzzzzzzzzzz),pos 22,c 14,pos 45,c 5,pic(zzzzzzzzzzzz),c 14,skip 1
	if width=1 then pr #255,using L1350: "   |            | READING   |          |   | "
	if width=2 or width=3 then pr #255,using L1350: "   |            | READING   |          |   |            | READING   |          "
	if fbc><0 then e$(2)="Disconnect"
	if width=1 then pr #255,using L1350: "=======================================|"
	if width=2 or width=3 then pr #255,using L1350: "=======================================|======================================="
	if width=1 then pr #255,using L1660: "|","                "," ACCOUNT  ","|"
	if width=2 then pr #255,using L1660: "|","                "," ACCOUNT  ","|","                "," ACCOUNT  ","|"
	if width=3 then pr #255,using L1660: "|","                "," ACCOUNT  ","|"
	if width=1 or width=3 then pr #255,using L1670: e$(2)(1:25),z$,"|"
	if width=2 then pr #255,using L1670: e$(2)(1:25),z$,"|",e2$(2)(1:25),z2$
	if width=1 or width=3 then pr #255,using L1670: e$(3)(1:25),"","|"
	if width=2 then pr #255,using L1670: e$(3)(1:25),"","|",e2$(3)(1:25),""
	if width=1 or width=3 then pr #255,using L1670: e$(4)(1:25),"","|"
	if width=2 then pr #255,using L1670: e$(4)(1:25),"","|",e2$(4)(1:25),""
	L1660: form pos 45,c 1,skip 1,pos 6,c 25,c 10,pos 45,c 1,pos 47,c 25,c 10,skip 1,pos 45,c 1,skip 1
	L1670: form pos 6,c 25,c 10,pos 45,c 1,pos 47,c 25,c 10,skip 1
	for j=1 to 3: pr #255,using L1320: "|": next j
	pr #255: newpage
	L1700: !
	if selectone=1 then goto SELECTONE
goto LOOP_TOP

SELECTONE: ! 
	fnTos
	fnLbl(1,1,"Account:",16,1)
	! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040
	if trim$(z$)<>"" then : _
		txt$="Last Account entered was "&z$ : _
		fnLbl(3,1,txt$,44,1) else : _
		txt$="" : _
		fnLbl(3,1,txt$,44,1)
	fncmbact(1,18) ! 
	resp$(1)=a$
	fnCmdKey("&Next",1,1,0,"Accept this record for printing") 
	fnCmdKey("&Complete",5,0,1,"Print all selected records")
	fnAcs(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10) 
	if trim$(a$)="" then goto DONE
	if ckey=5 then goto DONE

	read #11,using L1060,key=a$: z$,mat e$,f1$,d1,d3,f3$,c4,f2$,d5,d7,d9,d11,book1,mat a,d13,extra16,b2,b5 nokey SELECTONE
	form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
	goto L920

DONE: ! 
	close #1: ioerr L1910
	if formoption=2 then gosub RELEASE_PRINT: goto Xit
L1910: fncloseprn
Xit: fnXit
RELEASE_PRINT: ! r:
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
goto Xit ! /r
BLANKS: ! r: fill in blanks using prace
	pr #20: 'Call Print.MyFontSize(10)'
	column1=30 
	column2=90 
	column3=150
	lyne=137
	txt$=f1$ 
	lyne+=8.5 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=f3$ 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(ZZZZZZ)",d13) 
	lyne+=8.5 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+11)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zzz)",extra16) 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zzzzzzZZZZZZZZZ.zz)",b2) 
	lyne+=8.5 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zZZZZZZZZZ.zz)",b5) 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	pr #20: 'Call Print.MyFontSize(14)'
	txt$=e$(2) 
	lyne+=52 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
	txt$=e$(3) 
	lyne+=20 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
	txt$=e$(4) 
	lyne+=5 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
	txt$=e$(1) 
	lyne+=14 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
	txt$=z$ 
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-16)&','&str$(lyne-36)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	fnpa_newpage
return ! /r
include: ertn
