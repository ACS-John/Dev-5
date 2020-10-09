!  Replace S:\acsPR\prW2b
! pr W-2s for second state - chained to from newprw2a (now Payroll\PrintW2Forms)  though (9/27/2016)
	! FORM TYPE 22222 FOR WAGE AND TAX STATEMENT  - 1993
 
autoLibrary
on error goto Ertn
 
dim em$(3)*30,ss$*11,d(14),w(12),s(12),t(12),ta(1)
dim a$(3)*40,b$*12,f$*8,g$*8,h$*8,d$(10)*8,e$(10)*12
dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),eno$*8
 
open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
read #1,using L140: mat a$,b$,mat d$,loccode,mat e$
L140: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12
for j=1 to 3: a$(j)=a$(j)(1:30): next j
close #1:
pr newpage
pr f "10,5,C 60": "CHECK POSITION OF W2 FORMS FOR SECOND STATE"
pr f "12,15,C 60": "PRESS ENTER TO CONTINUE:"
input fields "12,40,C 1,I,N": pause$
open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
open #2: "Name=[Q]\PRmstr\PRW2ADDR.h[cno],NoShr",internal,input
open #hAddr:=fnH: "Name=[Temp]\Addr."&session$&",NoShr",internal,input,relative
read #hAddr,using 'form pos 1,n 10.2,n 1',rec=1: ficamax,w1
ficamaw=ficamax*10
first=1
ADDR_LOOP_TOP: ! r:
	read #2,using 'form pos 1,pd 3': addr eof EO_ADDR
	if addr=1 then goto ADDR_LOOP_TOP
	read #hAddr,using L310,rec=addr: eno,tcd,ty21,ty3,tlwh,pf$
	L310: form pos 1,n 8,n 2,3*pd 5.2,c 8
	if read1=1 then goto L360
	oldeno=eno
	oldtcd=tcd
	read1=1
	L360: !
	if oldeno=eno and oldtcd=tcd then goto L380
	if oldeno><eno or oldtcd><tcd then goto EMP_READ
	L380: !
	w(9)=w(9)+ty21 ! STATE WAGES
	w(7)=w(7)+ty3 ! STATE WH
	if tlwh=0 then goto L430 ! NO LOCAL WH
	w(10)=w(10)+ty21 ! LOCAL WAGES
	w(8)=w(8)+tlwh ! LOCAL WH
	L430: !
	oldeno=eno
	oldtcd=tcd
goto ADDR_LOOP_TOP
EMP_READ: !
	eno$=lpad$(str$(oldeno),8)
	read #1,using 'form pos 9,3*c 30,c 11',key=eno$: mat em$,ss$
	g$=ltrm$(eno$)
	stcode$=e$(oldtcd)
	state$=d$(oldtcd)(1:2)
	gosub PRINTW2
	mat w=(0)
	if lstrec=1 then goto FINIS
	goto L380
	EO_ADDR: !
	lstrec=1
	oldeno=eno
	oldtcd=tcd
goto EMP_READ ! /r
FINIS: ! r:
	close #1:
	close #2:
	close #hAddr:
	fncloseprn
goto Xit ! /r
Xit: fnXit
PRINTW2: ! r: pr W2 FORM
	fnopenprn
	! if rtrm$(file$(255))="PRN:/SELECT" then pr #255,using ' form pos 1,c 9': hex$("2B0205000A1021")
	pr #255,using L740: ss$
	pr #255,using L770: b$,w(2),w(1)
	L740: form pos 23,c 12,skip 2
	pr #255,using L770: a$(1),w(5),w(3)
	pr #255,using L770: a$(2),w(11),w(12)
	L770: form pos 5,c 32,2*pic(zzzzzzzzzzzz.zz),skip 2
	pr #255,using L770: a$(3),w(6),0
	pr #255,using L770: g$,w(4),dcb
	pr #255,using L770: em$(1),amt(1)+amt(2),0
	pr #255,using L820: em$(2),desc$(3),desc$(5)
	L820: form pos 5,c 32,c 15,c 16,skip 2
	pr #255,using L820: em$(3),desc$(4),desc$(6)
	pr #255,using L850: px$,x$
	L850: form skip 1,pos 51,c 1,pos p1,c 1,skip 2
	pr #255,using L870: state$,stcode$,w(9),w(7),pf$,w(10),w(8)
	L870: form pos 4,c 2,x 2,c 13,n 10.2,n 9.2,x 1,c 8,2*n 9.2,skip 2
	gosub NEWPGE
return ! /r
NEWPGE: ! r:
	pl=33 ! INSERT PAGE LENGTH IN LINES
	sk=pl-(krec(255)-int(krec(255)/pl)*pl)
	pr #255,using L1240: ""
	L1240: form c 1,skip sk
return ! /r
include: ertn
