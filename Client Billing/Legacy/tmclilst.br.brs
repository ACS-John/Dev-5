on fkey 5 goto L490
on error goto Ertn

autoLibrary
fnTop(program$,"Client Listing")
form pos 1,n 2,c 40,c 20,pos 83,n 6,pos 89,2*n 1
fnopenprn
dim z$*5,a$*30,prg$*20,app(20),ma(20),a3$*30
namtab=42-int(len(rtrm$(env$('cnam')))/2)
pr newpage
pr f "10,10,c 52,n": "ENTER 1 FOR NUMERIC LISTING, ENTER 2 FOR ALPHABETIC"
L140: input fields "10,65,n 1,eu,n": numalp conv L230
	pr newpage
	pr f "10,5,C 60": "ENTER APPLICATION NUMBER TO pr OR BLANK FOR ALL:"
L144: input fields "10,58,N 1,UE,N": sapp conv L144
	if sapp<0 or sapp>20 then goto L144
	if numalp<1 or numalp>2 then goto L140
	if numalp=1 then goto L170 else goto L190
L170: open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,i,k
	goto L200
L190: open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",i,i,k
L200: pr newpage
	pr f "10,10,c 46,n": "ENTER DATE FOR CLIENT LISTING IN MMDDYY FORMAT"
	pr f "10,60,n 6,n": dat
L230: input fields "10,60,n 6,eu,n": dat conv L230
	if dat<10100 or dat>123199 then goto L200
	pr newpage
	pr f "10,25,c 48,n": "CLIENT LISTING IN PROCESS"
	pr f "23,2,c 30,n": "Press F5 to stop"
	gosub L370
L290: read #1,using L300: z$,a$,a3$,mat app,mat ma eof L490
L300: form pos 1,c 5,c 30,x 30,c 30,pos 375,20*n 1,20*pd 3.2
	if sapp=0 then goto L304
	if app(sapp)=0 then goto L290
L304: for j=1 to 20
		if app(j)=0 then goto L308
		if ma(j)>0 then app(j)=2
L308: next j
	pr #255,using L320: z$,a$ pageoflow L340 ! MAT APP
L320: form pos 1,c 5,pos 10,c 30,20*n 3,skip 1
! pr #255: TAB(10);A3$ PAGEOFLOW 340
	goto L290
L340: pr #255: newpage
	gosub L370
	continue 
L370: p1=p1+1
	pr #255,using L390: date$,env$('cnam'),"PAGE",p1
L390: form skip 3,pos 1,c 8,pos namtab,c 40,pos 76,c 5,n 4,skip 1
	pr #255,using L410: time$,"CLIENT LISTING"
L410: form pos 1,c 8,pos 35,c 16,skip 1
	pr #255,using L430: dat
L430: form pos 39,pic(zz/zz/zz),skip 3
	pr #255,using L450: "CLIENT"
L450: form pos 2,c 6,skip 1
	pr #255,using L470: "NUMBER","CLIENT NAME"
L470: form pos 2,c 6,pos 14,c 26,c 60,skip 2
	return 
L490: close #1: ioerr L500
L500: fncloseprn
	if uprc$(rtrm$(prg$))="S:\Client Billing\Legacy\client" then chain prg$
	goto XIT

XIT: fnxit
include: ertn