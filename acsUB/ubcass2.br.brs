! Replace S:\acsUB\ubCass2
! -- Place Certified File Back on PC
 
	autoLibrary
	on error goto Ertn
 
	dim nam$*30,sta$*30,city$*23,csz$*30,opt1$(4),cap$*128,txt$*100
	dim a$*5,b$*4,c$*3,bc$*12,cr$*4,d$(2)
 
 
	fnTop(program$,cap$="Place Certified File Back on PC")
 
	sn$="ubCass2" : _
	fnTos(sn$) : _
	respc = 0
	fnLbl(1,1,"Path to Returned Postal Diskette:",33,1)
	opt1$(1)="A:\" : _
	opt1$(2)="C:\" : _
	opt1$(3)="E:\" : _
	opt1$(4)="F:\" : _
	fncomboa("AB",1,35,mat opt1$) : _
	resp$(respc+=1)=opt1$(1)
	fnLbl(3,1,"This program prints:")
	fnLbl(4,1,"Listing of Customer Addresses that could not be certified",58,2)
	fnCmdSet(2)
L180: fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dv$=resp$(1)
 
! Open #1: "Name="&DV$&"Cass1.Dat,RecL=223",External,Input Ioerr 180
	open #1: "Name="&dv$&"Cass1.Dat,RecL=113",external,input ioerr L180
	open #2: "Name=[Q]\UBmstr\Cass1.h[cno],RecL=112,Replace",internal,output
	open #3: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #4: "Name=[Q]\UBmstr\UBAdrBil.h[cno],Shr",internal,outIn,relative
	fnopenprn
	pr #255: "\qc {\b "&cap$ : _
	pr #255: "Listing of Customer Addresses that could not be certified" : _
	pr #255: date$("mm/dd/ccyy")&"}" : _
	pr #255: "\ql "
READ_A: !
! Read #1,Using 300: Z$,NAM$,STA$,CITY$,STATE$,A$,B$,C$,CR$ Eof END1
	read #1,using L301: z$,nam$,sta$,city$,state$,a$,b$,d$,cr$ eof END1
! bC$=A$&B$&C$  when return batch total
	bc$=a$&b$&d$ ! when have to generate barcode
	form pos 1,c 10,pos 12,c 30,pos 41,c 30,pos 106,c 23,pos 151,c 2,pos 166,c 5,pos 172,c 4,pos 192,c 3,pos 202,c 4
L301: form pos 1,c 10,pos 12,c 30,pos 41,c 30,pos 71,c 23,pos 94,c 2,pos 96,c 5,c 4,c 2,pos 107,c 4
	if rtrm$(bc$(6:9))="" then : _
		pr #255,using L320: z$,nam$,sta$,city$,state$,bc$ : goto READ_A
L320: form pos 1,c 12,2*c 32,c 25,c 4,c 12
! Gosub CREATE_CHECK_DIGIT  ! already done by melissa read bc$ as c 12 instead of c 11
	gosub CREATE_CHECK_DIGIT ! melissa returned only 11 digits
	write #2,using L350: z$,nam$,sta$,city$,state$,bc$,cr$
L350: form pos 1,c 10,2*c 30,c 23,c 2,c 12,c 4
	gosub L580
	goto READ_A
 
END1: !
	close #1:
	close #2:
	execute "Index [Q]\UBmstr\Cass1.h[cno],[Q]\UBmstr\Cass1Idx.h[cno],1,10,Replace,DupKeys -n"
	fncloseprn
Xit: fnXit
 
CREATE_CHECK_DIGIT: !
	bc$=rtrm$(bc$)
	if bc$="" then goto L560
	c1=0
	for j=1 to len(bc$)
		c1=c1+val(bc$(j:j)) conv L520
L520: next j
	c1$=str$(c1) : _
	l1=len(c1$) : _
	c2=val(c1$(l1:l1))
	if c2=0 then cd$="0" else cd$=str$(10-c2)
	bc$=bc$&cd$
L560: return
 
L580: csz$=rtrm$(city$)&", "&state$&" "&bc$(1:5)
	goto L640 ! don't update any addresses
	read #3,using "Form POS 385,PD 3",key=z$: aba nokey L640
	if aba=0 then goto L630
	rewrite #4,using "Form POS 41,2*C 30",rec=aba: sta$,csz$ noRec L630
	goto L640
L630: rewrite #3,using "Form POS 71,2*C 30",key=z$: sta$,csz$
L640: return
 
include: ertn
