! Replace S:\acsUB\UBRECRTR
 
	autoLibrary
	on error goto Ertn
 
	dim p$*10,z$*10,o(2),adr(2)
	dim gb(10),tg(11),d(15)
 
	fnTop(program$)
	fnTos
	mylen=30
	mypos=mylen+2
	fnLbl(1,1,"Transaction Date:" ,mylen,1)
	fnTxt(1,mypos,10,0,0,"3")
	resp$(1)=date$("ccyymmdd")
	fnLbl(3,1,"Warning ! Do not continue",mylen,1)
	fnLbl(4,1,"without consulting ACS",mylen,1)
	fnLbl(4,15,"",mylen,1)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
L180: !
x=pos(resp$(1),"/",1)
if x>0 then resp$(1)(x:x)="": goto L180
trandate=val(resp$(1))
if ckey=5 then goto Xit
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],NoShr",internal,outIn,keyed
open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],RecL=102,Replace",internal,output
do
	L240: read #1,using L250: z$,bal,mat d,mat gb eof READ_CUSTOMER_EOF
	L250: form pos 1,c 10,pos 292,pd 4.2,pos 217,15*pd 5,pos 388,10*pd 5.2
	if bal>0 then
		if bal<0 then tcode=3 else tcode=1
		if tcode=3 then tamount=-bal else tamount=bal
		for j=1 to 10
			tg(j)=gb(j)
			if tcode=3 then tg(j)=-tg(j)
		next j
		tbal=bal
		wr=d(1) : wu=d(3) : er=d(5) : eu=d(7) : gr=d(9) : gu=d(11)
		write #2,using L360: z$,trandate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
		L360: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	end if
loop
 
READ_CUSTOMER_EOF: !
	close #1:
	close #2:
	execute "Index [Q]\UBmstr\UBTransVB.h[cno]"&' '&"[Q]\UBmstr\UBTrIndx.h[cno] 1 19 Replace DupKeys -n"
goto Xit
Xit: fnXit
include: ertn
