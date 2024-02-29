! formerly S:\acsUB\ubUnPen
autoLibrary
on error goto Ertn
fnTop(program$)

fnTos
fnLbl(1,1,'Penalty Date:',26,1)
fnTxt(1,28,10,0,0,'3')
resp$(1)=''
fnCmdSet(2)
ckey=fnAcs(mat resp$)
ubpendat=val(srep$(resp$(1),'/',''))
if ckey=5 then goto Xit

fnAutomatedSavePoint('before')
open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,outIn,k
open #hTrans=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr',i,outIn,k
open #hTrans2=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr',i,outIn,k
dim serviceName$(10)*20
dim service$(10)
dim taxCode$(10)*1
dim penatly$(10)*1
fnGetServices(mat serviceName$,mat service$,mat taxCode$,mat penalty$)

dim z$*10
dim tg(11)
do
	read #hTrans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2': z$,tDate,tCode,tAmt,mat tg eof Xit
	if tDate=ubpendat and tCode=2 then
		dim gb(10)
		read #hCustomer,using 'form pos 1,C 10,pos 292,PD 4.2,pos 388,10*PD 5.2',key=z$: z$,bal,mat gb nokey NextTrans
		bal-=tAmt
		for j=1 to 10
			if uprc$(penalty$(j))='Y' then gb(j)-=tg(j) ! subtract penalty breakdown from balance breakdown
		next j
		rewrite #hCustomer,using 'form pos 1,C 10,pos 292,PD 4.2,,pos 388,10*PD 5.2',key=z$: z$,bal,mat gb
		delete #hTrans:
	end if
	NextTrans: !
loop
Xit: !
close #hCustomer: ioerr ignore
close #hTrans: ioerr ignore
close #hTrans2: ioerr ignore

fnXit
include: ertn no
