autoLibrary
on error goto Ertn
fnTop(program$)
fnTos
mylen=22 : mypos=mylen+3 : lc=0
fnLbl(lc+=1,1,'Oldest Date to Retain:',mylen,1)
fnTxt(lc,mypos,10,0,0,'1003')
resp$(1)=str$(date('ccyymmdd')-50000)
lc+=1
fnLbl(lc+=1,1,'All transactions with a',mylen*2,2)
fnLbl(lc+=1,1,'date prior to this date will be removed.',mylen*2,2)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 or ckey=99 then goto Xit
open #hTrans=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],NoShr',i,outIn,k
open #hNew=fnH: 'Name=[Q]\PRmstr\Work1.[wsid],Size=0,RecL=224,replace',i,outi,r
rd1=val(resp$(1))
do
	dim tdc(10)
	dim cp(32)
	read #hTrans,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,tdn,prd,realckno,mat tdc,mat cp eof EoTrans
	if prd>=rd1 then ! Keep it
		write #hNew,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,tdn,prd,realckno,mat tdc,mat cp
	end if
loop
EoTrans: !
	close #hNew:
	close #hTrans,free:
	fnRename('[Q]\PRmstr\Work1.[wsid]','[Q]\PRmstr\PayrollChecks.h[cno]')
	fnIndex('[Q]\PRmstr\PayrollChecks.h[cno]','[Q]\PRmstr\checkidx.h[cno]','1 17')
goto Xit

Xit: fnXit

include: ertn

