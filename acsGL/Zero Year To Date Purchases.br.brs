! formerly S:\acsGL\glZer109
! unused old program

autoLibrary
fnTop(program$)
on error goto Ertn

open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno],Shr",internal,outIn,keyed ioerr Xit
open #2: "Name=[Q]\GLmstr\gltr1099.h[cno]",internal,outIn,relative

SCR1: !
	fnTos
	lc=0 : mylen=55 : mypos=mylen+3 : center=2 : right=1
	fnLbl(lc+=1,1,"* * *   Warning   * * *",60,center)
	fnLbl(lc+=1,1,"This selection will dump all old purchase transactions from each",width,0)
	fnLbl(lc+=1,1,"vendor (payee) record. This selection should only be run at year end",width,0)
	fnLbl(lc+=1,1," after all 1099 forms have been printed.:",mylen,0)
	fnLbl(lc+=1,1," Enter ZERO to continue:",mylen,right)
	fnTxt(lc,mypos,5)
	resp$(1)=""
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	pas$=resp$(1)
	if lwrc$(pas$)<>lwrc$("zero") then goto SCR1

OLDEST_DATE: !
	fnTos
	lc=0 : mylen=30 : mypos=mylen+3 : width=0
	fnLbl(lc+=1,1,"Oldest Date to be Retained:",mylen,right)
	fnTxt(1,mypos,8,0,left,'CCYYMMDD',0,'For example, if you wantto dump all transactions up to the beginning of the new year, you would enter the first day of the new year.') 
	resp$(1)=str$(transactionendingdate)
	fnLbl(lc,45,"",0,right)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	lastdate=val(resp$(1))
do
	dim re$*12
	dim de$*30
	read #2,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30': trvn$,da,amt,re$,de$ eof Finis
	x=fndate_mmddyy_to_ccyymmdd(da)
	if x<lastdate then delete #2,rec=rec(2):
loop
Finis: !
	close #2:
	fnRemoveDeletedRecords('[Q]\GLmstr\gltr1099.h[cno]')
	fnIndex('[Q]\GLmstr\gltr1099.h[cno]','[Q]\GLmstr\gltridx1.h[cno]','1 8')
L440: !

Xit: fnXit
 
include: ertn
