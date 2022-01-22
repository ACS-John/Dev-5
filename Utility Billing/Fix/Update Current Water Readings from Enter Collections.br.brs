autoLibrary
fntop(program$)
fnAutomatedSavePoint('before fix')
! open #hCustomer1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k  ! was file #1, but it was getting closed incorrectly
! F_CUSTOMER_C: form pos 1,c 10,pos 41,c 30,pos 143,7*pd 2,pos 1821,n 1,pos 217,15*pd 5,pos 354,c 1,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,pos 1954,c 12,pos 1906,c 12
! open #hWork=fnH: 'Name=[Q]\UBmstr\Reads_and_Chgs.h[cno],KFName=[Q]\UBmstr\Reads_and_Chgs-Key.h[cno],Shr,Use,RecL=74,KPs=1,KLn=10',i,outIn,k
! open #hCustomer2=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",i,outIn,k
! open #hCustomer3=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",i,outIn,k
! open #hCustomer4=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",i,outIn,k
! open #hCustomer5=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,outIn,k

dim rc$(0)*10,rcN(0)
hRc=fn_openFio('UB Reads_and_Chgs',mat rc$,mat rcN, 1)
dim c$(0)*200,cN(0)
hC=fn_openFio('UB Customer',mat c$,mat cN)
do
	read #hRc,using form$(hRc): mat rc$,mat rcN eof EoRc
	read #hC,using form$(hC),key=rc$(rc_acct): mat c$,mat cN nokey Cnokey
	oldReading$=str$(cN(c_s01readingCur))
	newReading$=str$(rcN(rc_xa1))
	if oldReading$<>newReading$ then
		cN(c_s01readingCur)=rcN(rc_xa1)
		rewrite #hC,using form$(hC),key=rc$(rc_acct): mat c$,mat cN
		! r: append to note file
			open #h_notefile=fnH: "Name="&fnNoteDir$&"\"&trim$(c$(c_account))&".txt,Use",d,o
			pr #h_notefile: '** Current Reading programmatically updated from '&oldReading$&' to '&newReading$&' on '&date$('mm/dd/ccyy')&' at '&time$&' **'
			close #h_notefile: 
		! /r
	end if
	
loop
EoRc: !
pr 'completed successfully'
goto Xit

Cnokey: !
	pr bell;'nokey on '&rc$(rc_acct)
	pause
goto Xit

Xit: !
fnXit

include: fn_open
