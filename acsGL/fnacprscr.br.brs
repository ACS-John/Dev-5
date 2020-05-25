! Replace S:\acsGL\fnacprScr
! job cost screens???
def library fnacprscr
		autoLibrary
 
		fnTop(program$)
		on error goto Ertn
 
		dim a$(23)*20,floa$(23),io1$(23)
		dim b$(14)*20,flob$(14),io2$(14)
 
		data "Employee #"
		data "Name: F/M/L"
		data "Address"
		data "City,St Zip"
		data "Soc. Sec. Numb"
		data "Gross Wages Y.T.D."
		data "Gross Wages Q.T.D."
		data "Fed W/H Y.T.D."
		data "Fed W/H Q.T.D."
		data "FICA W/H Y.T.D."
		data "FICA W/H Q.T.D."
		data "State W/H Y.T.D."
		data "State W/H Q.T.D."
		data "Local W/H Y.T.D."
		data "Local W/H Q.T.D."
		data "Misc-1 W/H Y.T.D."
		data "Misc-2 W/H Y.T.D."
		data "Tips Y.T.D."
		data "Tips Q.T.D."
		data "Weeks Worked Y.T.D."
		data "Weeks Worked Q.T.D."
		data "EIC Y.T.D."
		data "EIC Q.T.D."
		read mat a$
		data "Employee #"
		data "Date"
		data "Check #"
		data "Gross Pay"
		data "FICA W/H"
		data "Fed W/H"
		data "State W/H"
		data "Local W/H"
		data "Misc-1 W/H"
		data "Misc-2 W/H"
		data "Tips"
		data "Weeks Worked"
		data "EIC"
		data "Net Pay"
		read mat b$
		for j=1 to 5
			floa$(j)=str$(j+3)&",10,C 20,N"
			if j>1 and j<5 then io1$(j)=str$(j+3)&",32,C 25,UT,N"
			if j=1 then io1$(j)="4,32,N 4,UT,N"
			if j=5 then io1$(j)="8,32,C 11,UT,N"
		next j
		x=6
		for j=6 to 22 step 2
			x=x+1
			floa$(j)=str$(x+3)&",3,C 20,N"
			floa$(j+1)=str$(x+3)&",41,C 20,N"
			io1$(j)=str$(x+3)&",23,N 11.2,UT,N"
			io1$(j+1)=str$(x+3)&",62,N 11.2,UT,N"
		next j
		for j=1 to 14
			flob$(j)=str$(j+3)&",2,C 20,N"
			if j>3 then goto L720
			if j=1 then io2$(j)="4,22,N 4,UT,N" else : _
				io2$(j)=str$(j+3)&",22,N 6,UT,N"
			goto L730
L720: io2$(j)=str$(j+3)&",22,N 11.2,UT,N"
L730: next j
		open #1: "Name=[Q]\GLmstr\ACPRSCF.h[cno],NoShr",internal,output ioerr L760
		close #1,free:
L760: open #1: "Name=[Q]\GLmstr\ACPRSCF.h[cno],SIZE=0,RecL=1288,NoShr",internal,output
		write #1,using 'Form POS 1,23*C 20,46*C 18': mat a$,mat floa$,mat io1$
		write #1,using 'Form POS 1,14*C 20,28*C 18': mat b$,mat flob$,mat io2$
		close #1:
		open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],Shr",internal,outIn ioerr L830
		close #1:
	goto Xit
L830: fnacglblds
goto Xit
Xit: fnend
include: Ertn