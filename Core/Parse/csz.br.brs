! Replace S:\Core\parse\CSZ.br
	dim ml$(2)*80
 
FNCSZ: ! extract  CITY$,STATE$,ZIP$ from CSZ$
def library fncsz(&csz$,&city$,&state$,&zip$)
		library 'S:\Core\Library': fnerror,fnMsgBox
		csz$=rtrm$(csz$)
		do
		csz$=srep$(csz$,'  ',' ')
		loop until pos(csz$,'  ')<=0
CSZ_SET_P1: !
		p1=pos(csz$,".",1)
		if p1>0 then csz$(p1:p1)="": goto CSZ_SET_P1
		l1=len(csz$) : _
		p1=pos(csz$,",",1)-1
		if p1=-1 then p1=pos(csz$," ",1)-1
		p2=pos(csz$," ",p1+3) : _
		city$=uprc$(rtrm$(csz$(1:p1))(1:15))
		if p2=0 then p2=pos(csz$," ",-1)+2 : _
			! just in case they forgot a space after the state before the zip, : _
			! but didn't forget the space after the city comma.
		if uprc$(city$(1:3))="FT " then city$(1:3)="Fort " else : _
			if uprc$(city$(1:3))="FT." then city$(1:3)="Fort "
		state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2)) : _
		zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1)))) : _
		zip5$=zip$(1:5) : _
		zip4$="" : _
		l2=len(zip$)
		if l2<9 then goto Xit
		on error goto MESSAGEBOX
		p2=pos(csz$," ",p1+3) : _
		city$=rtrm$(csz$(1:p1))(1:15) : _
		state$=rtrm$(csz$(p2-2:p2))(1:2) : _
		zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1)))) : _
		zip5$=zip$(1:5) : _
		zip4$="" : _
		l2=len(zip$)
		goto L180
MESSAGEBOX: !
		mat ml$(2) : _
		ml$(1)="You have a bad address: "&csz$ : _
		ml$(2)="You should fix the address and run this option again." : _
		fnMsgBox(mat ml$,resp$,cap$,48)
		goto Xit
L180: ! pr STATE$ ! XXX
		goto Xit
 
include: ertn
 
Xit: fnend
 
