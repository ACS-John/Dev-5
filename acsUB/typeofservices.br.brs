! Replace S:\acsUB\TypeOfServices   !   Type of Services (All Companies)
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(60)*20
	dim cap$*128
	dim item$(61)*20,colhdr$(61)
 
	fnTop(program$,cap$="Type of Services (All Companies)")
	fnTos(sn$:="TypeOServices")
	fnLbl(1,1,sn$&'1',20,2,3)
	colhdr$(1)='CNo'
	for j=1 to 10
		colhdr$(j+01)='Name '&str$(j)
		colhdr$(j+11)='Code '&str$(j)
		colhdr$(j+21)='Tax  '&str$(j)
		colhdr$(j+31)='Pen  '&str$(j)
		colhdr$(j+41)='Sub  '&str$(j)
		colhdr$(j+51)='Ord  '&str$(j)
	next j
	mat colmask$(60)
	mat colmask$=("")
	fnflexinit1(filename$:='toses',2,1,10,72,mat colhdr$,mat colmask$,1)
	fngetdir2("[Q]\UBmstr\ubData\",mat service_file$, '/ON','Service.h*')
! fnpause
	for service_file_item=1 to udim(service_file$)
		item$(1)=service_file$(service_file_item)(10:len(service_file$(service_file_item)))
		open #h_service=15: "Name=[Q]\UBmstr\ubData\"&service_file$(service_file_item),internal,outIn,relative
		read #h_service,using "form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*G 2,10*G 2",rec=1: mat item$(2:61)
		fnflexadd1(mat item$)
		close #h_service:
	next service_file_item
	fnCmdSet(4)
	fnAcs(sn$,win,mat resp$,ckey)
	fnXit
 
include: Ertn
 
