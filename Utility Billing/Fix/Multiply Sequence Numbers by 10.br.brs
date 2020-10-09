! modifies customer records depending upon how the program is configured, sequences services, etc
! r: setup library, dims, on err, fnTop, etc
	autoLibrary
	on errror goto ERTN
	fnTop(program$)
 ! /r
	! gosub OldWorkFromFixedWidthList
	! r: primary loop setup
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7,gb(10),extra$(11)*30
	dim extra(23)
	dim df$*1
	open #h_customer:=fnH: "Name=[Q]\UBmstr\Customer.h[cno]",internal,outIn,relative
	F_CustomerSequenceOnly: form pos 1743,N 7
	! /r
	do ! r: primary loop
		read_count+=1
		read #h_customer,using F_CustomerSequenceOnly: sequenceNumber eof PrimaryFinis
		didChange=0
		didChange+=fn_multiplySequenceByTen(sequenceNumber)
		if didChange then
			rewrite #h_customer,using F_CustomerSequenceOnly: sequenceNumber
			write_count+=1
		end if
	loop ! /r
	PrimaryFinis: !
		pr 'read_count=';read_count
		pr 'write_count=';write_count : pause
	Xit: fnXit
include: ertn
def fn_multiplySequenceByTen(&sequenceNumber)
	drbtReturn=0
	if len(str$(sequenceNumber))>999999 then
		pr bell;'sequence number ('&str$(sequenceNumber)&') wrong length to convert.'
		pause
	else
		sequenceNumber=sequenceNumber*10
		drbtReturn=1
	end if
	fn_multiplySequenceByTen=drbtReturn
fnend
