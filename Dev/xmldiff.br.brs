	autoLibrary
	print newpage
! dim fileOld$*256
! dim fileNew$*256
	dim lineold$*512
	dim linenew$*512
	mat loclist$(0)
	fnaddonec(mat loclist$,'18')
	fnaddonec(mat loclist$,'19')
	fnaddonec(mat loclist$,'62')
	fnaddonec(mat loclist$,'63')
	fnaddonec(mat loclist$,'64')
	fnaddonec(mat loclist$,'65')
	fnaddonec(mat loclist$,'75')
	fnaddonec(mat loclist$,'77')
	fnaddonec(mat loclist$,'78')
	fnaddonec(mat loclist$,'79')
	fnaddonec(mat loclist$,'80')
	fnaddonec(mat loclist$,'81')
	fnaddonec(mat loclist$,'82')
	fnaddonec(mat loclist$,'LA')
	fnaddonec(mat loclist$,'OC')
	for locitem=1 to udim(mat loclist$)
		location$=loclist$(locitem)
		dim filenameold$(0)*256
		dim filenamenew$(0)*256
		open #hint=fnH: 'name='&env$('temp')&'\diff.dat,kfname='&env$('temp')&'\diff.idx,recl=2048,kps=1,kln=8,replace',internal,outIn,keyed
		fngetdir2('C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\',mat filenameold$, '/s','e_q4.'&location$&'.*.*',mat unuseddate$,mat unusedtime$,1)
		fngetdir2('C:\Users\John\Desktop\pbjExport\' ,mat filenamenew$, '/s','e_q4.'&location$&'.*.*',mat unuseddate$,mat unusedtime$,1)
		for fileitem=1 to udim(filename$)
! .! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\'&filename$(fileItem)
! .! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\'&filename$(fileItem)
! .! fileOld$='C:\SageAX\ACC\wb\cdsk\pbjsent - q4 - submitted\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
! .! fileNew$='C:\Users\John\Desktop\pbjExport\08-pbj_emp_q4_081916-082616\e_q4.64.08192016-08262016.xml'
!
			open #hold=fnH: 'name='&filenameold$(fileitem),display,input
			open #hnew=fnH: 'name='&filenamenew$(fileitem),display,input
			do
				linput #hold: lineold$ eof DIFFFINIS
READNEW: !
				linput #hnew: linenew$ eof DIFFFINIS
				if lineold$(1:len('<hireDate>'))='<hireDate>' then
					hire$=lineold$(11:pos(lineold$,'<',13)-1)
				else if lineold$(1:len('<employeeId>'))='<employeeId>' then
					empid$=lineold$(13:pos(lineold$,'<',13)-1)
					hire$=''
! .    ! pr 'Emp: '&empid$
				else if lineold$<>linenew$ then
					term$=linenew$(18:27)
					if linenew$(1:len('<terminationDate>'))='<terminationDate>' and lineold$(1:len('<terminationDate>'))='<terminationDate>' then
						write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineold$&'   New: '&linenew$ ! &'  FileNew:'&filenameNew$(fileItem)
						terminationdatechangecount+=1
					else if linenew$(1:len('<terminationDate>'))<>'<terminationDate>' then
						write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,'*** Old: '&lineold$&'   New: '&linenew$ ! &'  FileNew:'&filenameNew$(fileItem)
						unusualcount+=1
						pause
					else
						write #hint,using 'form pos 1,C 18,C 10,C 10,C 400': empid$,hire$,term$,linenew$
						goto READNEW
					end if
				end if
			loop
DIFFFINIS: !
			close #hold:
			close #hnew:
		next fileitem
		if ~exists('C:\Users\John\Desktop\pbjFix') then execute 'mkdir C:\Users\John\Desktop\pbjFix'
		open #hdiff=fnH: 'name=C:\Users\John\Desktop\pbjFix\Loc'&location$&'_Q4_Missed_Terms_Update.xml,recl=2048,replace',display,output
		print #hdiff: '<?xml version="1.0" encoding="ASCII"?>'
		print #hdiff: '<nursingHomeData xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="nhpbj_2_00_0.xsd">'
		print #hdiff: '<header fileSpecVersion="2.00.0">'
		print #hdiff: '<facilityId>N046026</facilityId>'
		print #hdiff: '<stateCode>KS</stateCode>'
		print #hdiff: '<reportQuarter>4</reportQuarter>'
		print #hdiff: '<federalFiscalYear>2016</federalFiscalYear>'
		print #hdiff: '<softwareVendorName>RH Positive</softwareVendorName>'
		print #hdiff: '<softwareVendorEmail>ray.espinoza@adventistcare.org</softwareVendorEmail>'
		print #hdiff: '<softwareProductName>RH Positive</softwareProductName>'
		print #hdiff: '<softwareProductVersion>2016</softwareProductVersion>'
		print #hdiff: '</header>'
		print #hdiff: '<employees>'
		restore #hint:
		do
			read #hint,using 'form pos 1,v 18,v 10,v 10,v 374': emp$,hire$,term$,linenew$ eof REPORTFINIS
			if linenew$<>lineold$ then
! .  ! print #hDiff: lineNew$
				print #hdiff: '<employee>'
				print #hdiff: '<employeeId>'&emp$&'</employeeId>'
				print #hdiff: '<hireDate>'&hire$&'</hireDate>'
				if trim$(term$)<>'' then
					print #hdiff: '<terminationDate>'&term$&'</terminationDate>'
				end if
				print #hdiff: '</employee>'
				lineold$=linenew$
			end if
		loop
REPORTFINIS: !
		print #hdiff: '</employees>'
		print #hdiff: '</nursingHomeData>'
		close #hdiff:
		close #hint:
		if terminationdatechangecount>0 then print location$&' has '&str$(terminationdatechangecount)&' Termination Date Changes' : terminationdatechangecount=0
		if unusualcount>0 then print location$&' has '&str$(unusualcount)&' unusual diffs' : unusualcount=0
	next locitem
!
