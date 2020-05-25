! Replace S:\Core\br2brs
! creates a brs file for every br file
 
	autoLibrary
	on error goto Ertn
 
	dim dir$*255,filename$(999)*255
 
	if dir$="" then : _
		dir$="\\DISKSTATION\public\ACS\acs.402" : _
		pr "Directory not set default used (Default Dir: "&dir$&" )"
	if dir$(len(dir$):len(dir$))="\" then dir$=dir$(1:len(dir$)-1)
	option$="/s /a-d /on" ! widows xp ... /s   - include sub dirs. : _
	! .                                       /a-d - exclude dir. names : _
	! .                                       /on  - order by name
	fngetdir(dir$,mat filename$,option$)
! __
	for tmp=1 to udim(filename$)
		if rtrm$(filename$(tmp))="" then goto L170
	next tmp
L170: mat filename$(tmp)
	pr ' =)   Files Found = '&str$(tmp)
! __
	open #20: "Name=Proc."&wsid$&",Size=0,Replace",display,output
! __
	for j=1 to udim(filename$)
VALID_FILE_TEST: !
		if filename$(j)="" then goto AFT_LOOP1
		tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j)))) : _
		if tmpa$=uprc$(".FIX") then goto VALID_FILE_TEST_PASS else : _
			if tmpa$=uprc$(".CNV") then goto VALID_FILE_TEST_PASS else : _
				if tmpa$=uprc$(".br") then goto VALID_FILE_TEST_PASS
		tmpa$=uprc$(filename$(j)(len(filename$(j))-2:len(filename$(j)))) : _
		if tmpa$=uprc$(".br") then goto VALID_FILE_TEST_PASS
VALID_FILE_TEST_FAIL: !
		goto LOOP1_NEXT
VALID_FILE_TEST_PASS: !
		pr #20: "Load "&filename$(j)
		pr #20: "List >"&filename$(j)&"s" ! ...br + s = ...brs
		pr #20: " ! file created: "&filename$(j)&"s"
LOOP1_NEXT: !
	next j
! __
AFT_LOOP1: !
	mat filename$(999)
	pr #20: "load S:\Core\br2brs"
	pr #20: " ! 'Now all your *.br files have little *.brs parallels'"
	close #20:
	chain "proc=proc."&wsid$
 
Xit: stop
 
include: Ertn
 
