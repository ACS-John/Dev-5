! Replace S:\Core\br2brs
! creates a brs file for every br file

autoLibrary
on error goto Ertn

dim dir$*255,filename$(999)*255

if dir$="" then
	dir$="\\DISKSTATION\public\ACS\acs.402"
	pr "Directory not set default used (Default Dir: "&dir$&" )"
end if
if dir$(len(dir$):len(dir$))="\" then dir$=dir$(1:len(dir$)-1)
option$="/s /a-d /on" ! widows xp ... /s   - include sub dirs.
!                                         /a-d - exclude dir. names
!                                         /on  - order by name
fngetdir2(dir$,mat filename$,option$)

for tmp=1 to udim(filename$)
	if rtrm$(filename$(tmp))="" then goto L170
next tmp
L170: !
mat filename$(tmp)
pr ' =)   Files Found = '&str$(tmp)

open #20: "Name=Proc.[session],Size=0,Replace",d,o

for j=1 to udim(filename$)
	ValidFileTest: !
	if filename$(j)="" then goto AFT_LOOP1
	tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j))))
	if tmpa$=uprc$(".FIX") then 
		goto VftPass 
	else if tmpa$=uprc$(".CNV") then 
		goto VftPass 
	else if tmpa$=uprc$(".br") then 
		goto VftPass
	end if
	tmpa$=uprc$(filename$(j)(len(filename$(j))-2:len(filename$(j))))
	if tmpa$=uprc$(".br") then goto VftPass
	ValidFileTestFAIL: !
	goto LOOP1_NEXT
	VftPass: !
	pr #20: "Load "&filename$(j)
	pr #20: "List >"&filename$(j)&"s" !  ..br + s = ...brs
	pr #20: " ! file created: "&filename$(j)&"s"
	LOOP1_NEXT: !
next j

AFT_LOOP1: !
	mat filename$(999)
	pr #20: "load S:\Core\br2brs"
	pr #20: " ! 'Now all your *.br files have little *.brs parallels'"
	close #20:
chain "proc=proc.[session]"

Xit: stop

include: ertn

