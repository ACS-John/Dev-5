! Replace S:\Core\brs2br

autoLibrary
on error goto Ertn


pr ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
pr ' * WARNING:  This program is about to convert all (*.brs) source'
pr ' *           code files into (*.br) working program files.      '
pr ' *                (including subdirectories)                    '
pr ' *           Continue [Enter]             Cancel[Esc]           '
pr ' *                                                              '
pr ' * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
input fields '22,1,Cu 1,AE,N': warn$
if cmdkey=5 or cmdkey=99 or warn$='Q' or warn$='X' or warn$='N' then 
	goto Xit
end if

rpl$='Save'
if uprc$(rpl$)<>uprc$('SAVE') and uprc$(rpl$)<>uprc$('Replace') then 
	rpl$='Replace' ! default to Replace unless correctly specified
end if
dim dir$*256
if dir$='' then 
	dir$='\\DISKSTATION\public\ACS\acs.402\' 
	pr 'Directory not set default used (Default Dir: '&dir$&' )'
end if
if dir$(len(dir$):len(dir$))='\' then dir$=dir$(1:len(dir$)-1)
if exists(dir$)=0 then
	pr 'Directory not found...'
	goto Xit
end if
dim filename$(999)*60
fnGetDir2(dir$,mat filename$)
for tmp=1 to udim(mat filename$)
	if rtrm$(filename$(tmp))='' then goto L280
next tmp
L280: !
mat filename$(tmp)
pr 'Files Found = '&str$(tmp)

open #20: 'Name=Proc.[acsUserId],Size=0,Replace',d,o

for j=1 to udim(mat filename$)
	if filename$(j)='' then goto AFT_LOOP1
	tmpa$=uprc$(filename$(j)(len(filename$(j))-3:len(filename$(j))))
	if tmpa$<>uprc$('.brs') then goto LOOP1_NEXT
	pr #20: ' ! Begin process of file ( '&filename$(j)&' ) '
	pr #20: ' ! '
	pr #20: 'Load '&dir$&'\'&filename$(j)&',Source'
	pr #20: ' ! '
	pr #20: rpl$&' '&dir$&'\'&filename$(j)(1:len(filename$(j))-1) ! removes the 's' from the ext changing .brs ext to .br
	pr #20: ' ! '
	pr #20: ' ! Completed update from source: '&filename$(j)
	pr #20: ' ! '
	LOOP1_NEXT: !
next j

AFT_LOOP1: !
	mat filename$(999)
	pr #20: 'Load S:\Core\br2brs'
	pr #20: ' ! Now you''ve done it.  You''ve gone and updated all your'
	pr #20: ' ! program files (*.br) from source code files (*.brs)'
	close #20:
chain 'proc=proc.[acsUserId]'

Xit: stop

include: ertn

