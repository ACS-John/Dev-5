! Replace S:\acsGL\CmbBud.br
! creates a screen ace combobox for budget files
def library fncmbbud(indexfile$*200)
	autoLibrary

	dim ln$*200,options$(50)*200

	execute 'Dir [Q]\GLmstr\budget*.h[cno] >FlexWork.tmp' ! Ioerr 271
	do
		linput #13: ln$ eof L170
		x=pos(ln$,'<DIR>',1)
		if x>0 and ln$(1:1)<>'.' then 
			options$(j+=1)=ln$(46:len(trim$(ln$)))
		end if
	loop
	L170: !

	close #13:
	pause
	if j<=0 then j=1
	mat options$(j)
	fen$='CBud.h[cno]'
	fncmbbud=fnComboA(fen$,1,1,mat option$,'Select from the list of budget files. To add a new budget file, take the Add option.',20,container)
fnend

