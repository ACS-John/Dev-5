def library fnCmbBurden(myline,mypos; addall,container,indexfile$*200,___,if$*200)
	if addall<>1 then addall=0
	if addall=0 then fen$='Cburden.h[cno]' else fen$='CburdenALL.h[cno]'
	end if
	if indexfile$='' then 
		if$='[Q]\PRmstr\burdenidx.h[cno]' 
	else 
		if$=indexfile$
	end if
	fnCmbBurden=fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\burden.h[cno]',1,8,9,30,if$,1+addall,0,'Select from the list of personnel burden records. To add a personnel burden record, take the Add option.',container)
fnend

