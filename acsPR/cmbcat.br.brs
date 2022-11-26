! Replace S:\acsPR\CmbCat.br
! creates a screen ace combobox for category records
def library fnCmbCat(myline,mypos; addall,container,indexfile$*200, ___,if$*200)
		autoLibrary

		if addall<>1 then addall=0
		if addall=0 then fen$='Ccat.h[cno]' else fen$='CcatALL.h[cno]'
		if indexfile$='' then if$='[Q]\PRmstr\catindx.h[cno]' else if$=indexfile$
		fnCmbCat=fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\jccat.h[cno]',1,11,12,25,if$,1+addall,1,'Select from the list of categories. To add a category record, go to the Category File.',container)
fnend

