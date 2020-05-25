! Replace S:\acsPR\SubCmbCat.br
! creates a screen ace combobox for sub-category records
def library fncmbsubcat(myline,mypos; addall,container,indexfile$*200)
	autoLibrary
	dim df$*200,if$*200
	if addall<>1 then addall=0
	if addall=0 then fen$="Csubcat.h[cno]" else fen$="CsubcatALL.h[cno]"
	if indexfile$="" then if$="[Q]\PRmstr\scindex.h[cno]" else if$=indexfile$
	fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\scmstr.h[cno]",1,3,4,25,if$,1+addall,1,"Select from the list of sub-categories. To add a sub-category record, go to the Sub-Category File.",container)
	indexfile$=""
fnend

