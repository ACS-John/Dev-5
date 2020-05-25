! Replace S:\acsPR\CmbCategory.br
! creates a screen ace combobox for Category
def library fncmbcategory(myline,mypos; addall,container,indexfile$*200)
	autoLibrary
	dim df$*200,if$*200
 
	if addall<>1 then addall=0
	if addall=0 then
		fen$="Ccategory.h[cno]"
	else
		fen$="CcategoryALL.h[cno]"
	end if
	if indexfile$="" then
		if$="[Q]\PRmstr\categoryidx.h[cno]"
	else
		if$=indexfile$
	end if
	fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\category.h[cno]",1,5,6,30,if$,1+addall,0,"Select from the list of Category records. To add a Category record, take the Add option.",container)
	indexfile$=""
fnend
