! Replace S:\acsPR\CmbSubCat.br
! creates a screen ace combobox for sub-category records
def library fncmbsubcat(myline,mypos; addall,container)
	library 'S:\Core\Library': fncombof
	if addall<>1 then addall=0
	if addall=0 then fen$="Subcat.h[cno]" else fen$="SubCatALL.h[cno]"
	fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\SCMStR.h[cno]",1,3,4,25,"[Q]\PRmstr\SCindEx.h[cno]",1+addall,0,"Select from the list of Sub-Categories.",container)
fnend 

