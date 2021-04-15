def library fnbuttonOrDisabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
	autoLibrary
	if enable then 
		fnButton(lyne,ps,txt$,comkey, tt$,1,width,container,tabcon,default,cancel)
	else 
		fnLbl(lyne,ps,txt$, width,2,font_mod,container,tabcon,'This "'&txt$&'" button is currently disabled.'&chr$(10)&tt$)
	end if 
fnend 
