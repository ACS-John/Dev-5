def fn_setup
	setup=1
	autoLibrary
fnend 
def library fnbutton_or_disabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
	if ~setup then fn_setup
	fnbutton_or_disabled=fn_button_or_disabled(enable,lyne,ps,txt$,comkey, tt$,width,container,tabcon,default,cancel)
fnend 
def fn_button_or_disabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
	if enable then 
		fnButton(lyne,ps,txt$,comkey, tt$,1,width,container,tabcon,default,cancel)
	else 
		fnLbl(lyne,ps,txt$, width,2,font_mod,container,tabcon,'The "'&txt$&'" button is currently disabled.'&chr$(10)&tt$)
	end if 
fnend 
