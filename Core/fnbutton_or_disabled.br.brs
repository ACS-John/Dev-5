20000   def fn_setup
20020     let setup=1
20040     library 'S:\Core\Library': fnbutton,fnlbl
20060   fnend 
20080   def library fnbutton_or_disabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
20100     if ~setup then let fn_setup
20120     let fnbutton_or_disabled=fn_button_or_disabled(enable,lyne,ps,txt$,comkey, tt$,width,container,tabcon,default,cancel)
20140   fnend 
20160   def fn_button_or_disabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
20180     if enable then 
20200       let fnbutton(lyne,ps,txt$,comkey, tt$,1,width,container,tabcon,default,cancel)
20220     else 
20240       let fnlbl(lyne,ps,txt$, width,2,font_mod,container,tabcon,'The "'&txt$&'" button is currently disabled.'&chr$(10)&tt$)
20260     end if 
20280   fnend 
