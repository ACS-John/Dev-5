92000   def library fnkey_change(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
92020     fnkey_change=fn_key_change(h_filehandle,f_fileform_key_only$,key_from$,key_to$)
92040   fnend 
94000   def fn_key_change(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
94020 ! account must be indexed by account key primarily, other stuff may follow
94040     dim tmp_key$*128
94060 !   debug=1
94080 ! if debug then    rwcnt=0
94100     f_fileform_key_only$=srep$(lwrc$(f_fileform_key_only$),'n ','g ')
94120     restore #h_filehandle,key>=rpad$(key_from$,kln(h_filehandle)): nokey KC_FINIS
94140     do 
94160       read #h_filehandle,using f_fileform_key_only$: tmp_key$ eof KC_FINIS
94180       if tmp_key$=key_from$ then 
94200         rewrite #h_filehandle,using f_fileform_key_only$: key_to$
94220 !       if debug then pr 'rewrite '&file$(h_filehandle)&' record '&str$(rec(h_filehandle)) : rwcnt+=1
94240       end if 
94260     loop until tmp_key$<>key_from$
94280 KC_FINIS: ! 
94300 ! if debug then pr file$(h_filehandle)&'    rwcnt=';rwcnt : pause
94320   fnend 
