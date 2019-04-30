def library fnKeyChange(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
	fnKeyChange=fn_key_change(h_filehandle,f_fileform_key_only$,key_from$,key_to$)
fnend 
def fn_key_change(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
	! account must be indexed by account key primarily, other stuff may follow
	dim tmp_key$*128
	!   debug=1
	! if debug then    rwcnt=0
	f_fileform_key_only$=srep$(lwrc$(f_fileform_key_only$),'n ','g ')
	restore #h_filehandle,key>=rpad$(key_from$,kln(h_filehandle)): nokey KC_FINIS
	do 
		read #h_filehandle,using f_fileform_key_only$: tmp_key$ eof KC_FINIS
		if tmp_key$=key_from$ then 
			rewrite #h_filehandle,using f_fileform_key_only$: key_to$
	!       if debug then pr 'rewrite '&file$(h_filehandle)&' record '&str$(rec(h_filehandle)) : rwcnt+=1
		end if 
	loop until tmp_key$<>key_from$
	KC_FINIS: ! 
	! if debug then pr file$(h_filehandle)&'    rwcnt=';rwcnt : pause
fnend 
def library fnkeyDelete(h_filehandle,f_fileform_key_only$*128,key_from$*128)
	fnkeyDelete=fn_keyDelete(h_filehandle,f_fileform_key_only$,key_from$)
fnend 
def fn_keyDelete(h_filehandle,f_fileform_key_only$*128,key_from$*128)
	! account must be indexed by account key primarily, other stuff may follow
	dim tmp_key$*128
	!   debug=1
	! if debug then    rwcnt=0
	f_fileform_key_only$=srep$(lwrc$(f_fileform_key_only$),'n ','g ')
	restore #h_filehandle,key>=rpad$(key_from$,kln(h_filehandle)): nokey KD_FINIS
	do 
		read #h_filehandle,using f_fileform_key_only$: tmp_key$ eof KD_FINIS
		if tmp_key$=key_from$ then 
			delete #h_filehandle:
	!       if debug then pr 'delete '&file$(h_filehandle)&' record '&str$(rec(h_filehandle)) : rwcnt+=1
		end if 
	loop until tmp_key$<>key_from$
	KD_FINIS: ! 
	! if debug then pr file$(h_filehandle)&'    rwcnt=';rwcnt : pause
fnend 
