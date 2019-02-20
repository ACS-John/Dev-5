def library fnsession_size_setup(; &session_rows,&session_cols)
	fnsession_size_setup=fn_sessionsize_setup( session_rows,session_cols)
fnend
def fn_sessionsize_setup(; &session_rows,&session_cols)
	if ~setup_session_size or session_rows=0 or session_Cols=0 then
		setup_session_size=1
		session_rows=max(24,val(env$('Session_Rows')))
		session_cols=max(80,val(env$('Session_Cols')))
		if session_rows=0 then session_rows=24 : setenv('Session_Rows',str$(session_rows))
		if session_cols=0 then session_cols=80 : setenv('Session_Cols',str$(session_cols))
	end if
fnend
