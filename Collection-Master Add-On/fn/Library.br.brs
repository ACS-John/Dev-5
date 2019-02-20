def library fnsession_size_setup(; &session_rows,&session_cols)
	library 'Collection-Master Add-On\fn\session_size_setup.br': fnsession_size_setup
	! library program$(1:pos(program$,'\',-1))&'session_size_setup.br': fnsession_size_setup
	fnsession_size_setup=fnsession_size_setup( session_rows,session_cols)
fnend
def library fnmulti_select(mat ms_selected$,mat ms_unselected$; cap$*80,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
	library 'Collection-Master Add-On\fn\multi_select.br': fnmulti_select
	! library program$(1:pos(program$,'\',-1))&'multi_select.br': fnmulti_select
	fnmulti_select=fnmulti_select(mat ms_selected$,mat ms_unselected$, cap$,mat ms_grid_heading$,mat ms_grid_width,mat ms_grid_form$,ms_rotation_default)
fnend
