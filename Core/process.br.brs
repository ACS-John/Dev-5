! Replace S:\Core\Process.br
def library fnprocess(; putpro)
	! fnprocess is an value returning function.  It returns a 1=on, 0=off 
	! used for payroll and general ledger automatic processing
	option retain 
	if putpro=-1 then process=oldpro=putpro=0
	if putpro>0 then process=oldpro=putpro else process=oldpro
	fnprocess=process
fnend 
!
def library fnkillauto
	! Kills automatic processing - turns it off (used for payroll and general ledger automatic processing)
	library 'S:\Core\Library': fnprocess
	fnprocess(-1)
	execute "Free "&env$('temp')&'\'&session$&"-Process-[cno].dat" ioerr ignore
fnend 
