00010 ! Replace S:\Core\KillAuto.br
00020 ! Kills automatic processing - turns it off (used for payroll and general ledger automatic processing)
00030 def library fnkillauto
00040   library 'S:\Core\Library': fnprocess
00060   fnprocess(-1)
00080   execute "Free "&env$('temp')&'\'&session$&"-Process-"&env$('cno')&".dat" ioerr ignore
00090 fnend 
