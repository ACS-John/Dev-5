
autoLibrary
dim data$(0)*256
dim dataN(0)
h=fn_openFio('TM Client 420',mat data$,mat dataN)
do
	read #h,using form$(h): mat data$,mat dataN eof EoH
	data$(client_id)=trim$(data$(client_id))
	rewrite #h,using form$(h): mat data$,mat dataN
loop
EoH: !
Xit: end ! fnXit
include: fn_open
