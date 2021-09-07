! Discover
! Finan Table
! 52 claims
! Purge the Finan Table for all
! Data\Finan
!  have ready by monday.
fn_setup
fnCopy('FINAN.INT//6','Finan(beforePrePurge[datetime].int//6')
open #hFinan:=fngethandle: "NAME=FINAN.INT//6,Shr",i,outi,r
do
	read #hFinan,using finan_formall$: mat finan_data$,mat finan_data eof EoFinan
	readCount+=1
	fncom(readCount,lrec(hFinan))
	if srch(mat fileno$,trim$(finan_data$(finan_fileno)))>0 then
		matchCount+=1
		delete #hFinan:
	end if
loop
EoFinan: !
pr 'readCount=';readCount
pr 'matchCount=';matchCount
close #hFinan:
execute 'subproc FinanIdx//8'
 
goto Xit
Xit: end
def fn_setup
	if ~setup then
		setup=1
 
		autoLibrary
 
		library "clsutil/library": fncom
		library "clsutil/library": fnplus,fnget_form,fnget_formarr
		library "clsutil/library": fnstime,fnstime$
		library "clsutil/library": fngethandle,fnget_var$
		library "clsutil/library": fnuser_init$,fnget_formall$
		library "clsutil/library": fnsql_int,fncustom,fnmessagebox,fnask_file1
		library "clsutil/library": fnedi_finis
		library "clsutil/library": fnadd_one$
		! library "intermnt/prog2": fnmasforw_data$
 
		dim finan_data$(1)*60,finan_data(1)
		dim finan_fieldsc$(1)*20,finan_fieldsn$(1)*20
		dim finan_formc$*2048,finan_formn$*2048
		fnget_form("finan",mat finan_data$,mat finan_data,mat finan_fieldsc$,mat finan_fieldsn$,finan_formc$,finan_formn$)
		dim finan_formall$*2048
		finan_formall$=fnget_formall$
		execute "*subproc "&fnget_var$("finan",mat finan_fieldsc$,mat finan_fieldsn$)
		dim finan_fc$(1,3)*80,finan_fn$(1,3)*80,finan_des_c$(1)*80,finan_des_n$(1)*80,finan_seq$(1)*80,finan_valid$(1)*80
		fnget_formarr("finan",mat finan_data$,mat finan_data,mat finan_fieldsc$,mat finan_fieldsn$,mat finan_fc$,mat finan_fn$,mat finan_des_c$,mat finan_des_n$,mat finan_seq$,mat finan_valid$)
 
 
		! r: get mat fileno$
			dim fileno$(0)*64
			mat fileno$(0)
			fnadd_one$(mat fileno$,'08-40042')
			fnadd_one$(mat fileno$,'05-08528')
			fnadd_one$(mat fileno$,'08-40079')
			!  ..  add more here ...
		! /r
 
	end if
fnend
 
