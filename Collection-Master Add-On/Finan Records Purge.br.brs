! Discover
! Finan Table
! 52 claims
! Purge the Finan Table for all 
! Data\Finan
!  have ready by monday.
fn_setup
		! OPEN #hFinan:=fngethandle: "NAME=FINAN.INT//6,KFNAME=FINAN.IDX//6,shr",INTERNAL,OUTIN,KEYED
exec 'config filenames mixed_case'
exec 'copy FINAN.INT//6 Finan(beforePrePurge'&date$('ccyymmdd')&srep$(time$,':','')&'.int//6'
open #hFinan:=fngethandle: "NAME=FINAN.INT//6,Shr",internal,outin,relative
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
			fnadd_one$(mat fileno$,'08-28638')
			fnadd_one$(mat fileno$,'05-16398')
			fnadd_one$(mat fileno$,'08-28686')
			fnadd_one$(mat fileno$,'03-09639')
			fnadd_one$(mat fileno$,'06-04979')
			fnadd_one$(mat fileno$,'08-40091')
			fnadd_one$(mat fileno$,'08-34517')
			fnadd_one$(mat fileno$,'08-28613')
			fnadd_one$(mat fileno$,'09-02486')
			fnadd_one$(mat fileno$,'07-12904')
			fnadd_one$(mat fileno$,'08-32359')
			fnadd_one$(mat fileno$,'09-01042')
			fnadd_one$(mat fileno$,'08-05288')
			fnadd_one$(mat fileno$,'08-05290')
			fnadd_one$(mat fileno$,'08-12983')
			fnadd_one$(mat fileno$,'08-15382')
			fnadd_one$(mat fileno$,'08-17135')
			fnadd_one$(mat fileno$,'08-21265')
			fnadd_one$(mat fileno$,'08-21305')
			fnadd_one$(mat fileno$,'09700016')
			fnadd_one$(mat fileno$,'09700023')
			fnadd_one$(mat fileno$,'09700073')
			fnadd_one$(mat fileno$,'09700075')
			fnadd_one$(mat fileno$,'09700095')
			fnadd_one$(mat fileno$,'09700218')
			fnadd_one$(mat fileno$,'09700232')
			fnadd_one$(mat fileno$,'09700393')
			fnadd_one$(mat fileno$,'09700494')
			fnadd_one$(mat fileno$,'09700516')
			fnadd_one$(mat fileno$,'09700854')
			fnadd_one$(mat fileno$,'09700880')
			fnadd_one$(mat fileno$,'09700887')
			fnadd_one$(mat fileno$,'09700956')
			fnadd_one$(mat fileno$,'09700971')
			fnadd_one$(mat fileno$,'09701099')
			fnadd_one$(mat fileno$,'09701225')
			fnadd_one$(mat fileno$,'09701256')
			fnadd_one$(mat fileno$,'09701363')
			fnadd_one$(mat fileno$,'09701369')
			fnadd_one$(mat fileno$,'09701390')
			fnadd_one$(mat fileno$,'09701489')
			fnadd_one$(mat fileno$,'09701617')
			fnadd_one$(mat fileno$,'09701709')
			fnadd_one$(mat fileno$,'09701712')
			fnadd_one$(mat fileno$,'10700666')
			fnadd_one$(mat fileno$,'10700736')
			fnadd_one$(mat fileno$,'10700948')
			fnadd_one$(mat fileno$,'09700787')
			fnadd_one$(mat fileno$,'14803963')
		! /r

	end if
fnend

